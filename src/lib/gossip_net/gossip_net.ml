[%%import
"../../config.mlh"]

open Core
open Async
open Pipe_lib
open Network_peer
open Kademlia
open O1trace
module Membership = Membership.Haskell

type ('q, 'r) dispatch =
  Versioned_rpc.Connection_with_menu.t -> 'q -> 'r Deferred.Or_error.t

module type Message_intf = sig
  type msg [@@deriving to_yojson]

  include
    Versioned_rpc.Both_convert.One_way.S
    with type callee_msg := msg
     and type caller_msg := msg

  val summary : msg -> string
end

module type Config_intf = sig
  type t =
    { timeout: Time.Span.t
    ; target_peer_count: int
    ; initial_peers: Host_and_port.t list
    ; addrs_and_ports: Kademlia.Node_addrs_and_ports.t
    ; conf_dir: string
    ; logger: Logger.t
    ; trust_system: Trust_system.t
    ; max_concurrent_connections: int option }
  [@@deriving make]
end

module type S = sig
  type msg

  module Connection_with_state : sig
    type t = Banned | Allowed of Rpc.Connection.t Ivar.t
  end

  type t =
    { timeout: Time.Span.t
    ; logger: Logger.t
    ; trust_system: Trust_system.t
    ; target_peer_count: int
    ; broadcast_writer: msg Linear_pipe.Writer.t
    ; received_reader: msg Envelope.Incoming.t Strict_pipe.Reader.t
    ; addrs_and_ports: Kademlia.Node_addrs_and_ports.t
    ; initial_peers: Host_and_port.t list
    ; peers: Peer.Hash_set.t
    ; peers_by_ip: (Unix.Inet_addr.t, Peer.t list) Hashtbl.t
    ; connections:
        ( Unix.Inet_addr.t
        , (Uuid.t, Connection_with_state.t) Hashtbl.t )
        Hashtbl.t
    ; first_connect: unit Ivar.t
    ; max_concurrent_connections: int option }

  module Config : Config_intf

  val create :
    Config.t -> Host_and_port.t Rpc.Implementation.t list -> t Deferred.t

  val received : t -> msg Envelope.Incoming.t Strict_pipe.Reader.t

  val broadcast : t -> msg Linear_pipe.Writer.t

  val broadcast_all :
    t -> msg -> (unit -> [`Done | `Continue] Deferred.t) Staged.t

  val random_peers : t -> int -> Peer.t list

  val random_peers_except : t -> int -> except:Peer.Hash_set.t -> Peer.t list

  val peers : t -> Peer.t list

  val initial_peers : t -> Host_and_port.t list

  val query_peer :
    t -> Peer.t -> ('q, 'r) dispatch -> 'q -> 'r Or_error.t Deferred.t

  val query_random_peers :
    t -> int -> ('q, 'r) dispatch -> 'q -> 'r Or_error.t Deferred.t List.t
end

module Make (Message : Message_intf) : S with type msg := Message.msg = struct
  module Connection_with_state = struct
    type t = Banned | Allowed of Rpc.Connection.t Ivar.t

    let value_map ~when_allowed ~when_banned t =
      match t with Allowed c -> when_allowed c | _ -> when_banned
  end

  type t =
    { timeout: Time.Span.t
    ; logger: Logger.t
    ; trust_system: Trust_system.t
    ; target_peer_count: int
    ; broadcast_writer: Message.msg Linear_pipe.Writer.t
    ; received_reader: Message.msg Envelope.Incoming.t Strict_pipe.Reader.t
    ; addrs_and_ports: Kademlia.Node_addrs_and_ports.t
    ; initial_peers: Host_and_port.t list
    ; peers: Peer.Hash_set.t
    ; peers_by_ip: (Unix.Inet_addr.t, Peer.t list) Hashtbl.t
    ; connections:
        ( Unix.Inet_addr.t
        , (Uuid.t, Connection_with_state.t) Hashtbl.t )
        Hashtbl.t
          (**mapping a Uuid to a connection to be able to remove it from the hash
         *table since Rpc.Connection.t doesn't have the socket information*)
    ; first_connect: unit Ivar.t
    ; max_concurrent_connections: int option
          (* maximum number of concurrent connections from an ip (infinite if None)*)
    }

  module Config = struct
    type t =
      { timeout: Time.Span.t
      ; target_peer_count: int
      ; initial_peers: Host_and_port.t list
      ; addrs_and_ports: Kademlia.Node_addrs_and_ports.t
      ; conf_dir: string
      ; logger: Logger.t
      ; trust_system: Trust_system.t
      ; max_concurrent_connections: int option }
    [@@deriving make]
  end

  (* OPTIMIZATION: use fast n choose k implementation - see python or old flow code *)
  let random_sublist xs n = List.take (List.permute xs) n

  let create_connection_with_menu peer r w =
    match%bind Rpc.Connection.create r w ~connection_state:(fun _ -> peer) with
    | Error exn ->
        return (Or_error.of_exn exn)
    | Ok conn ->
        Versioned_rpc.Connection_with_menu.create conn

  (* remove peer from set of peers and peers_by_ip

     there are issues with this simple approach, because
     Kademlia is not informed when peers are removed, so:

     - the node may not be informed when a peer reconnects, so the
        peer won't be re-added to the peer set
     - Kademlia may propagate information about the removed peers
        other nodes
  *)
  let remove_peer t peer =
    Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
      !"Removing peer from peer set: %s"
      (Peer.to_string peer)
      ~metadata:[("peer", Peer.to_yojson peer)] ;
    Coda_metrics.(Gauge.dec_one Network.peers) ;
    Hash_set.remove t.peers peer ;
    Hashtbl.update t.peers_by_ip peer.host ~f:(function
      | None ->
          failwith "Peer to remove doesn't appear in peers_by_ip"
      | Some ip_peers ->
          List.filter ip_peers ~f:(fun ip_peer -> not (Peer.equal ip_peer peer)) )

  let is_unix_errno errno unix_errno =
    Int.equal (Unix.Error.compare errno unix_errno) 0

  (* Create a socket bound to the correct IP, for outgoing connections. *)
  let get_socket t =
    let socket = Async.Socket.(create Type.tcp) in
    (* Binding with a source port of 0 tells the kernel to pick a free one for
       us. Because we're binding an address and port before the kernel knows
       whether this will be a listen socket or an outgoing socket, it won't
       allocate the same source port twice, even when the destination IP will be
       different. So we could in very extreme cases run out of ephemeral ports.
       The IP_BIND_ADDRESS_NO_PORT socket option informs the kernel we're
       binding the socket to an IP and intending to make an outgoing connection,
       but it only works on Linux. I think we don't need to worry about it,
       there are a lot of ephemeral ports and we don't make very many
       simultaneous connections.
    *)
    Async.Socket.bind_inet socket @@ `Inet (t.addrs_and_ports.bind_ip, 0)

  let try_call_rpc t (peer : Peer.t) dispatch query =
    (* use error collection, close connection strategy of Tcp.with_connection *)
    let collect_errors writer f =
      let monitor = Writer.monitor writer in
      ignore (Monitor.detach_and_get_error_stream monitor) ;
      choose
        [ choice (Monitor.get_next_error monitor) (fun e -> Error e)
        ; choice (try_with ~name:"Tcp.collect_errors" f) Fn.id ]
    in
    let close_connection reader writer =
      Writer.close writer ~force_close:(Clock.after (sec 30.))
      >>= fun () -> Reader.close reader
    in
    let call () =
      try_with (fun () ->
          let socket = get_socket t in
          let peer_addr = `Inet (peer.host, peer.communication_port) in
          let%bind connected_socket = Async.Socket.connect socket peer_addr in
          let reader, writer =
            let fd = Socket.fd connected_socket in
            (Reader.create fd, Writer.create fd)
          in
          let run_query () =
            create_connection_with_menu peer reader writer
            >>=? fun conn -> dispatch conn query
          in
          let result = collect_errors writer run_query in
          Deferred.any
            [ (result >>| fun (_ : ('a, exn) Result.t) -> ())
            ; Reader.close_finished reader
            ; Writer.close_finished writer ]
          >>= fun () ->
          close_connection reader writer
          >>= fun () -> result >>| function Ok v -> v | Error e -> raise e )
      >>= function
      | Ok (Ok result) ->
          (* call succeeded, result is valid *)
          return (Ok result)
      | Ok (Error err) -> (
          (* call succeeded, result is an error *)
          Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
            "RPC call error: $error, same error in machine format: \
             $machine_error"
            ~metadata:
              [ ("error", `String (Error.to_string_hum err))
              ; ("machine_error", `String (Error.to_string_mach err)) ] ;
          match (Error.to_exn err, Error.sexp_of_t err) with
          | ( _
            , Sexp.List
                [ Sexp.Atom "src/connection.ml.Handshake_error.Handshake_error"
                ; _ ] ) ->
              let%map () =
                Trust_system.(
                  record t.trust_system t.logger peer.host
                    Actions.
                      (Outgoing_connection_error, Some ("handshake error", [])))
              in
              remove_peer t peer ; Error err
          | ( _
            , Sexp.List
                [ Sexp.List
                    [ Sexp.Atom "rpc_error"
                    ; Sexp.List [Sexp.Atom "Connection_closed"; _] ]
                ; _connection_description
                ; _rpc_tag
                ; _rpc_version ] ) ->
              let%map () =
                Trust_system.(
                  record t.trust_system t.logger peer.host
                    Actions.
                      ( Outgoing_connection_error
                      , Some ("Closed connection", []) ))
              in
              remove_peer t peer ; Error err
          | _ ->
              let%map () =
                Trust_system.(
                  record t.trust_system t.logger peer.host
                    Actions.
                      ( Violated_protocol
                      , Some
                          ( "RPC call failed, reason: $exn"
                          , [("exn", `String (Error.to_string_hum err))] ) ))
              in
              remove_peer t peer ; Error err )
      | Error monitor_exn -> (
          (* call itself failed *)
          (* TODO: learn what other exceptions are raised here *)
          let exn = Monitor.extract_exn monitor_exn in
          match exn with
          | Unix.Unix_error (errno, _, _)
            when is_unix_errno errno Unix.ECONNREFUSED ->
              let%map () =
                Trust_system.(
                  record t.trust_system t.logger peer.host
                    Actions.
                      ( Outgoing_connection_error
                      , Some ("Connection refused", []) ))
              in
              remove_peer t peer ; Or_error.of_exn exn
          | _ ->
              Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
                "RPC call raised an exception: $exn"
                ~metadata:[("exn", `String (Exn.to_string exn))] ;
              return (Or_error.of_exn exn) )
    in
    match Hashtbl.find t.connections peer.host with
    | None ->
        call ()
    | Some conn_map ->
        if
          Option.is_some t.max_concurrent_connections
          && Hashtbl.length conn_map
             >= Option.value_exn t.max_concurrent_connections
        then
          Deferred.return
            (Or_error.errorf
               !"Not connecting to peer %s. Number of open connections to the \
                 peer equals the limit %d.\n"
               (Peer.to_string peer)
               (Option.value_exn t.max_concurrent_connections))
        else call ()

  let broadcast_selected t peers msg =
    let send peer =
      try_call_rpc t peer
        (fun conn m -> return (Message.dispatch_multi conn m))
        msg
    in
    trace_event "broadcasting message" ;
    Deferred.List.iter ~how:`Parallel peers ~f:(fun peer ->
        match%map send peer with
        | Ok () ->
            ()
        | Error e ->
            Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
              "Broadcasting message $message_summary to $peer failed: $error"
              ~metadata:
                [ ("error", `String (Error.to_string_hum e))
                ; ("message_summary", `String (Message.summary msg))
                ; ("message", Message.msg_to_yojson msg)
                ; ("peer", Peer.to_yojson peer) ] )

  let broadcast_random t n msg =
    let selected_peers = random_sublist (Hash_set.to_list t.peers) n in
    broadcast_selected t selected_peers msg

  let create (config : Config.t)
      (implementation_list : Host_and_port.t Rpc.Implementation.t list) =
    trace_task "gossip net" (fun () ->
        let%bind membership =
          match%map
            trace_task "membership" (fun () ->
                Membership.connect ~initial_peers:config.initial_peers
                  ~node_addrs_and_ports:config.addrs_and_ports
                  ~conf_dir:config.conf_dir ~logger:config.logger
                  ~trust_system:config.trust_system )
          with
          | Ok membership ->
              membership
          | Error e ->
              failwith
                (Printf.sprintf "Failed to connect to kademlia process: %s\n"
                   (Error.to_string_hum e))
        in
        let peer_events = Membership.changes membership in
        let first_connect = Ivar.create () in
        let broadcast_reader, broadcast_writer = Linear_pipe.create () in
        let received_reader, received_writer =
          Strict_pipe.create ~name:"received gossip messages"
            (Buffered (`Capacity 64, `Overflow Crash))
        in
        let t =
          { timeout= config.timeout
          ; logger= config.logger
          ; trust_system= config.trust_system
          ; target_peer_count= config.target_peer_count
          ; broadcast_writer
          ; received_reader
          ; addrs_and_ports= config.addrs_and_ports
          ; peers= Peer.Hash_set.create ()
          ; initial_peers= config.initial_peers
          ; peers_by_ip= Hashtbl.create (module Unix.Inet_addr)
          ; connections= Hashtbl.create (module Unix.Inet_addr)
          ; max_concurrent_connections= config.max_concurrent_connections
          ; first_connect }
        in
        don't_wait_for
          (Strict_pipe.Reader.iter (Trust_system.ban_pipe config.trust_system)
             ~f:(fun addr ->
               match Hashtbl.find t.connections addr with
               | None ->
                   Deferred.unit
               | Some conn_tbl ->
                   Logger.debug t.logger ~module_:__MODULE__ ~location:__LOC__
                     !"Peer %s banned, disconnecting."
                     (Unix.Inet_addr.to_string addr) ;
                   let%map () =
                     Deferred.List.iter (Hashtbl.to_alist conn_tbl)
                       ~f:(fun (_, conn_state) ->
                         Connection_with_state.value_map conn_state
                           ~when_allowed:(fun conn_ivar ->
                             let%bind conn = Ivar.read conn_ivar in
                             Rpc.Connection.close conn )
                           ~when_banned:Deferred.unit )
                   in
                   Hashtbl.map_inplace conn_tbl ~f:(fun conn_state ->
                       Connection_with_state.value_map conn_state
                         ~when_allowed:(fun _ -> Connection_with_state.Banned)
                         ~when_banned:Banned ) )) ;
        trace_task "rebroadcasting messages" (fun () ->
            don't_wait_for
              (Linear_pipe.iter_unordered ~max_concurrency:64 broadcast_reader
                 ~f:(fun m ->
                   Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
                     "broadcasting message" ;
                   broadcast_random t t.target_peer_count m )) ) ;
        let implementations =
          let implementations =
            Versioned_rpc.Menu.add
              ( Message.implement_multi
                  (fun client_host_and_port ~version:_ msg ->
                    (* wrap received message in envelope *)
                    Coda_metrics.(
                      Counter.inc_one Network.gossip_messages_received) ;
                    let sender =
                      Envelope.Sender.Remote
                        (Unix.Inet_addr.of_string
                           client_host_and_port.Host_and_port.host)
                    in
                    Strict_pipe.Writer.write received_writer
                      (Envelope.Incoming.wrap ~data:msg ~sender) )
              @ implementation_list )
          in
          let handle_unknown_rpc conn ~rpc_tag ~version =
            let inet_addr = Unix.Inet_addr.of_string conn.Host_and_port.host in
            Deferred.don't_wait_for
              Trust_system.(
                record t.trust_system t.logger inet_addr
                  Actions.
                    ( Violated_protocol
                    , Some
                        ( "Attempt to make unknown (fixed-version) RPC call \
                           \"$rpc\" with version $version"
                        , [("rpc", `String rpc_tag); ("version", `Int version)]
                        ) )) ;
            `Close_connection
          in
          Rpc.Implementations.create_exn ~implementations
            ~on_unknown_rpc:(`Call handle_unknown_rpc)
        in
        trace_task "peer events" (fun () ->
            Linear_pipe.iter_unordered ~max_concurrency:64 peer_events
              ~f:(function
              | Connect peers ->
                  Ivar.fill_if_empty first_connect () ;
                  Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
                    !"Connected to some peers [%s]"
                    (Peer.pretty_list peers) ;
                  List.iter peers ~f:(fun peer ->
                      Coda_metrics.(Gauge.inc_one Network.peers) ;
                      Hash_set.add t.peers peer ;
                      Hashtbl.add_multi t.peers_by_ip ~key:peer.host ~data:peer
                  ) ;
                  Deferred.unit
              | Disconnect peers ->
                  Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
                    "Some peers disconnected %s" (Peer.pretty_list peers) ;
                  List.iter peers ~f:(remove_peer t) ;
                  Deferred.unit )
            |> ignore ) ;
        let%map _ =
          Tcp.Server.create
            ~on_handler_error:
              (`Call
                (fun addr exn ->
                  Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
                    "Exception raised in gossip net TCP server handler when \
                     connected to address $address: $exn"
                    ~metadata:
                      [ ("exn", `String (Exn.to_string_mach exn))
                      ; ("address", `String (Socket.Address.to_string addr)) ] ;
                  raise exn ))
            Tcp.(
              Where_to_listen.bind_to
                (Bind_to_address.Address t.addrs_and_ports.bind_ip)
                (Bind_to_port.On_port t.addrs_and_ports.communication_port))
            (fun client reader writer ->
              let client_inet_addr = Socket.Address.Inet.addr client in
              let%bind () =
                Trust_system.(
                  record t.trust_system t.logger client_inet_addr
                    Actions.(Connected, None))
              in
              let conn_map =
                Option.value_map
                  ~default:(Hashtbl.create (module Uuid))
                  (Hashtbl.find t.connections client_inet_addr)
                  ~f:Fn.id
              in
              let is_client_banned =
                let peer_status =
                  Trust_system.Peer_trust.lookup t.trust_system
                    client_inet_addr
                in
                match peer_status.banned with
                | Banned_until _ ->
                    true
                | Unbanned ->
                    false
              in
              if is_client_banned then (
                Logger.info t.logger ~module_:__MODULE__ ~location:__LOC__
                  "Rejecting connection from banned peer %s"
                  (Socket.Address.Inet.to_string client) ;
                Reader.close reader >>= fun _ -> Writer.close writer )
              else if
                Option.is_some t.max_concurrent_connections
                && Hashtbl.length conn_map
                   >= Option.value_exn t.max_concurrent_connections
              then (
                Logger.error t.logger ~module_:__MODULE__ ~location:__LOC__
                  "Gossip net TCP server cannot open another connection. \
                   Number of open connections from client $client equals the \
                   limit $max_connections"
                  ~metadata:
                    [ ("client", `String (Socket.Address.Inet.to_string client))
                    ; ( "max_connections"
                      , `Int (Option.value_exn t.max_concurrent_connections) )
                    ] ;
                Reader.close reader >>= fun _ -> Writer.close writer )
              else
                let conn_id = Uuid_unix.create () in
                Hashtbl.add_exn conn_map ~key:conn_id
                  ~data:(Allowed (Ivar.create ())) ;
                Hashtbl.set t.connections ~key:client_inet_addr ~data:conn_map ;
                let%map () =
                  Rpc.Connection.server_with_close reader writer
                    ~implementations
                    ~connection_state:(fun conn ->
                      (* connection state is the client's IP and ephemeral port
                        when connecting to the server over TCP; the ephemeral
                        port is distinct from the client's discovery and
                        communication ports *)
                      Connection_with_state.value_map
                        (Hashtbl.find_exn conn_map conn_id)
                        ~when_allowed:(fun ivar -> Ivar.fill ivar conn)
                        ~when_banned:() ;
                      Hashtbl.set t.connections
                        ~key:(Socket.Address.Inet.addr client)
                        ~data:conn_map ;
                      Socket.Address.Inet.to_host_and_port client )
                    ~on_handshake_error:
                      (`Call
                        (fun exn ->
                          Trust_system.(
                            record t.trust_system t.logger client_inet_addr
                              Actions.
                                ( Incoming_connection_error
                                , Some
                                    ( "Handshake error: $exn"
                                    , [("exn", `String (Exn.to_string exn))] )
                                )) ))
                in
                let conn_map =
                  Hashtbl.find_exn t.connections client_inet_addr
                in
                Hashtbl.remove conn_map conn_id ;
                if Hashtbl.is_empty conn_map then
                  Hashtbl.remove t.connections client_inet_addr
                else
                  Hashtbl.set t.connections ~key:client_inet_addr
                    ~data:conn_map )
        in
        t )

  let received t = t.received_reader

  let broadcast t = t.broadcast_writer

  let peers t = Hash_set.to_list t.peers

  let initial_peers t = t.initial_peers

  let broadcast_all t msg =
    let to_broadcast = ref (List.permute (Hash_set.to_list t.peers)) in
    stage (fun () ->
        let selected = List.take !to_broadcast t.target_peer_count in
        to_broadcast := List.drop !to_broadcast t.target_peer_count ;
        let%map () = broadcast_selected t selected msg in
        if List.length !to_broadcast = 0 then `Done else `Continue )

  let random_peers t n = random_sublist (Hash_set.to_list t.peers) n

  let random_peers_except t n ~(except : Peer.Hash_set.t) =
    let new_peers = Hash_set.(diff t.peers except |> to_list) in
    random_sublist new_peers n

  let query_peer t (peer : Peer.t) rpc query =
    Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
      !"Querying peer %s" (Peer.to_string peer) ;
    try_call_rpc t peer rpc query

  let query_random_peers t n rpc query =
    let peers = random_peers t n in
    Logger.trace t.logger ~module_:__MODULE__ ~location:__LOC__
      !"Querying random peers: %s"
      (Peer.pretty_list peers) ;
    List.map peers ~f:(fun peer -> query_peer t peer rpc query)
end
