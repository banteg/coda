(** An interface to limited libp2p functionality for Coda to use.

A subprocess is spawned to run the go-libp2p code. This module communicates
with that subprocess over an ad-hoc RPC protocol.

TODO: separate internal helper errors from underlying libp2p errors.

In general, functions in this module return ['a Deferred.Or_error.t]. Unless
otherwise mentioned, the deferred is resolved immediately once the RPC action
to the libp2p helper is finished. Unless otherwise mentioned, everything can
throw an exception due to an internal helper error. These indicate a bug in
this module/the helper, and not misuse.
*)

open Async
open Pipe_lib

(** Handle to all network functionality. *)
type net

(** Essentially a hash of a public key. *)
type peer_id

module Keypair : sig
  type t

  (** Securely generate a new keypair.

  [random net] can fail if generating secure random bytes fails. *)
  val random : net -> t Deferred.Or_error.t

  val to_string : t -> string

  val to_peerid : t -> peer_id
end

module Multiaddr : sig
  type t

  val to_string : t -> string

  val of_string : string -> t
end

module PeerID : sig
  type t = peer_id

  val to_string : t -> string

  val of_keypair : Keypair.t -> t
end

module Pubsub : sig
  (** A subscription to a pubsub topic. *)
  module Subscription : sig
    type t

    (** Publish a message to this pubsub topic.
    *
    * Returned deferred is resolved once the publish is enqueued locally.
    * This function continues to work even if [unsubscribe t] has been called.
    * It is exactly [Pubsub.publish] with the topic this subscription was
    * created for, and fails in the same way. *)
    val publish : t -> string -> unit Deferred.t

    (** Unsubscribe from this topic, closing the write pipe.
    *
    * Returned deferred is resolved once the unsubscription is complete.
    * This can fail if already unsubscribed. *)
    val unsubscribe : t -> unit Deferred.Or_error.t
  end

  (** Publish a message to a topic.
  *
  * Returned deferred is resolved once the publish is enqueued.
  * This can fail if signing the message failed.
  *  *)
  val publish : net -> topic:string -> data:string -> unit Deferred.t

  (** Subscribe to a pubsub topic.
    *
    * Fails if already subscribed. If it succeeds, incoming messages for that
    * topic will be written to the pipe. Returned deferred is resolved with [Ok
    * sub] as soon as the subscription is enqueued.
    *)
  val subscribe :
       net
    -> string
    -> ( string Envelope.Incoming.t
       , _ Strict_pipe.buffered
       , unit )
       Strict_pipe.Writer.t
    -> Subscription.t Deferred.Or_error.t

  (** [register_validator net topic f] validates messages on [topic] with [f] before forwarding them.
    *
    * [f] will be called once per new message, and will not be called again until
    * the deferred it returns is resolved. The helper process waits 5 seconds for
    * the result of [f] to be reported, otherwise it considers the message invalid.
    *
    * This can fail if a validator is already registered for [topic].
  *)
  val register_validator :
       net
    -> string
    -> f:(peerid:string -> data:string -> bool Deferred.t)
    -> unit Deferred.Or_error.t
end

(** [create ~logger ~conf_dir] starts a new [net] storing its state in [conf_dir]
  *
  * The new [net] isn't connected to any network until [configure] is called.
  *
  * This can fail for a variety of reasons related to spawning the subprocess.
*)
val create : logger:Logger.t -> conf_dir:string -> net Deferred.Or_error.t

(** Configure the network connection.
  *
  * Listens on each address in [maddrs].
  *
  * This will only connect to peers that share the same [network_id].
  *
  * This fails if initializing libp2p fails for any reason.
*)
val configure :
     net
  -> me:Keypair.t
  -> maddrs:Multiaddr.t list
  -> network_id:string
  -> unit Deferred.Or_error.t

(** List of all peers we know about. *)
val peers : net -> PeerID.t list Deferred.t

(** Randomly pick a few peers from all the ones we know about. *)
val random_peers : net -> int -> PeerID.t list Deferred.t

(** An open stream.

  Close the write pipe when you are done. This won't close the reading end.
  The reading end will be closed when the remote peer closes their writing
  end. Once both write ends are closed, the stream ends.

  IMPORTANT NOTE: A single write to the stream will not necessarily result
  in a single read on the other side. libp2p may fragment messages arbitrarily.
 *)
module Stream : sig
  type t

  (** [pipes t] returns the same pipes that [open_stream] passes to the handler. *)
  val pipes : t -> string Pipe.Reader.t * string Pipe.Writer.t

  (** [reset t] informs the other process to close the stream.

    The returned [Deferred.Or_error.t] is fulfilled with [Ok ()] immediately
    once the reset is performed. It does not wait for the other host to
    acknowledge.
    *)
  val reset : t -> unit Deferred.Or_error.t

  (** TODO: remote addr + peerid *)
end

(** [Protocol_handler.t] is the rough equivalent to [Tcp.Server.t].

  This lets one stop handling a protocol.
 *)
module Protocol_handler : sig
  type t

  (** Returns the protocol string being handled. *)
  val handling_protocol : t -> string

  (** Whether [close t] has been called. *)
  val is_closed : t -> bool

  (** Stop handling new streams on this protocol.

  [reset_existing_streams] controls whether open streams for this protocol
  will be reset, and defaults to [false].
  *)
  val close : ?reset_existing_streams:bool -> t -> unit Deferred.t
end

(** Opens a stream with a peer on a particular protocol.

  Close the write pipe when you are done. This won't close the reading end.
  The reading end will be closed when the remote peer closes their writing
  end. Once both write ends are closed, the connection terminates.

  This can fail if the peer isn't reachable, doesn't implement the requrested
  protocol, and probably for other reasons.
 *)
val open_stream :
  net -> protocol:string -> PeerID.t -> Stream.t Deferred.Or_error.t

(** Handle incoming streams for a protocol.

  [on_handler_error] determines what happens if the handler throws an
  exception. If an exception is raised by [on_handler_error] (either explicitly
  via [`Raise], or in the function passed via [`Call]), [Protocol_handler.close] will
  be called.

  The function in `Call will be passed the stream that faulted.
*)
val handle_protocol :
     net
  -> on_handler_error:[`Raise | `Ignore | `Call of Stream.t -> exn -> unit]
  -> protocol:string
  -> (Stream.t -> unit Deferred.t)
  -> Protocol_handler.t Deferred.Or_error.t

(** Try listening on a multiaddr.
*
* If successful, returns the list of all addresses this net is listening on.
* For example, if listening on ["/ip4/127.0.0.1/tcp/0"], it might return
* ["/ip4/127.0.0.1/tcp/35647"] after the OS selects an available listening
* port.
*)
val listen_on : net -> Multiaddr.t -> Multiaddr.t list Deferred.Or_error.t

(** The list of addresses this net is listening on.

  This returns the same thing that [listen_on] does, without listening
  on an address.
*)
val listening_addrs : net -> Multiaddr.t list Deferred.Or_error.t

(** Stop listening, close all connections and subscription pipes, and kill the subprocess. *)
val shutdown : net -> unit Deferred.t
