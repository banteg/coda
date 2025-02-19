open Core_kernel

(** Sync_status represent states interacting with peers in the coda protocol.
    When the protocol is starting, the node should be in the CONNECT state
    trying to connect to a peer. Once it connects to a peer, the node should be
    in the LISTENING state waiting for peers to send a message to them. When
    the node receives a constant flow of messages, its state should be SYNCED.
    However, when the node is bootstrapping, its state is BOOTSTRAPPING. If it
    hasn’t received messages for some time
    (Consensus.Constants.inactivity_secs), then it is OFFLINE. *)
let to_string = function
  | `Connecting ->
      "Connecting"
  | `Listening ->
      "Listening"
  | `Offline ->
      "Offline"
  | `Bootstrap ->
      "Bootstrap"
  | `Synced ->
      "Synced"

let of_string string =
  match String.lowercase string with
  | "connecting" ->
      Ok `Connecting
  | "listening" ->
      Ok `Listening
  | "offline" ->
      Ok `Offline
  | "bootstrap" ->
      Ok `Bootstrap
  | "synced" ->
      Ok `Synced
  | status ->
      Error (Error.createf !"%s is not a valid status" status)

let to_yojson status = `String (to_string status)

module Stable = struct
  module V1 = struct
    module T = struct
      type t = [`Connecting | `Listening | `Offline | `Bootstrap | `Synced]
      [@@deriving bin_io, version, sexp, hash, compare, equal]

      let to_yojson = to_yojson
    end

    include T
    include Hashable.Make (T)
  end

  module Latest = V1
end

type t = [`Connecting | `Listening | `Offline | `Bootstrap | `Synced]
[@@deriving sexp, hash, equal]

include Hashable.Make (Stable.Latest.T)

let%test "of_string (to_string x) == x" =
  List.for_all [`Offline; `Bootstrap; `Synced; `Connecting; `Listening]
    ~f:(fun sync_status ->
      equal sync_status (of_string (to_string sync_status) |> Or_error.ok_exn)
  )
