open Async_kernel
open Core_kernel
open Coda_base
open Pipe_lib

(** A [Resource_pool_base_intf] is a mutable pool of resources that supports
 *  mutation via some [Resource_pool_diff_intf]. A [Resource_pool_base_intf]
 *  can only be initialized, and any interaction with it must go through
 *  its [Resource_pool_diff_intf] *)
module type Resource_pool_base_intf = sig
  type t [@@deriving sexp_of]

  type transition_frontier

  val create :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> frontier_broadcast_pipe:transition_frontier Option.t
                               Broadcast_pipe.Reader.t
    -> t
end

(** A [Resource_pool_diff_intf] is a representation of a mutation to
 *  perform on a [Resource_pool_base_intf]. It includes the logic for
 *  processing this mutation and applying it to an underlying
 *  [Resource_pool_base_intf]. *)
module type Resource_pool_diff_intf = sig
  type pool

  type t [@@deriving sexp]

  val summary : t -> string

  val apply : pool -> t Envelope.Incoming.t -> t Deferred.Or_error.t
end

(** A [Resource_pool_intf] ties together an associated pair of
 *  [Resource_pool_base_intf] and [Resource_pool_diff_intf]. *)
module type Resource_pool_intf = sig
  include Resource_pool_base_intf

  module Diff : Resource_pool_diff_intf with type pool := t
end

(** A [Network_pool_base_intf] is the core implementation of a
 *  network pool on top of a [Resource_pool_intf]. It wraps
 *  some [Resource_pool_intf] and provides a generic interface
 *  for interacting with the [Resource_pool_intf] using the
 *  network. A [Network_pool_base_intf] wires the [Resource_pool_intf]
 *  into the network using pipes of diffs and transition frontiers.
 *  It also provides a way to apply new diffs and rebroadcast them
 *  to the network if necessary. *)
module type Network_pool_base_intf = sig
  type t

  type resource_pool

  type resource_pool_diff

  type transition_frontier

  val create :
       logger:Logger.t
    -> trust_system:Trust_system.t
    -> incoming_diffs:resource_pool_diff Envelope.Incoming.t
                      Linear_pipe.Reader.t
    -> frontier_broadcast_pipe:transition_frontier Option.t
                               Broadcast_pipe.Reader.t
    -> t

  val of_resource_pool_and_diffs :
       resource_pool
    -> logger:Logger.t
    -> incoming_diffs:resource_pool_diff Envelope.Incoming.t
                      Linear_pipe.Reader.t
    -> t

  val resource_pool : t -> resource_pool

  val broadcasts : t -> resource_pool_diff Linear_pipe.Reader.t

  val apply_and_broadcast :
    t -> resource_pool_diff Envelope.Incoming.t -> unit Deferred.t
end

(** A [Snark_resource_pool_intf] is a superset of a
 *  [Resource_pool_intf] specifically for handling snarks. *)
module type Snark_resource_pool_intf = sig
  type ledger_proof

  type work

  type transition_frontier

  include
    Resource_pool_base_intf
    with type transition_frontier := transition_frontier

  val bin_writer_t : t Bin_prot.Writer.t

  val add_snark :
       t
    -> work:work
    -> proof:ledger_proof list
    -> fee:Fee_with_prover.t
    -> [`Rebroadcast | `Don't_rebroadcast]

  val request_proof : t -> work -> ledger_proof list Priced_proof.t option
end

(** A [Snark_pool_diff_intf] is the resource pool diff for
 *  a [Snark_resource_pool_intf]. *)
module type Snark_pool_diff_intf = sig
  type ledger_proof

  type work

  type resource_pool

  module Stable : sig
    module V1 : sig
      type t =
        | Add_solved_work of work * ledger_proof list Priced_proof.Stable.V1.t
      [@@deriving sexp, yojson, bin_io, version]
    end

    module Latest = V1
  end

  type t = Stable.Latest.t [@@deriving sexp, yojson]

  val summary : t -> string

  val apply : resource_pool -> t Envelope.Incoming.t -> t Deferred.Or_error.t
end
