open Core

type t = Snark_params.Tock.Field.t * Snark_params.Tock.Field.t
[@@deriving sexp, eq, bin_io, compare, hash]

include Codable.S with type t := t

module Stable : sig
  module V1 : sig
    type t = Snark_params.Tock.Field.t * Snark_params.Tock.Field.t
    [@@deriving sexp, eq, bin_io, compare, hash, version]

    include Codable.S with type t := t
  end

  module Latest = V1
end

open Snark_params.Tick

type var = Inner_curve.Scalar.var * Inner_curve.Scalar.var

val to_base58_check : t -> string

val of_base58_check_exn : string -> t

val dummy : t
