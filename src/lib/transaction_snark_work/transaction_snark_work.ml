open Core_kernel
open Async_kernel
open Module_version
open Currency
open Signature_lib

module Make (Ledger_proof : sig
  type t [@@deriving sexp, bin_io]
end) :
  Coda_intf.Transaction_snark_work_intf
  with type ledger_proof := Ledger_proof.t = struct
  let proofs_length = 2

  module Statement = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = Transaction_snark.Statement.Stable.V1.t list
          [@@deriving bin_io, sexp, hash, compare, yojson, version]
        end

        include T
        include Registration.Make_latest_version (T)
        include Hashable.Make_binable (T)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "transaction_snark_work_statement"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    (* bin_io omitted *)
    type t = Stable.Latest.t [@@deriving sexp, hash, compare, yojson]

    include Hashable.Make (Stable.Latest)

    let gen =
      Quickcheck.Generator.list_with_length proofs_length
        Transaction_snark.Statement.gen
  end

  module T = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t =
            { fee: Fee.Stable.V1.t
            ; proofs: Ledger_proof.t list
            ; prover: Public_key.Compressed.Stable.V1.t }
          [@@deriving sexp, bin_io, version {asserted}]
        end

        include T
        include Registration.Make_latest_version (T)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "transaction_snark_work"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    (* bin_io omitted *)
    type t = Stable.Latest.t =
      {fee: Fee.t; proofs: Ledger_proof.t list; prover: Public_key.Compressed.t}
    [@@deriving sexp]
  end

  include T

  type unchecked = t

  module Checked = struct
    include T

    let create_unsafe = Fn.id
  end

  let forget = Fn.id

  let fee {fee; _} = fee
end

include Make (Ledger_proof.Stable.V1)
