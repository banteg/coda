open Core_kernel
open Currency

module Test_inputs = struct
  module Transaction_witness = Int
  module Ledger_hash = Int
  module Sparse_ledger = Int
  module Transaction = Int
  module Ledger_proof_statement = Fee

  module Ledger_proof = struct
    module T = struct
      type t = Fee.t [@@deriving hash, compare, sexp]

      let of_binable = Fee.of_int

      let to_binable = Fee.to_int
    end

    include Binable.Of_binable (Int) (T)
    include T
  end

  module Transaction_snark_work = struct
    type t = Fee.t

    let fee = Fn.id
  end

  module Snark_pool = struct
    module T = struct
      type t = Ledger_proof.t list [@@deriving bin_io, hash, compare, sexp]
    end

    module Work = Hashable.Make_binable (T)

    type t = Transaction_snark_work.t Work.Table.t

    let get_completed_work (t : t) = Work.Table.find t

    let create () = Work.Table.create ()

    let add_snark t ~work ~fee = Work.Table.add_exn t ~key:work ~data:fee
  end

  module Staged_ledger = struct
    type t = int List.t

    let work i = Snark_work_lib.Work.Single.Spec.Transition (Fee.of_int i, i, i)

    let chunks_of xs ~n = List.groupi xs ~break:(fun i _ _ -> i mod n = 0)

    let paired ls =
      let pairs = chunks_of ls ~n:2 in
      List.map pairs ~f:(fun js ->
          match js with
          | [j] ->
              (work j, None)
          | [j1; j2] ->
              (work j1, Some (work j2))
          | _ ->
              failwith "error pairing jobs" )

    let all_work_pairs_exn (t : t) = paired t
  end
end

module Implementation_inputs = struct
  open Coda_base
  module Ledger_hash = Ledger_hash
  module Ledger_proof_statement = Transaction_snark.Statement
  module Sparse_ledger = Sparse_ledger
  module Transaction = Transaction
  module Transaction_witness = Transaction_witness
  module Ledger_proof = Ledger_proof
  module Transaction_snark_work = Transaction_snark_work
  module Snark_pool = Network_pool.Snark_pool
  module Staged_ledger = Staged_ledger
end
