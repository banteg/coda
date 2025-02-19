open Core_kernel
open Coda_base
open Module_version

let option lab =
  Option.value_map ~default:(Or_error.error_string lab) ~f:(fun x -> Ok x)

let map2_or_error xs ys ~f =
  let rec go xs ys acc =
    match (xs, ys) with
    | [], [] ->
        Ok (List.rev acc)
    | x :: xs, y :: ys -> (
      match f x y with Error e -> Error e | Ok z -> go xs ys (z :: acc) )
    | _, _ ->
        Or_error.error_string "Length mismatch"
  in
  go xs ys []

module Make
    (Inputs : Coda_intf.Tmp_test_stub_hack
              .For_transaction_snark_scan_state_intf) (Constants : sig
        val transaction_capacity_log_2 : int

        val work_delay_factor : int

        val latency_factor : int
    end) :
  Coda_intf.Transaction_snark_scan_state_generalized_intf
  with type transaction_snark_work := Inputs.Transaction_snark_work.t
   and type transaction_snark_statement := Transaction_snark.Statement.t
   and type sok_message := Inputs.Sok_message.t
   and type frozen_ledger_hash := Frozen_ledger_hash.t
   and type ledger_undo := Ledger.Undo.t
   and type transaction := Transaction.t
   and type staged_ledger_aux_hash := Inputs.Staged_ledger_aux_hash.t
   and type ledger_proof := Inputs.Ledger_proof.t = struct
  open Inputs

  module type Monad_with_Or_error_intf = sig
    type 'a t

    include Monad.S with type 'a t := 'a t

    module Or_error : sig
      type nonrec 'a t = 'a Or_error.t t

      include Monad.S with type 'a t := 'a t
    end
  end

  module Transaction_with_witness = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          (* TODO: The statement is redundant here - it can be computed from the witness and the transaction *)
          type t =
            { transaction_with_info: Ledger.Undo.Stable.V1.t
            ; statement: Transaction_snark.Statement.Stable.V1.t
            ; witness: Transaction_witness.Stable.V1.t sexp_opaque }
          [@@deriving sexp, bin_io, version]
        end

        include T
        include Registration.Make_latest_version (T)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "transaction_snark_scan_state_transaction_with_witness"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    type t = Stable.Latest.t =
      { transaction_with_info: Ledger.Undo.t
      ; statement: Transaction_snark.Statement.t
      ; witness: Transaction_witness.t }
    [@@deriving sexp]
  end

  module Ledger_proof_with_sok_message = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = Ledger_proof.Stable.V1.t * Sok_message.Stable.V1.t
          [@@deriving sexp, bin_io, version]
        end

        include T
        include Registration.Make_latest_version (T)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "transaction_snark_scan_state_ledger_proof_with_sok_message"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    type t = Ledger_proof.t * Sok_message.t [@@deriving sexp]
  end

  module Available_job = struct
    type t =
      ( Ledger_proof_with_sok_message.t
      , Transaction_with_witness.t )
      Parallel_scan.Available_job.t
    [@@deriving sexp]
  end

  module Space_partition = Parallel_scan.Space_partition

  module Job_view = struct
    type t = Transaction_snark.Statement.t Parallel_scan.Job_view.t
    [@@deriving sexp]

    let to_yojson ((pos, job) : t) : Yojson.Safe.json =
      let hash_string h = Sexp.to_string (Frozen_ledger_hash.sexp_of_t h) in
      let statement_to_yojson (s : Transaction_snark.Statement.t) =
        `Assoc
          [ ("Source", `String (hash_string s.source))
          ; ("Target", `String (hash_string s.target))
          ; ("Fee Excess", Currency.Fee.Signed.to_yojson s.fee_excess)
          ; ("Supply Increase", Currency.Amount.to_yojson s.supply_increase) ]
      in
      let opt_json x =
        Option.value_map x ~default:(`List []) ~f:statement_to_yojson
      in
      let job_to_yojson =
        match job with
        | Merge (x, y) ->
            `Assoc [("M", `List [opt_json x; opt_json y])]
        | Base x ->
            `Assoc [("B", `List [opt_json x])]
      in
      `List [`Int pos; job_to_yojson]
  end

  type job = Available_job.t [@@deriving sexp]

  type parallel_scan_completed_job =
    Ledger_proof_with_sok_message.Stable.V1.t
    Parallel_scan.State.Completed_job.t
  [@@deriving sexp, bin_io]

  (*Work capacity represents max number of work in the tree. this includes the jobs that are currently in the tree and the ones that would arise in the future when current jobs are done*)
  let work_capacity =
    let open Constants in
    let extra_jobs =
      let rec go i count =
        if i = work_delay_factor - 1 then count
        else go (i + 1) (count + Int.pow 2 (i - 1))
      in
      go (max latency_factor 1) 0
    in
    (Int.pow 2 (transaction_capacity_log_2 + 1) - 1)
    (*Transaction_capacity_tree size (c-tree) *)
    * (Int.pow 2 (work_delay_factor - Int.max (latency_factor - 1) 0) - 1)
    (*all but one c-tree that are in the tree formed at root_depth = latency_factor+1 *)
    + extra_jobs

  (*half the number of jobs on each level above work_delay_factor depth and below latency_factor depth*)

  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          { (*Job_count: Keeping track of the number of jobs added to the tree. Every transaction added amounts to two jobs*)
            tree:
              ( Ledger_proof_with_sok_message.Stable.V1.t
              , Transaction_with_witness.Stable.V1.t )
              Parallel_scan.State.Stable.V1.t
          ; mutable job_count: int }
        [@@deriving sexp, bin_io, version]
      end

      include T
      include Registration.Make_latest_version (T)

      let hash t =
        let state_hash =
          Parallel_scan.State.hash t.tree
            (Binable.to_string (module Ledger_proof_with_sok_message.Stable.V1))
            (Binable.to_string (module Transaction_with_witness.Stable.V1))
        in
        Staged_ledger_aux_hash.of_bytes
          ( (state_hash |> Digestif.SHA256.to_raw_string)
          ^ Int.to_string t.job_count )

      let is_valid t =
        let k = max Constants.work_delay_factor 2 in
        Parallel_scan.parallelism ~state:t.tree
        = Int.pow 2 (Constants.transaction_capacity_log_2 + k)
        && t.job_count <= work_capacity
        && Parallel_scan.is_valid t.tree

      include Binable.Of_binable
                (T)
                (struct
                  type nonrec t = t

                  let to_binable = Fn.id

                  let of_binable t =
                    (* assert (is_valid t) ; *)
                    t
                end)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "transaction_snark_scan_state"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  type t = Stable.Latest.t =
    { tree:
        ( Ledger_proof_with_sok_message.Stable.V1.t
        , Transaction_with_witness.Stable.V1.t )
        Parallel_scan.State.Stable.V1.t
    ; mutable job_count: int }
  [@@deriving sexp]

  [%%define_locally
  Stable.Latest.(hash, is_valid)]

  (**********Helpers*************)

  let create_expected_statement
      {Transaction_with_witness.transaction_with_info; witness; statement} =
    let open Or_error.Let_syntax in
    let source =
      Frozen_ledger_hash.of_ledger_hash
      @@ Sparse_ledger.merkle_root witness.ledger
    in
    let%bind transaction = Ledger.Undo.transaction transaction_with_info in
    let%bind after =
      Or_error.try_with (fun () ->
          Sparse_ledger.apply_transaction_exn witness.ledger transaction )
    in
    let target =
      Frozen_ledger_hash.of_ledger_hash @@ Sparse_ledger.merkle_root after
    in
    let pending_coinbase_before =
      statement.pending_coinbase_stack_state.source
    in
    let pending_coinbase_after =
      match transaction with
      | Coinbase c ->
          Pending_coinbase.Stack.push pending_coinbase_before c
      | _ ->
          pending_coinbase_before
    in
    let%bind fee_excess = Transaction.fee_excess transaction in
    let%map supply_increase = Transaction.supply_increase transaction in
    { Transaction_snark.Statement.source
    ; target
    ; fee_excess
    ; supply_increase
    ; pending_coinbase_stack_state=
        { Transaction_snark.Pending_coinbase_stack_state.source=
            pending_coinbase_before
        ; target= pending_coinbase_after }
    ; proof_type= `Base }

  let completed_work_to_scanable_work (job : job) (fee, current_proof, prover)
      : parallel_scan_completed_job Or_error.t =
    let sok_digest = Ledger_proof.sok_digest current_proof
    and proof = Ledger_proof.underlying_proof current_proof in
    match job with
    | Base ({statement; _}, _) ->
        let ledger_proof = Ledger_proof.create ~statement ~sok_digest ~proof in
        Ok (Lifted (ledger_proof, Sok_message.create ~fee ~prover))
    | Merge ((p, _), (p', _), _) ->
        let s = Ledger_proof.statement p and s' = Ledger_proof.statement p' in
        let open Or_error.Let_syntax in
        let%map fee_excess =
          Currency.Fee.Signed.add s.fee_excess s'.fee_excess
          |> option "Error adding fees"
        and supply_increase =
          Currency.Amount.add s.supply_increase s'.supply_increase
          |> option "Error adding supply_increases"
        in
        let statement =
          { Transaction_snark.Statement.source= s.source
          ; target= s'.target
          ; supply_increase
          ; pending_coinbase_stack_state=
              { source= s.pending_coinbase_stack_state.source
              ; target= s'.pending_coinbase_stack_state.target }
          ; fee_excess
          ; proof_type= `Merge }
        in
        Parallel_scan.State.Completed_job.Merged
          ( Ledger_proof.create ~statement ~sok_digest ~proof
          , Sok_message.create ~fee ~prover )

  let total_proofs (works : Transaction_snark_work.t list) =
    List.sum (module Int) works ~f:(fun w -> List.length w.proofs)

  (*************exposed functions*****************)

  module Make_statement_scanner
      (M : Monad_with_Or_error_intf) (Verifier : sig
          type t

          val verify :
               verifier:t
            -> proof:Ledger_proof.t
            -> statement:Transaction_snark.Statement.t
            -> message:Sok_message.t
            -> sexp_bool M.t
      end) =
  struct
    module Fold = Parallel_scan.State.Make_foldable (M)

    (*TODO: fold over the pending_coinbase tree and validate the statements?*)
    let scan_statement {tree; _} ~verifier :
        (Transaction_snark.Statement.t, [`Error of Error.t | `Empty]) Result.t
        M.t =
      let write_error description =
        sprintf !"Staged_ledger.scan_statement: %s\n" description
      in
      let open M.Let_syntax in
      let with_error ~f message =
        let%map result = f () in
        Result.map_error result ~f:(fun e ->
            Error.createf !"%s: %{sexp:Error.t}" (write_error message) e )
      in
      let merge_acc ~verify_proof (acc : Transaction_snark.Statement.t option)
          s2 : Transaction_snark.Statement.t option M.Or_error.t =
        let with_verification ~f =
          M.map (verify_proof ()) ~f:(fun is_verified ->
              if not is_verified then
                Or_error.error_string (write_error "Bad merge proof")
              else f () )
        in
        let open Or_error.Let_syntax in
        with_error "Bad merge proof" ~f:(fun () ->
            match acc with
            | None ->
                with_verification ~f:(fun () -> return (Some s2))
            | Some s1 ->
                with_verification ~f:(fun () ->
                    let%map merged_statement =
                      Transaction_snark.Statement.merge s1 s2
                    in
                    Some merged_statement ) )
      in
      let fold_step acc_statement job =
        match job with
        | Parallel_scan.State.Job.Merge (Rcomp (proof, message))
        | Merge (Lcomp (proof, message)) ->
            let statement = Ledger_proof.statement proof in
            merge_acc
              ~verify_proof:(fun () ->
                Verifier.verify ~verifier ~proof ~statement ~message )
              acc_statement statement
        | Merge Empty ->
            M.Or_error.return acc_statement
        | Merge (Bcomp ((proof_1, message_1), (proof_2, message_2), _place)) ->
            let open M.Or_error.Let_syntax in
            let%bind merged_statement =
              M.return
              @@ Transaction_snark.Statement.merge
                   (Ledger_proof.statement proof_1)
                   (Ledger_proof.statement proof_2)
            in
            merge_acc acc_statement merged_statement ~verify_proof:(fun () ->
                let open M.Let_syntax in
                let%map verified_list =
                  M.all
                    (List.map [(proof_1, message_1); (proof_2, message_2)]
                       ~f:(fun (proof, message) ->
                         Verifier.verify ~verifier ~proof
                           ~statement:(Ledger_proof.statement proof)
                           ~message ))
                in
                List.for_all verified_list ~f:Fn.id )
        | Base None ->
            M.Or_error.return acc_statement
        | Base (Some (transaction, _place)) ->
            with_error "Bad base statement" ~f:(fun () ->
                let open M.Or_error.Let_syntax in
                let%bind expected_statement =
                  M.return (create_expected_statement transaction)
                in
                if
                  Transaction_snark.Statement.equal transaction.statement
                    expected_statement
                then
                  merge_acc
                    ~verify_proof:(fun () -> M.return true)
                    acc_statement transaction.statement
                else
                  M.return
                  @@ Or_error.error_string (write_error "Bad base statement")
            )
      in
      let res =
        Fold.fold_chronological_until tree ~init:None
          ~finish:(Fn.compose M.return Result.return) ~f:(fun acc job ->
            let open Container.Continue_or_stop in
            match%map fold_step acc job with
            | Ok next ->
                Continue next
            | Error e ->
                Stop (Error e) )
      in
      match%map res with
      | Ok None ->
          Error `Empty
      | Ok (Some res) ->
          Ok res
      | Error e ->
          Error (`Error e)

    let check_invariants t ~verifier ~error_prefix
        ~ledger_hash_end:current_ledger_hash
        ~ledger_hash_begin:snarked_ledger_hash =
      let clarify_error cond err =
        if not cond then Or_error.errorf "%s : %s" error_prefix err else Ok ()
      in
      let open M.Let_syntax in
      match%map scan_statement ~verifier t with
      | Error (`Error e) ->
          Error e
      | Error `Empty ->
          let current_ledger_hash = current_ledger_hash in
          Option.value_map ~default:(Ok ()) snarked_ledger_hash ~f:(fun hash ->
              clarify_error
                (Frozen_ledger_hash.equal hash current_ledger_hash)
                "did not connect with snarked ledger hash" )
      | Ok
          { fee_excess
          ; source
          ; target
          ; supply_increase= _
          ; pending_coinbase_stack_state= _ (*TODO: check pending coinbases?*)
          ; proof_type= _ } ->
          let open Or_error.Let_syntax in
          let%map () =
            Option.value_map ~default:(Ok ()) snarked_ledger_hash
              ~f:(fun hash ->
                clarify_error
                  (Frozen_ledger_hash.equal hash source)
                  "did not connect with snarked ledger hash" )
          and () =
            clarify_error
              (Frozen_ledger_hash.equal current_ledger_hash target)
              "incorrect statement target hash"
          and () =
            clarify_error
              (Currency.Fee.Signed.equal Currency.Fee.Signed.zero fee_excess)
              "nonzero fee excess"
          in
          ()
  end

  let statement_of_job : job -> Transaction_snark.Statement.t option = function
    | Base ({statement; _}, _) ->
        Some statement
    | Merge ((p1, _), (p2, _), _) ->
        let stmt1 = Ledger_proof.statement p1
        and stmt2 = Ledger_proof.statement p2 in
        let open Option.Let_syntax in
        let%bind () =
          Option.some_if
            (Frozen_ledger_hash.equal stmt1.target stmt2.source)
            ()
        in
        let%map fee_excess =
          Currency.Fee.Signed.add stmt1.fee_excess stmt2.fee_excess
        and supply_increase =
          Currency.Amount.add stmt1.supply_increase stmt2.supply_increase
        in
        { Transaction_snark.Statement.source= stmt1.source
        ; target= stmt2.target
        ; supply_increase
        ; pending_coinbase_stack_state=
            { source= stmt1.pending_coinbase_stack_state.source
            ; target= stmt2.pending_coinbase_stack_state.target }
        ; fee_excess
        ; proof_type= `Merge }

  let capacity t = Parallel_scan.parallelism ~state:t.tree

  let create ~latency_factor ~work_delay_factor ~transaction_capacity_log_2 =
    (* Transaction capacity log_2 is 1/2^work_delay_factor the capacity for work parallelism *)
    let k = max work_delay_factor 2 in
    assert (work_delay_factor - latency_factor >= 1) ;
    { tree=
        Parallel_scan.start
          ~parallelism_log_2:(transaction_capacity_log_2 + k)
          ~root_at_depth:latency_factor
    ; job_count= 0 }

  let empty () =
    let open Constants in
    create ~latency_factor ~work_delay_factor ~transaction_capacity_log_2

  let extract_txns txns_with_witnesses =
    (* TODO: This type checks, but are we actually pulling the inverse txn here? *)
    List.map txns_with_witnesses
      ~f:(fun (txn_with_witness : Transaction_with_witness.t) ->
        Ledger.Undo.transaction txn_with_witness.transaction_with_info
        |> Or_error.ok_exn )

  let fill_work_and_enqueue_transactions t transactions work =
    let open Or_error.Let_syntax in
    let enqueue_transactions t transactions =
      Parallel_scan.enqueue_data ~state:t ~data:transactions
    in
    let fill_in_transaction_snark_work t
        (works : Transaction_snark_work.t list) :
        (Ledger_proof.t * Transaction.t list) option Or_error.t =
      let%bind next_jobs =
        Parallel_scan.next_k_jobs ~state:t ~k:(total_proofs works)
      in
      let%bind scanable_work_list =
        map2_or_error next_jobs
          (List.concat_map works
             ~f:(fun {Transaction_snark_work.fee; proofs; prover} ->
               List.map proofs ~f:(fun proof -> (fee, proof, prover)) ))
          ~f:completed_work_to_scanable_work
      in
      let%map result =
        Parallel_scan.fill_in_completed_jobs ~state:t
          ~completed_jobs:scanable_work_list
      in
      let really_result =
        Option.map result ~f:(fun ((proof, _), txns_with_witnesses) ->
            (proof, extract_txns txns_with_witnesses) )
      in
      really_result
    in
    let work_count =
      List.sum
        (module Int)
        work
        ~f:(fun (w : Transaction_snark_work.t) -> List.length w.proofs)
    in
    let old_proof = Parallel_scan.last_emitted_value t.tree in
    let%bind () = Parallel_scan.update_curr_job_seq_no t.tree in
    let%bind proof_opt = fill_in_transaction_snark_work t.tree work in
    let%bind () = enqueue_transactions t.tree transactions in
    (*important: Everytime a proof is emitted, reduce the job count by 1 
    because you only had to do (2^x - 2^latency_factor extra jobs). This is important because 
    otherwise the job count would never become zero*)
    let adjust_job_count =
      Option.value_map ~default:0 ~f:(fun _ -> 1) proof_opt
    in
    let new_count =
      t.job_count
      + (List.length transactions * 2)
      - work_count - adjust_job_count
    in
    let%bind () =
      Option.value_map ~default:(Ok ()) proof_opt ~f:(fun (proof, _) ->
          let curr_source = (Ledger_proof.statement proof).source in
          (*TODO: get genesis ledger hash if the old_proof is none*)
          let prev_target =
            Option.value_map ~default:curr_source old_proof
              ~f:(fun ((p', _), _) -> (Ledger_proof.statement p').target)
          in
          if Frozen_ledger_hash.equal curr_source prev_target then Ok ()
          else Or_error.error_string "Unexpected ledger proof emitted" )
    in
    if new_count <= work_capacity then (
      t.job_count <- new_count ;
      Ok proof_opt )
    else
      Or_error.error_string
        (sprintf
           "Job count (%d) exceeded work_capacity(%d). Cannot enqueue the \
            transactions"
           new_count work_capacity)

  let latest_ledger_proof t =
    let open Option.Let_syntax in
    let%map proof, txns_with_witnesses =
      Parallel_scan.last_emitted_value t.tree
    in
    (proof, extract_txns txns_with_witnesses)

  let current_job_count t = t.job_count

  let free_space t = Parallel_scan.free_space ~state:t.tree

  let next_k_jobs t ~k = Parallel_scan.next_k_jobs ~state:t.tree ~k

  let next_jobs t = Parallel_scan.next_jobs ~state:t.tree

  let next_jobs_sequence t = Parallel_scan.next_jobs_sequence ~state:t.tree

  let next_on_new_tree t = Parallel_scan.next_on_new_tree t.tree

  let base_jobs_on_latest_tree t =
    Parallel_scan.base_jobs_on_latest_tree t.tree

  let staged_transactions t =
    List.map (Parallel_scan.current_data t.tree)
      ~f:(fun (t : Transaction_with_witness.t) -> t.transaction_with_info)

  let all_transactions t =
    List.map ~f:(fun (t : Transaction_with_witness.t) ->
        t.transaction_with_info |> Ledger.Undo.transaction )
    @@ Parallel_scan.State.transactions t.tree
    |> Or_error.all

  let copy {tree; job_count} = {tree= Parallel_scan.State.copy tree; job_count}

  let partition_if_overflowing t =
    let max_throughput = Int.pow 2 Constants.transaction_capacity_log_2 in
    Parallel_scan.partition_if_overflowing t.tree ~max_slots:max_throughput

  let current_job_sequence_number {tree; _} =
    Parallel_scan.current_job_sequence_number tree

  let extract_from_job (job : job) =
    match job with
    | Parallel_scan.Available_job.Base (d, _) ->
        First (d.transaction_with_info, d.statement, d.witness)
    | Merge ((p1, _), (p2, _), _) ->
        Second (p1, p2)

  let snark_job_list_json t =
    let all_jobs : Job_view.t list =
      let fa (a : Ledger_proof_with_sok_message.t) =
        Ledger_proof.statement (fst a)
      in
      let fd (d : Transaction_with_witness.t) = d.statement in
      Parallel_scan.view_jobs_with_position t.tree fa fd
    in
    Yojson.Safe.to_string (`List (List.map all_jobs ~f:Job_view.to_yojson))

  let all_work_to_do t :
      Transaction_snark_work.Statement.t Sequence.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map work_seq = next_jobs_sequence t in
    Sequence.chunks_exn
      (Sequence.map work_seq ~f:(fun job ->
           match statement_of_job job with
           | None ->
               assert false
           | Some stmt ->
               stmt ))
      Transaction_snark_work.proofs_length
end

module Constants = Constants
