open Core_kernel
open Async
open Coda_base

module type Inputs_intf = sig
  include Transition_frontier.Inputs_intf

  module Transition_frontier :
    Coda_intf.Transition_frontier_intf
    with type external_transition_validated := External_transition.Validated.t
     and type mostly_validated_external_transition :=
                ( [`Time_received] * Truth.true_t
                , [`Proof] * Truth.true_t
                , [`Frontier_dependencies] * Truth.true_t
                , [`Staged_ledger_diff] * Truth.false_t )
                External_transition.Validation.with_transition
     and type transaction_snark_scan_state := Staged_ledger.Scan_state.t
     and type staged_ledger_diff := Staged_ledger_diff.t
     and type staged_ledger := Staged_ledger.t
     and type verifier := Verifier.t
end

module Make (Inputs : Inputs_intf) :
  Coda_intf.Sync_handler_intf
  with type external_transition := Inputs.External_transition.t
   and type external_transition_validated :=
              Inputs.External_transition.Validated.t
   and type transition_frontier := Inputs.Transition_frontier.t
   and type parallel_scan_state := Inputs.Staged_ledger.Scan_state.t = struct
  open Inputs

  let get_breadcrumb_ledgers frontier =
    List.map
      (Transition_frontier.all_breadcrumbs frontier)
      ~f:
        (Fn.compose Staged_ledger.ledger
           Transition_frontier.Breadcrumb.staged_ledger)

  let get_ledger_by_hash ~frontier ledger_hash =
    let ledger_breadcrumbs =
      Sequence.of_lazy
        (lazy (Sequence.of_list @@ get_breadcrumb_ledgers frontier))
    in
    Sequence.append
      (Sequence.singleton
         (Transition_frontier.shallow_copy_root_snarked_ledger frontier))
      ledger_breadcrumbs
    |> Sequence.find ~f:(fun ledger ->
           Ledger_hash.equal (Ledger.merkle_root ledger) ledger_hash )

  let answer_query :
         frontier:Inputs.Transition_frontier.t
      -> Ledger_hash.t
      -> Sync_ledger.Query.t Envelope.Incoming.t
      -> logger:Logger.t
      -> trust_system:Trust_system.t
      -> Sync_ledger.Answer.t Option.t Deferred.t =
   fun ~frontier hash query ~logger ~trust_system ->
    match get_ledger_by_hash ~frontier hash with
    | None ->
        return None
    | Some ledger ->
        let responder =
          Sync_ledger.Mask.Responder.create ledger ignore ~logger ~trust_system
        in
        Sync_ledger.Mask.Responder.answer_query responder query

  let transition_catchup ~frontier state_hash =
    let open Option.Let_syntax in
    let%bind transitions =
      Transition_frontier.root_history_path_map frontier
        ~f:(fun b ->
          Transition_frontier.Breadcrumb.transition_with_hash b
          |> With_hash.data |> External_transition.Validated.forget_validation
          )
        state_hash
    in
    let length =
      Int.min
        (Non_empty_list.length transitions)
        (2 * Transition_frontier.max_length)
    in
    Non_empty_list.take (Non_empty_list.rev transitions) length
    >>| Non_empty_list.rev

  let mplus ma mb = if Option.is_some ma then ma else mb

  let get_staged_ledger_aux_and_pending_coinbases_at_hash ~frontier state_hash
      =
    let open Option.Let_syntax in
    let%map breadcrumb =
      mplus
        (Transition_frontier.find frontier state_hash)
        (Transition_frontier.find_in_root_history frontier state_hash)
    in
    let staged_ledger =
      Transition_frontier.Breadcrumb.staged_ledger breadcrumb
    in
    let scan_state = Staged_ledger.scan_state staged_ledger in
    let merkle_root =
      Staged_ledger.ledger staged_ledger |> Ledger.merkle_root
    in
    let pending_coinbases =
      Staged_ledger.pending_coinbase_collection staged_ledger
    in
    (scan_state, merkle_root, pending_coinbases)
end

include Make (struct
  include Transition_frontier.Inputs
  module Transition_frontier = Transition_frontier
end)
