open Core_kernel
open Async_kernel
open Coda_state
open Pipe_lib

module type Inputs_intf = sig
  include Transition_frontier.Inputs_intf

  module Network : sig
    type t
  end

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

  module Transition_frontier_controller :
    Coda_intf.Transition_frontier_controller_intf
    with type external_transition_validated := External_transition.Validated.t
     and type external_transition_with_initial_validation :=
                External_transition.with_initial_validation
     and type transition_frontier := Transition_frontier.t
     and type breadcrumb := Transition_frontier.Breadcrumb.t
     and type network := Network.t
     and type verifier := Verifier.t

  module Bootstrap_controller :
    Coda_intf.Bootstrap_controller_intf
    with type network := Network.t
     and type verifier := Verifier.t
     and type transition_frontier := Transition_frontier.t
     and type external_transition_with_initial_validation :=
                External_transition.with_initial_validation
end

module Make (Inputs : Inputs_intf) = struct
  open Inputs
  module Initial_validator = Initial_validator.Make (Inputs)

  let create_bufferred_pipe ?name () =
    Strict_pipe.create ?name (Buffered (`Capacity 50, `Overflow Crash))

  let is_transition_for_bootstrap ~logger root_state new_transition =
    let open External_transition in
    let new_state = protocol_state new_transition in
    Consensus.Hooks.should_bootstrap
      ~existing:(Protocol_state.consensus_state root_state)
      ~candidate:(Protocol_state.consensus_state new_state)
      ~logger:
        (Logger.extend logger
           [ ( "selection_context"
             , `String "Transition_router.is_transition_for_bootstrap" ) ])

  let get_root_state frontier =
    Transition_frontier.root frontier
    |> Transition_frontier.Breadcrumb.transition_with_hash |> With_hash.data
    |> External_transition.Validated.protocol_state

  let start_transition_frontier_controller ~logger ~trust_system ~verifier
      ~network ~time_controller ~proposer_transition_reader
      ~verified_transition_writer ~clear_reader ~collected_transitions
      ~transition_reader_ref ~transition_writer_ref ~frontier_w frontier =
    Logger.info logger ~module_:__MODULE__ ~location:__LOC__
      "Starting Transition Frontier Controller phase" ;
    let ( transition_frontier_controller_reader
        , transition_frontier_controller_writer ) =
      create_bufferred_pipe ~name:"transition frontier controller pipe" ()
    in
    transition_reader_ref := transition_frontier_controller_reader ;
    transition_writer_ref := transition_frontier_controller_writer ;
    Broadcast_pipe.Writer.write frontier_w (Some frontier) |> don't_wait_for ;
    let new_verified_transition_reader =
      Transition_frontier_controller.run ~logger ~trust_system ~verifier
        ~network ~time_controller ~collected_transitions ~frontier
        ~network_transition_reader:!transition_reader_ref
        ~proposer_transition_reader ~clear_reader
    in
    Strict_pipe.Reader.iter new_verified_transition_reader
      ~f:
        (Fn.compose Deferred.return
           (Strict_pipe.Writer.write verified_transition_writer))
    |> don't_wait_for

  let start_bootstrap_controller ~logger ~trust_system ~verifier ~network
      ~time_controller ~proposer_transition_reader ~verified_transition_writer
      ~clear_reader ~transition_reader_ref ~transition_writer_ref ~ledger_db
      ~frontier_w frontier =
    Logger.info logger ~module_:__MODULE__ ~location:__LOC__
      "Starting Bootstrap Controller phase" ;
    let bootstrap_controller_reader, bootstrap_controller_writer =
      create_bufferred_pipe ~name:"bootstrap controller pipe" ()
    in
    transition_reader_ref := bootstrap_controller_reader ;
    transition_writer_ref := bootstrap_controller_writer ;
    Transition_frontier.close frontier ;
    Broadcast_pipe.Writer.write frontier_w None |> don't_wait_for ;
    upon
      (Bootstrap_controller.run ~logger ~trust_system ~verifier ~network
         ~ledger_db ~frontier ~transition_reader:!transition_reader_ref)
      (fun (new_frontier, collected_transitions) ->
        Strict_pipe.Writer.kill !transition_writer_ref ;
        start_transition_frontier_controller ~logger ~trust_system ~verifier
          ~network ~time_controller ~proposer_transition_reader
          ~verified_transition_writer ~clear_reader ~collected_transitions
          ~transition_reader_ref ~transition_writer_ref ~frontier_w
          new_frontier )

  let run ~logger ~trust_system ~verifier ~network ~time_controller
      ~frontier_broadcast_pipe:(frontier_r, frontier_w) ~ledger_db
      ~network_transition_reader ~proposer_transition_reader frontier =
    let clear_reader, clear_writer =
      Strict_pipe.create ~name:"clear" Synchronous
    in
    let verified_transition_reader, verified_transition_writer =
      create_bufferred_pipe ~name:"verified transitions" ()
    in
    let transition_reader, transition_writer =
      create_bufferred_pipe ~name:"transition pipe" ()
    in
    let transition_reader_ref = ref transition_reader in
    let transition_writer_ref = ref transition_writer in
    (* This might be unsafe. Image the following scenario:
       If a node joined at the very end of the first epoch, and
       it didn't receive any transition from network for a while.
       Then it went to the second epoch and it could propose at
       the second epoch. *)
    if Consensus.Hooks.is_genesis @@ Coda_base.Block_time.now time_controller
    then
      start_transition_frontier_controller ~logger ~trust_system ~verifier
        ~network ~time_controller ~proposer_transition_reader
        ~verified_transition_writer ~clear_reader ~collected_transitions:[]
        ~transition_reader_ref ~transition_writer_ref ~frontier_w frontier
    else
      start_bootstrap_controller ~logger ~trust_system ~verifier ~network
        ~time_controller ~proposer_transition_reader
        ~verified_transition_writer ~clear_reader ~transition_reader_ref
        ~transition_writer_ref ~ledger_db ~frontier_w frontier ;
    let ( valid_protocol_state_transition_reader
        , valid_protocol_state_transition_writer ) =
      create_bufferred_pipe ~name:"valid transitions" ()
    in
    Initial_validator.run ~logger ~trust_system ~verifier
      ~transition_reader:network_transition_reader
      ~valid_transition_writer:valid_protocol_state_transition_writer ;
    Strict_pipe.Reader.iter_without_pushback
      valid_protocol_state_transition_reader ~f:(fun transition_with_time ->
        let `Transition enveloped_transition, _ = transition_with_time in
        let transition =
          Envelope.Incoming.data enveloped_transition |> fst |> With_hash.data
        in
        ( match Broadcast_pipe.Reader.peek frontier_r with
        | Some frontier ->
            if
              is_transition_for_bootstrap ~logger (get_root_state frontier)
                transition
            then (
              Strict_pipe.Writer.kill !transition_writer_ref ;
              Strict_pipe.Writer.write clear_writer `Clear |> don't_wait_for ;
              start_bootstrap_controller ~logger ~trust_system ~verifier
                ~network ~time_controller ~proposer_transition_reader
                ~verified_transition_writer ~clear_reader
                ~transition_reader_ref ~transition_writer_ref ~ledger_db
                ~frontier_w frontier )
        | None ->
            () ) ;
        Strict_pipe.Writer.write !transition_writer_ref transition_with_time )
    |> don't_wait_for ;
    verified_transition_reader
end

include Make (struct
  include Transition_frontier.Inputs
  module Transition_frontier = Transition_frontier
  module Network = Coda_networking
  module Transition_frontier_controller = Transition_frontier_controller
  module Bootstrap_controller = Bootstrap_controller
end)
