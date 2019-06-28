open Core_kernel

module Make
    (Inputs : Intf.Inputs_intf)
    (Lib : Intf.Lib_intf with module Inputs := Inputs) =
struct
  let work ~snark_pool ~fee (staged_ledger : Inputs.Staged_ledger.t)
      (state : Lib.State.t) =
    let unseen_jobs = Lib.all_works staged_ledger state in
    match Lib.get_expensive_work ~snark_pool ~fee unseen_jobs with
    | [] ->
        ([], state)
    | _ ->
        let i = Random.int (List.length unseen_jobs) in
        let x = List.nth_exn unseen_jobs i in
        (Lib.pair_to_list x, Lib.State.set state x)
end

let%test_module "test" =
  ( module struct
    module Test = Test.Make_test (Make)
  end )
