[%%import
"../../config.mlh"]

open Core
open Import
open Coda_numbers
open Module_version
module Fee = Currency.Fee
module Payload = User_command_payload

module Poly = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type ('payload, 'pk, 'signature) t =
          {payload: 'payload; sender: 'pk; signature: 'signature}
        [@@deriving bin_io, compare, sexp, hash, yojson, version]
      end

      include T
    end

    module Latest = V1
  end

  type ('payload, 'pk, 'signature) t =
        ('payload, 'pk, 'signature) Stable.Latest.t =
    {payload: 'payload; sender: 'pk; signature: 'signature}
  [@@deriving sexp, hash, yojson]
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t =
        ( Payload.Stable.V1.t
        , Public_key.Stable.V1.t
        , Signature.Stable.V1.t )
        Poly.Stable.V1.t
      [@@deriving bin_io, compare, sexp, hash, yojson, version]
    end

    let version_byte = Base58_check.Version_bytes.user_command

    include T
    include Registration.Make_latest_version (T)
    include Comparable.Make (T)
    include Hashable.Make (T)

    let accounts_accessed ({payload; sender; _} : t) =
      Public_key.compress sender :: Payload.accounts_accessed payload
  end

  module Latest = V1

  module Module_decl = struct
    let name = "user_command"

    type latest = Latest.t
  end

  module Registrar = Registration.Make (Module_decl)
  module Registered_V1 = Registrar.Register (V1)
end

type t = Stable.Latest.t [@@deriving sexp, yojson, hash]

let accounts_accessed = Stable.Latest.accounts_accessed

include Comparable.Make (Stable.Latest)

let payload Poly.{payload; _} = payload

let fee = Fn.compose Payload.fee payload

let nonce = Fn.compose Payload.nonce payload

let sender t = Public_key.compress Poly.(t.sender)

let sign (kp : Signature_keypair.t) (payload : Payload.t) : t =
  { payload
  ; sender= kp.public_key
  ; signature= Schnorr.sign kp.private_key payload }

module For_tests = struct
  (* Pretend to sign a command. Much faster than actually signing. *)
  let fake_sign (kp : Signature_keypair.t) (payload : Payload.t) : t =
    { payload
    ; sender= kp.public_key
    ; signature= (Snark_params.Tock.Field.zero, Snark_params.Tock.Field.zero)
    }
end

module Gen = struct
  let gen_inner (sign' : Signature_lib.Keypair.t -> Payload.t -> t) ~key_gen
      ?(nonce = Account_nonce.zero) ~max_fee create_body =
    let open Quickcheck.Generator.Let_syntax in
    let%bind sender, (receiver : Signature_keypair.t) = key_gen
    and fee = Int.gen_incl 0 max_fee >>| Currency.Fee.of_int
    and memo = String.quickcheck_generator in
    let%map body = create_body receiver in
    let payload : Payload.t =
      Payload.create ~fee ~nonce
        ~memo:(User_command_memo.create_by_digesting_string_exn memo)
        ~body
    in
    sign' sender payload

  let with_random_participants ~keys ~gen =
    let key_gen = Quickcheck_lib.gen_pair @@ Quickcheck_lib.of_array keys in
    gen ~key_gen

  module Payment = struct
    let gen_inner (sign' : Signature_lib.Keypair.t -> Payload.t -> t) ~key_gen
        ?(nonce = Account_nonce.zero) ~max_amount ~max_fee () =
      gen_inner sign' ~key_gen ~nonce ~max_fee
      @@ fun {public_key= receiver; _} ->
      let open Quickcheck.Generator.Let_syntax in
      let%map amount = Int.gen_incl 1 max_amount >>| Currency.Amount.of_int in
      User_command_payload.Body.Payment
        {receiver= Public_key.compress receiver; amount}

    let gen ?(sign_type = `Fake) =
      match sign_type with
      | `Fake ->
          gen_inner For_tests.fake_sign
      | `Real ->
          gen_inner sign

    let gen_with_random_participants ?sign_type ~keys ?nonce ~max_amount
        ~max_fee =
      with_random_participants ~keys ~gen:(fun ~key_gen ->
          gen ?sign_type ~key_gen ?nonce ~max_amount ~max_fee )
  end

  module Stake_delegation = struct
    let gen ~key_gen ?nonce ~max_fee () =
      gen_inner For_tests.fake_sign ~key_gen ?nonce ~max_fee
        (fun {public_key= new_delegate; _} ->
          Quickcheck.Generator.return
          @@ User_command_payload.Body.Stake_delegation
               (Set_delegate {new_delegate= Public_key.compress new_delegate})
      )

    let gen_with_random_participants ~keys ?nonce ~max_fee =
      with_random_participants ~keys ~gen:(gen ?nonce ~max_fee)
  end

  let payment = Payment.gen

  let payment_with_random_participants = Payment.gen_with_random_participants

  let stake_delegation = Stake_delegation.gen

  let stake_delegation_with_random_participants =
    Stake_delegation.gen_with_random_participants

  let sequence :
         ?length:int
      -> ?sign_type:[`Fake | `Real]
      -> ( Signature_lib.Keypair.t
         * Currency.Amount.t
         * Coda_numbers.Account_nonce.t )
         array
      -> t list Quickcheck.Generator.t =
   fun ?length ?(sign_type = `Fake) account_info ->
    let open Quickcheck.Generator in
    let open Quickcheck.Generator.Let_syntax in
    let%bind n_commands =
      Option.value_map length ~default:small_non_negative_int ~f:return
    in
    if Int.(n_commands = 0) then return []
    else
      let n_accounts = Array.length account_info in
      (* How many commands will be issued from each account? *)
      let%bind command_splits =
        Quickcheck_lib.gen_division n_commands n_accounts
      in
      let command_splits' = Array.of_list command_splits in
      (* List of payment senders in the final order. *)
      let%bind command_senders =
        Quickcheck_lib.shuffle
        @@ List.concat_mapi command_splits ~f:(fun idx cmds ->
               List.init cmds ~f:(Fn.const idx) )
      in
      (* within the accounts, how will the currency be split into separate
         payments? *)
      let%bind currency_splits =
        Quickcheck_lib.init_gen_array
          ~f:(fun i ->
            let%bind spend_all = bool in
            let balance = Tuple3.get2 account_info.(i) in
            let amount_to_spend =
              if spend_all then balance
              else Currency.Amount.of_int (Currency.Amount.to_int balance / 2)
            in
            Quickcheck_lib.gen_division_currency amount_to_spend
              command_splits'.(i) )
          n_accounts
        |> (* We need to ensure each command has enough currency for a fee of 2
             or more, so it'll be enough to buy the requisite transaction snarks.
          *)
           Quickcheck.Generator.filter ~f:(fun splits ->
               Array.for_all splits ~f:(fun split ->
                   List.for_all split ~f:(fun amt ->
                       Currency.Amount.(amt >= of_int 2) ) ) )
      in
      let account_nonces = Array.map ~f:Tuple3.get3 account_info in
      let uncons_exn = function
        | [] ->
            failwith "uncons_exn"
        | x :: xs ->
            (x, xs)
      in
      Quickcheck_lib.map_gens command_senders ~f:(fun sender ->
          let this_split, rest_splits = uncons_exn currency_splits.(sender) in
          currency_splits.(sender) <- rest_splits ;
          let nonce = account_nonces.(sender) in
          account_nonces.(sender) <- Account_nonce.succ nonce ;
          let%bind fee =
            Currency.Fee.(
              gen_incl (of_int 2)
                (min (of_int 10) @@ Currency.Amount.to_fee this_split))
          in
          let amount =
            Option.value_exn Currency.Amount.(this_split - of_fee fee)
          in
          let%bind receiver =
            map ~f:(fun idx ->
                Public_key.compress (Tuple3.get1 account_info.(idx)).public_key
            )
            @@ Int.gen_uniform_incl 0 (n_accounts - 1)
          in
          let memo = User_command_memo.dummy in
          let payload =
            Payload.create ~fee ~nonce ~memo ~body:(Payment {receiver; amount})
          in
          let sign' =
            match sign_type with `Fake -> For_tests.fake_sign | `Real -> sign
          in
          return @@ sign' (account_info.(sender) |> Tuple3.get1) payload )
end

module With_valid_signature = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t = Stable.V1.t
        [@@deriving sexp, eq, bin_io, yojson, version, hash]
      end

      include T
      include Registration.Make_latest_version (T)

      let compare = Stable.V1.compare

      module Gen = Gen
    end

    module Latest = V1

    module Module_decl = struct
      let name = "user_command_with_valid_signature"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  include Stable.Latest
  include Comparable.Make (Stable.Latest)
end

module Base58_check = Codable.Make_base58_check (Stable.Latest)

[%%define_locally
Base58_check.(to_base58_check, of_base58_check, of_base58_check_exn)]

[%%define_locally
Base58_check.String_ops.(to_string, of_string)]

[%%if
fake_hash]

let check_signature _ = true

[%%else]

let check_signature ({payload; sender; signature} : t) =
  Schnorr.verify signature
    (Snark_params.Tick.Inner_curve.of_affine sender)
    payload

[%%endif]

let gen_test =
  let keys = Array.init 2 ~f:(fun _ -> Signature_keypair.create ()) in
  Gen.payment_with_random_participants ~sign_type:`Real ~keys ~max_amount:10000
    ~max_fee:1000 ()

let%test_unit "completeness" =
  Quickcheck.test ~trials:20 gen_test ~f:(fun t -> assert (check_signature t))

let%test_unit "json" =
  Quickcheck.test ~trials:20 ~sexp_of:sexp_of_t gen_test ~f:(fun t ->
      assert (Codable.For_tests.check_encoding (module Stable.Latest) ~equal t)
  )

let check t = Option.some_if (check_signature t) t

let forget_check t = t

let filter_by_participant user_commands public_key =
  List.filter user_commands ~f:(fun user_command ->
      List.mem
        (accounts_accessed user_command)
        public_key ~equal:Public_key.Compressed.equal )
