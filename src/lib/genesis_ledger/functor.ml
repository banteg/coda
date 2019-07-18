open Core_kernel
open Coda_base
open Signature_lib
open Currency

let pk = Public_key.Compressed.of_base58_check_exn

let sk = Private_key.of_base58_check_exn

module type Base_intf = sig
  val accounts : (Private_key.t option * Account.t) list
end

module Make_from_base (Base : Base_intf) : Intf.S = struct
  include Base

  (* TODO: #1488 compute this at compile time instead of lazily *)
  let t =
    lazy
      (let ledger = Ledger.create_ephemeral () in
       List.iter accounts ~f:(fun (_, account) ->
           Ledger.create_new_account_exn ledger account.public_key account ) ;
       ledger)

  let find_account_record_exn ~f =
    List.find_exn accounts ~f:(fun (_, account) -> f account)

  let find_new_account_record_exn old_account_pks =
    find_account_record_exn ~f:(fun new_account ->
        not
          (List.exists old_account_pks ~f:(fun old_account_pk ->
               Public_key.equal
                 (Public_key.decompress_exn (Account.public_key new_account))
                 old_account_pk )) )

  let keypair_of_account_record_exn (private_key, account) =
    let open Account in
    let sk_error_msg =
      "cannot access genesis ledger account private key "
      ^ "(HINT: did you forget to compile with `--profile=test`?)"
    in
    let pk_error_msg = "failed to decompress a genesis ledger public key" in
    let private_key = Option.value_exn private_key ~message:sk_error_msg in
    let public_key =
      Option.value_exn
        (Public_key.decompress account.Poly.Stable.Latest.public_key)
        ~message:pk_error_msg
    in
    {Keypair.public_key; private_key}

  let largest_account_exn =
    let error_msg =
      "cannot calculate largest account in genesis ledger: "
      ^ "genesis ledger has no accounts"
    in
    Memo.unit (fun () ->
        List.max_elt accounts ~compare:(fun (_, a) (_, b) ->
            Balance.compare a.balance b.balance )
        |> Option.value_exn ?here:None ?error:None ~message:error_msg )

  let largest_account_keypair_exn =
    Memo.unit (fun () -> keypair_of_account_record_exn (largest_account_exn ()))
end

module With_private = struct
  type account_data =
    {pk: Public_key.Compressed.t; sk: Private_key.t; balance: int}

  module type Source_intf = sig
    val accounts : account_data list
  end

  module Make (Source : Source_intf) : Intf.S = struct
    include Make_from_base (struct
      let accounts =
        List.map Source.accounts ~f:(fun {pk; sk; balance} ->
            (Some sk, Account.create pk (Balance.of_int balance)) )
    end)
  end
end

module Without_private = struct
  type account_data = {pk: Public_key.Compressed.t; balance: int}

  module type Source_intf = sig
    val accounts : account_data list
  end

  module Make (Source : Source_intf) : Intf.S = struct
    include Make_from_base (struct
      let accounts =
        List.map Source.accounts ~f:(fun {pk; balance} ->
            (None, Account.create pk (Balance.of_int balance)) )
    end)
  end
end
