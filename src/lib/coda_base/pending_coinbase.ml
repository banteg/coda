open Core_kernel
open Import
open Snarky
open Snark_params
open Snark_params.Tick
open Tuple_lib
open Let_syntax
open Currency
open Fold_lib
open Module_version

module Coinbase_data = struct
  type t =
    Public_key.Compressed.Stable.V1.t
    * Amount.Stable.V1.t
    * State_body_hash.Stable.V1.t
  [@@deriving bin_io, sexp]

  let of_coinbase (cb : Coinbase.t) : t =
    (cb.proposer, cb.amount, cb.state_body_hash)

  type var = Public_key.Compressed.var * Amount.var * State_body_hash.var

  type value = t [@@deriving bin_io, sexp]

  let length_in_triples =
    Public_key.Compressed.length_in_triples + Amount.length_in_triples
    + State_body_hash.length_in_triples

  let var_of_t ((public_key, amount, state_body_hash) : value) =
    ( Public_key.Compressed.var_of_t public_key
    , Amount.var_of_t amount
    , State_body_hash.var_of_t state_body_hash )

  let var_to_triples (public_key, amount, state_body_hash) =
    let%bind public_key = Public_key.Compressed.var_to_triples public_key in
    let amount = Amount.var_to_triples amount in
    let%map state_body_hash = State_body_hash.var_to_triples state_body_hash in
    public_key @ amount @ state_body_hash

  let fold ((public_key, amount, state_body_hash) : t) =
    let open Fold in
    Public_key.Compressed.fold public_key
    +> Amount.fold amount
    +> State_body_hash.fold state_body_hash

  let typ : (var, value) Typ.t =
    let spec =
      let open Data_spec in
      [Public_key.Compressed.typ; Amount.typ; State_body_hash.typ]
    in
    let of_hlist
          : 'public_key 'amount 'state_body_hash.    ( unit
                                                     ,    'public_key
                                                       -> 'amount
                                                       -> 'state_body_hash
                                                       -> unit )
                                                     H_list.t
            -> 'public_key * 'amount * 'state_body_hash =
      let open H_list in
      fun [public_key; amount; state_body_hash] ->
        (public_key, amount, state_body_hash)
    in
    let to_hlist (public_key, amount, state_body_hash) =
      H_list.[public_key; amount; state_body_hash]
    in
    Typ.of_hlistable spec ~var_to_hlist:to_hlist ~var_of_hlist:of_hlist
      ~value_to_hlist:to_hlist ~value_of_hlist:of_hlist

  let empty = (Public_key.Compressed.empty, Amount.zero, State_body_hash.dummy)

  let genesis = empty
end

module Stack_with_state_hash_id : sig
  module Stable : sig
    module V1 : sig
      type t [@@deriving bin_io, sexp, compare, eq, version]
    end

    module Latest = V1
  end

  (* bin_io, version omitted *)
  type t = Stable.Latest.t [@@deriving sexp, compare, eq]

  val of_int : int -> t

  val to_int : t -> int

  val zero : t

  val incr_by_one : t -> t Or_error.t

  val to_string : t -> string

  val ( > ) : t -> t -> bool
end = struct
  module Stable = struct
    module V1 = struct
      module T = struct
        type t = int [@@deriving sexp, compare, eq, bin_io, version]
      end

      include T
      include Registration.Make_latest_version (T)
    end

    module Latest = V1

    module Module_decl = struct
      let name = "pending_coinbase_stack_id"

      type latest = Latest.t
    end

    module Registrar = Registration.Make (Module_decl)
    module Registered_V1 = Registrar.Register (V1)
  end

  type t = Stable.Latest.t [@@deriving sexp, compare]

  [%%define_locally
  Int.(( > ), to_string, zero, to_int, of_int, equal)]

  let incr_by_one t1 =
    let t2 = t1 + 1 in
    if t2 < t1 then Or_error.error_string "Stack_with_state_hash_id overflow"
    else Ok t2
end

module type Data_hash_binable_intf = sig
  type t [@@deriving sexp, compare, eq, yojson, hash]

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving bin_io, sexp, compare, eq, yojson, hash, version]
    end

    module Latest = V1
  end

  type var

  val var_of_t : t -> var

  val typ : (var, t) Typ.t

  val var_to_triples : var -> (Boolean.var Triple.t list, _) Tick.Checked.t

  val length_in_triples : int

  val equal_var : var -> var -> (Boolean.var, _) Tick.Checked.t

  val fold : t -> bool Triple.t Fold.t

  val to_bytes : t -> string

  val to_bits : t -> bool list

  val gen : t Quickcheck.Generator.t
end

module Data_hash_binable = struct
  include Data_hash.Make_full_size ()
end

module Coinbase_stack = struct
  module Stack = struct
    include Data_hash_binable

    let push (h : t) cb =
      let coinbase = Coinbase_data.of_coinbase cb in
      Pedersen.digest_fold Hash_prefix.coinbase_stack
        Fold.(Coinbase_data.fold coinbase +> fold h)
      |> of_hash

    let empty =
      of_hash (Pedersen.(State.salt "CoinbaseStack") |> Pedersen.State.digest)

    module Checked = struct
      type t = var

      let push (t : t) (coinbase : Coinbase_data.var) =
        (*Prefix+Coinbase+Current-stack*)
        let init =
          Pedersen.Checked.Section.create
            ~acc:(`Value Hash_prefix.coinbase_stack.acc)
            ~support:
              (Interval_union.of_interval (0, Hash_prefix.length_in_triples))
        in
        let%bind coinbase_section =
          let%bind bs = Coinbase_data.var_to_triples coinbase in
          Pedersen.Checked.Section.extend init bs
            ~start:Hash_prefix.length_in_triples
        in
        let%bind with_t =
          let%bind bs = var_to_triples t in
          Pedersen.Checked.Section.extend Pedersen.Checked.Section.empty bs
            ~start:
              (Hash_prefix.length_in_triples + Coinbase_data.length_in_triples)
        in
        let%map s =
          Pedersen.Checked.Section.disjoint_union_exn coinbase_section with_t
        in
        let digest, _ =
          Pedersen.Checked.Section.to_initial_segment_digest_exn s
        in
        var_of_hash_packed digest

      let if_ = if_

      let empty = var_of_t empty
    end
  end
end

(* Pending coinbase hash *)
module Hash_builder = struct
  include Data_hash_binable

  let merge ~height (h1 : t) (h2 : t) =
    let open Tick.Pedersen in
    State.digest
      (hash_fold
         Hash_prefix.coinbase_merkle_tree.(height)
         Fold.(Digest.fold (h1 :> field) +> Digest.fold (h2 :> field)))
    |> of_hash

  let empty_hash =
    let open Tick.Pedersen in
    digest_fold (State.create ())
      (Fold.string_triples "Pending coinbases merkle tree")
    |> of_hash

  let of_digest = Fn.compose Fn.id of_hash
end

module Stack_builder = Coinbase_stack.Stack

(* Sparse_ledger.Make is applied more than once in the code, so
   it can't make assumptions about the internal structure of its module
   arguments. Therefore, for modules with a bin_io type passed to the functor,
   that type cannot be in a version module hierarchy. We build the required
   modules for Hash and Stack.
 *)

module Make (Depth : sig
  val depth : int
end) =
struct
  include Depth

  (* Total number of stacks *)
  let max_coinbase_stack_count = Int.pow 2 depth

  module Stack = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = Stack_builder.Stable.V1.t
          [@@deriving bin_io, eq, yojson, hash, sexp, compare, version]
        end

        include T
        include Registration.Make_latest_version (T)

        let data_hash (t : t) = Hash_builder.of_digest (t :> field)
      end

      module Latest = V1

      module Module_decl = struct
        let name = "pending_coinbase_stack"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    (* bin_io, version omitted *)
    type t = Stable.Latest.t [@@deriving eq, yojson, compare, sexp, hash]

    type var = Stack_builder.var

    let data_hash = Stable.Latest.data_hash

    [%%define_locally
    Stack_builder.
      ( to_bits
      , to_bytes
      , fold
      , equal_var
      , length_in_triples
      , var_to_triples
      , hash_fold_t
      , empty
      , push
      , gen
      , var_of_t
      , typ )]

    module Checked = Stack_builder.Checked
  end

  module Stack_with_state_hash = struct
    module Poly = struct
      module Stable = struct
        module V1 = struct
          module T = struct
            type ('stack, 'state_hash) t =
              {stack: 'stack; state_hash: 'state_hash}
            [@@deriving bin_io, eq, yojson, hash, sexp, compare, version]
          end

          include T
        end

        module Latest = V1
      end

      type ('stack, 'state_hash) t = ('stack, 'state_hash) Stable.Latest.t =
        {stack: 'stack; state_hash: 'state_hash}
      [@@deriving yojson, hash, sexp, compare]
    end

    module Stable = struct
      module V1 = struct
        module T = struct
          type t = (Stack.Stable.V1.t, State_hash.Stable.V1.t) Poly.Stable.V1.t
          [@@deriving bin_io, eq, yojson, hash, sexp, compare, version]
        end

        include T
        include Registration.Make_latest_version (T)

        let data_hash (t : t) =
          (* TODO: does this make sense *)
          Hash_builder.(
            merge ~height:0
              (of_digest (t.stack :> field))
              (of_digest (t.state_hash :> field)))
      end

      module Latest = V1

      module Module_decl = struct
        let name = "pending_coinbase_stack_with_state_hash"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    (* bin_io, version omitted *)
    type t = Stable.Latest.t [@@deriving yojson, compare, sexp, hash]

    [%%define_locally
    Stable.Latest.(data_hash)]

    type var = (Stack.var, State_hash.var) Poly.t

    let to_hlist {Poly.stack; state_hash} = H_list.[stack; state_hash]

    let of_hlist :
           (unit, 'stack -> 'state_hash -> unit) H_list.t
        -> ('stack, 'stack_hash) Poly.t =
     fun H_list.[stack; state_hash] -> {stack; state_hash}

    let data_spec = Snark_params.Tick.Data_spec.[Stack.typ; State_hash.typ]

    let typ : (var, t) Typ.t =
      Snark_params.Tick.Typ.of_hlistable data_spec ~var_to_hlist:to_hlist
        ~var_of_hlist:of_hlist ~value_to_hlist:to_hlist
        ~value_of_hlist:of_hlist

    let empty = {Poly.stack= Stack.empty; state_hash= State_hash.dummy}

    let equal_stacks t1 t2 = Stack.equal t1.Poly.stack t2.Poly.stack

    let var_to_hash_packed var =
      (* TODO : is this right *)
      let stack_hash =
        Coinbase_stack.Stack.var_to_hash_packed var.Poly.stack
      in
      let state_hash_hash =
        State_hash.var_to_hash_packed var.Poly.state_hash
      in
      Tick0.Field.Var.add stack_hash state_hash_hash

    let if_ (cond : Tick0.Boolean.var) ~(then_ : var) ~(else_ : var) :
        (var, 'a) Tick0.Checked.t =
      let%bind stack =
        Stack.Checked.if_ cond ~then_:then_.stack ~else_:else_.stack
      in
      let%map state_hash =
        State_hash.if_ cond ~then_:then_.state_hash ~else_:else_.state_hash
      in
      {Poly.stack; state_hash}

    module Checked = struct
      let empty =
        { Poly.stack= Stack.Checked.empty
        ; state_hash= State_hash.(var_of_t dummy) }
    end
  end

  module Hash = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = Hash_builder.Stable.V1.t
          [@@deriving bin_io, eq, compare, sexp, version, yojson, hash]
        end

        include T
        include Registration.Make_latest_version (T)

        type var = Hash_builder.var

        let merge = Hash_builder.merge
      end

      module Latest = V1

      module Module_decl = struct
        let name = "pending_coinbase_hash"

        type latest = Latest.t
      end

      module Registrar = Registration.Make (Module_decl)
      module Registered_V1 = Registrar.Register (V1)
    end

    (* bin_io, version omitted *)
    type t = Stable.Latest.t [@@deriving eq, compare, sexp, yojson, hash]

    type var = Stable.Latest.var

    [%%define_locally
    Stable.Latest.(merge)]

    [%%define_locally
    Hash_builder.
      ( of_digest
      , empty_hash
      , gen
      , to_bits
      , to_bytes
      , fold
      , equal_var
      , length_in_triples
      , var_to_triples
      , var_of_t
      , var_of_hash_packed
      , var_to_hash_packed
      , typ )]
  end

  (* the arguments to Sparse_ledger.Make are all versioned; a particular choice of those
     versions yields a version of the result
   *)

  module V1_make =
    Sparse_ledger_lib.Sparse_ledger.Make
      (Hash.Stable.V1)
      (Stack_with_state_hash_id.Stable.V1)
      (Stack_with_state_hash.Stable.V1)

  module Merkle_tree = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = V1_make.Stable.V1.t
          [@@deriving bin_io, sexp, version {unnumbered}]
        end

        include T
      end

      module Latest = V1
    end

    module Latest_make = V1_make

    [%%define_locally
    Latest_make.
      ( of_hash
      , get_exn
      , path_exn
      , set_exn
      , find_index_exn
      , add_path
      , merkle_root )]
  end

  module Checked = struct
    open Coinbase_stack

    type var = Hash.Stable.V1.var

    module Merkle_tree =
      Snarky.Merkle_tree.Checked
        (Tick)
        (struct
          type value = Pedersen.Checked.Digest.t

          type var = Pedersen.Checked.Digest.var

          let typ = Pedersen.Checked.Digest.typ

          let hash ~height h1 h2 =
            let to_triples (bs : Pedersen.Checked.Digest.Unpacked.var) =
              Bitstring_lib.Bitstring.pad_to_triple_list
                ~default:Boolean.false_
                (bs :> Boolean.var list)
            in
            let%bind h1 = Pedersen.Checked.Digest.choose_preimage h1
            and h2 = Pedersen.Checked.Digest.choose_preimage h2 in
            Pedersen.Checked.digest_triples
              ~init:Hash_prefix.coinbase_merkle_tree.(height)
              (to_triples h1 @ to_triples h2)

          let assert_equal h1 h2 = Field.Checked.Assert.equal h1 h2

          let if_ = Field.Checked.if_
        end)
        (struct
          include Stack_with_state_hash

          type value = t [@@deriving sexp]

          let hash (t : var) =
            (* TODO: is this OK *)
            let stack_hash = Stack.(var_to_hash_packed t.Poly.stack) in
            let state_hash_hash =
              State_hash.(var_to_hash_packed t.Poly.state_hash)
            in
            return @@ Tick0.Field.Var.add stack_hash state_hash_hash
        end)

    module Path = Merkle_tree.Path

    type path = Path.value

    module Address = struct
      include Merkle_tree.Address

      let typ = typ ~depth
    end

    type _ Request.t +=
      | Coinbase_stack_with_state_hash_path : Address.value -> path Request.t
      | Get_coinbase_stack_with_state_hash :
          Address.value
          -> (Stack_with_state_hash.t * path) Request.t
      | Set_coinbase_stack_with_state_hash :
          Address.value * Stack_with_state_hash.t
          -> unit Request.t
      | Find_index_of_newest_stack_with_state_hash : Address.value Request.t
      | Find_index_of_oldest_stack_with_state_hash : Address.value Request.t

    let reraise_merkle_requests (With {request; respond}) =
      match request with
      | Merkle_tree.Get_path addr ->
          respond (Delegate (Coinbase_stack_with_state_hash_path addr))
      | Merkle_tree.Set (addr, stack) ->
          respond (Delegate (Set_coinbase_stack_with_state_hash (addr, stack)))
      | Merkle_tree.Get_element addr ->
          respond (Delegate (Get_coinbase_stack_with_state_hash addr))
      | _ ->
          unhandled

    let get t addr =
      handle
        (Merkle_tree.get_req ~depth (Hash.var_to_hash_packed t) addr)
        reraise_merkle_requests

    let update_state_hash ~(state_hash : State_hash.var)
        ~(state_body_hash : State_body_hash.var) =
      let%bind state_hash_triples = State_hash.var_to_triples state_hash in
      let%bind state_body_hash_triples =
        State_body_hash.var_to_triples state_body_hash
      in
      let triples = state_hash_triples @ state_body_hash_triples in
      let%map digest =
        Snark_params.Tick.Pedersen.Checked.digest_triples
          ~init:Hash_prefix.protocol_state triples
      in
      State_hash.var_of_hash_packed digest

    let%snarkydef add_coinbase t (pk, amount, state_body_hash) =
      let%bind addr =
        request_witness Address.typ
          As_prover.(
            map (return ()) ~f:(fun _ ->
                Find_index_of_newest_stack_with_state_hash ))
      in
      let equal_to_zero x = Amount.(equal_var x (var_of_t zero)) in
      let chain if_ b ~then_ ~else_ =
        let%bind then_ = then_ and else_ = else_ in
        if_ b ~then_ ~else_
      in
      handle
        (Merkle_tree.modify_req ~depth (Hash.var_to_hash_packed t) addr
           ~f:(fun stack_with_state_hash ->
             let total_coinbase_amount =
               Currency.Amount.var_of_t Coda_compile_config.coinbase
             in
             let%bind rem_amount =
               Currency.Amount.Checked.sub total_coinbase_amount amount
             in
             let%bind amount1_equal_to_zero = equal_to_zero amount in
             let%bind amount2_equal_to_zero = equal_to_zero rem_amount in
             (*TODO:Optimize here since we are pushing twice to the same stack*)
             let%bind stack_with_amount1 =
               Coinbase_stack.Stack.Checked.push stack_with_state_hash.stack
                 (pk, amount, state_body_hash)
             in
             let%bind stack_with_amount2 =
               Coinbase_stack.Stack.Checked.push stack_with_amount1
                 (pk, rem_amount, state_body_hash)
             in
             let%bind new_state_hash =
               update_state_hash ~state_hash:stack_with_state_hash.state_hash
                 ~state_body_hash
             in
             chain Stack_with_state_hash.if_ amount1_equal_to_zero
               ~then_:(return stack_with_state_hash)
               ~else_:
                 (Stack_with_state_hash.if_ amount2_equal_to_zero
                    ~then_:
                      Stack_with_state_hash.Poly.
                        {stack= stack_with_amount1; state_hash= new_state_hash}
                    ~else_:
                      Stack_with_state_hash.Poly.
                        {stack= stack_with_amount2; state_hash= new_state_hash})
         ))
        reraise_merkle_requests
      >>| Hash.var_of_hash_packed

    let%snarkydef pop_coinbases t ~proof_emitted =
      let%bind addr =
        request_witness Address.typ
          As_prover.(
            map (return ()) ~f:(fun _ ->
                Find_index_of_oldest_stack_with_state_hash ))
      in
      let%bind prev, prev_path =
        request_witness
          Typ.(Stack_with_state_hash.typ * Path.typ ~depth)
          As_prover.(
            map (read Address.typ addr) ~f:(fun a ->
                Get_coinbase_stack_with_state_hash a ))
      in
      let stack_with_state_hash_hash =
        Stack_with_state_hash.var_to_hash_packed
      in
      let prev_entry_hash = stack_with_state_hash_hash prev in
      let%bind () =
        Merkle_tree.implied_root prev_entry_hash addr prev_path
        >>= Field.Checked.Assert.equal (Hash.var_to_hash_packed t)
      in
      let%bind next =
        Stack_with_state_hash.if_ proof_emitted
          ~then_:Stack_with_state_hash.Checked.empty ~else_:prev
      in
      let next_entry_hash = stack_with_state_hash_hash next in
      let%bind () =
        perform
          (let open As_prover in
          let open Let_syntax in
          let%map addr = read Address.typ addr
          and next = read Stack_with_state_hash.typ next in
          Set_coinbase_stack_with_state_hash (addr, next))
      in
      let%map new_root =
        Merkle_tree.implied_root next_entry_hash addr prev_path
      in
      (Hash.var_of_hash_packed new_root, prev)
  end

  module Poly = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type ('tree, 'stack_id, 'state_hash) t =
            { tree: 'tree
            ; pos_list: 'stack_id list
            ; new_pos: 'stack_id
            ; previous_state_hash: 'state_hash }
          [@@deriving bin_io, sexp, version]
        end

        include T
      end

      module Latest = V1
    end

    type ('tree, 'stack_id, 'state_hash) t =
          ('tree, 'stack_id, 'state_hash) Stable.Latest.t =
      { tree: 'tree
      ; pos_list: 'stack_id list
      ; new_pos: 'stack_id
      ; previous_state_hash: 'state_hash }
  end

  module Stable = struct
    module V1 = struct
      module T = struct
        type t =
          ( Merkle_tree.Stable.V1.t
          , Stack_with_state_hash_id.Stable.V1.t
          , State_hash.Stable.V1.t )
          Poly.Stable.V1.t
        [@@deriving bin_io, sexp, version {unnumbered}]
      end

      include T
    end

    module Latest = V1
  end

  type t = Stable.Latest.t [@@deriving sexp]

  let create_exn' () =
    let init_hash = Stack.data_hash Stack.empty in
    let hash_on_level, root_hash =
      List.fold
        (List.init depth ~f:(fun i -> i + 1))
        ~init:([(0, init_hash)], init_hash)
        ~f:(fun (hashes, (cur_hash : Stack.t)) height ->
          let (merged : Stack.t) =
            Hash.merge ~height:(height - 1) cur_hash cur_hash
          in
          ((height, merged) :: hashes, merged) )
    in
    let rec create_path height path key =
      if height < 0 then path
      else
        let hash =
          Option.value_exn
            (List.Assoc.find ~equal:Int.equal hash_on_level height)
        in
        create_path (height - 1)
          ((if key mod 2 = 0 then `Left hash else `Right hash) :: path)
          (key / 2)
    in
    let rec make_tree t key =
      if
        Stack_with_state_hash_id.( > ) key
          (Stack_with_state_hash_id.of_int @@ (Int.pow 2 depth - 1))
      then t
      else
        let path =
          create_path (depth - 1) [] (Stack_with_state_hash_id.to_int key)
        in
        make_tree
          (Merkle_tree.add_path t path key Stack_with_state_hash.empty)
          (Or_error.ok_exn (Stack_with_state_hash_id.incr_by_one key))
    in
    { Poly.tree=
        make_tree
          (Merkle_tree.of_hash ~depth root_hash)
          Stack_with_state_hash_id.zero
    ; pos_list= []
    ; new_pos= Stack_with_state_hash_id.zero
    ; previous_state_hash= State_hash.dummy }

  [%%define_locally
  Or_error.(try_with)]

  let create () = try_with (fun () -> create_exn' ())

  let merkle_root (t : t) = Merkle_tree.merkle_root t.tree

  let get_stack_with_state_hash (t : t) index =
    try_with (fun () -> Merkle_tree.get_exn t.tree index)

  let path (t : t) index =
    try_with (fun () -> Merkle_tree.path_exn t.tree index)

  let set_stack_with_state_hash (t : t) index stack_with_state_hash =
    try_with (fun () ->
        {t with tree= Merkle_tree.set_exn t.tree index stack_with_state_hash}
    )

  let find_index (t : t) key =
    try_with (fun () -> Merkle_tree.find_index_exn t.tree key)

  let incr_index (t : t) ~is_new =
    let open Or_error.Let_syntax in
    if is_new then
      let%map new_pos =
        if
          Stack_with_state_hash_id.equal t.new_pos
            (Stack_with_state_hash_id.of_int (max_coinbase_stack_count - 1))
        then Ok Stack_with_state_hash_id.zero
        else Stack_with_state_hash_id.incr_by_one t.new_pos
      in
      {t with pos_list= t.new_pos :: t.pos_list; new_pos}
    else Ok t

  let latest_stack_with_state_hash_id (t : t) ~is_new =
    if is_new then Ok t.new_pos
    else
      match List.hd t.pos_list with
      | Some x ->
          Ok x
      | None ->
          Or_error.error_string
            "No Stack_with_state_hash_id for the latest stack"

  let latest_stack_with_state_hash (t : t) ~is_new =
    let open Or_error.Let_syntax in
    let%bind key = latest_stack_with_state_hash_id t ~is_new in
    Or_error.try_with (fun () ->
        let index = Merkle_tree.find_index_exn t.tree key in
        Merkle_tree.get_exn t.tree index )

  let oldest_stack_with_state_hash_id (t : t) = List.last t.pos_list

  let remove_oldest_stack_with_state_hash_id t =
    match List.rev t with
    | [] ->
        Or_error.error_string "No coinbase stack-with-state-hash to pop"
    | x :: xs ->
        Ok (x, List.rev xs)

  let oldest_stack_with_state_hash t =
    let open Or_error.Let_syntax in
    let key =
      Option.value ~default:Stack_with_state_hash_id.zero
        (oldest_stack_with_state_hash_id t)
    in
    let%bind index = find_index t key in
    get_stack_with_state_hash t index

  (* this is the same computation for combining state hashes and state body hashes as
     `Protocol_state.hash_abstract', not available here because it would create 
     a module dependency cycle
   *)
  let update_state_hash ~state_hash ~state_body_hash =
    let open Fold in
    Snark_params.Tick.Pedersen.digest_fold Hash_prefix.protocol_state
      (State_hash.fold state_hash +> State_body_hash.fold state_body_hash)
    |> State_hash.of_hash

  let add_coinbase t ~coinbase ~is_new =
    let open Or_error.Let_syntax in
    let%bind key = latest_stack_with_state_hash_id t ~is_new in
    let%bind stack_index = find_index t key in
    let%bind {stack= stack_before; state_hash= previous_state_hash} =
      get_stack_with_state_hash t stack_index
    in
    let stack_with_state_hash_after =
      let stack = Stack.push stack_before coinbase in
      let state_hash =
        if is_new then t.previous_state_hash
        else
          update_state_hash ~state_hash:previous_state_hash
            ~state_body_hash:coinbase.state_body_hash
      in
      Stack_with_state_hash.Poly.{stack; state_hash}
    in
    let%bind t' = incr_index t ~is_new in
    (* state hash that was paired with the "before" stack becomes previous state hash at top level *)
    set_stack_with_state_hash
      {t' with previous_state_hash}
      stack_index stack_with_state_hash_after

  let update_coinbase_stack_with_state_hash t stack_with_state_hash ~is_new =
    let open Or_error.Let_syntax in
    let%bind key = latest_stack_with_state_hash_id t ~is_new in
    let%bind stack_index = find_index t key in
    let%bind t' = incr_index t ~is_new in
    set_stack_with_state_hash t' stack_index stack_with_state_hash

  let remove_coinbase_stack_with_state_hash (t : t) =
    let open Or_error.Let_syntax in
    let%bind oldest_stack, remaining =
      remove_oldest_stack_with_state_hash_id t.pos_list
    in
    let%bind stack_index = find_index t oldest_stack in
    let%bind stack = get_stack_with_state_hash t stack_index in
    let%map t' =
      set_stack_with_state_hash t stack_index Stack_with_state_hash.empty
    in
    (stack, {t' with pos_list= remaining})

  let hash_extra ({pos_list; new_pos; _} : t) =
    let h = Digestif.SHA256.init () in
    let h =
      Digestif.SHA256.feed_string h
        (List.fold pos_list ~init:"" ~f:(fun s a ->
             s ^ Stack_with_state_hash_id.to_string a ))
    in
    let h =
      Digestif.SHA256.feed_string h
        (Stack_with_state_hash_id.to_string new_pos)
    in
    Digestif.SHA256.(get h |> to_raw_string)

  let handler (t : t) ~is_new =
    let pending_coinbase = ref t in
    let coinbase_stack_path_exn idx =
      List.map
        (path !pending_coinbase idx |> Or_error.ok_exn)
        ~f:(function `Left h -> h | `Right h -> h)
    in
    stage (fun (With {request; respond}) ->
        match request with
        | Checked.Coinbase_stack_with_state_hash_path idx ->
            let path =
              (coinbase_stack_path_exn idx :> Pedersen.Digest.t list)
            in
            respond (Provide path)
        | Checked.Find_index_of_oldest_stack_with_state_hash ->
            let stack_id =
              Option.value ~default:Stack_with_state_hash_id.zero
                (oldest_stack_with_state_hash_id !pending_coinbase)
            in
            let index =
              find_index !pending_coinbase stack_id |> Or_error.ok_exn
            in
            respond (Provide index)
        | Checked.Find_index_of_newest_stack_with_state_hash ->
            let stack_id =
              match
                latest_stack_with_state_hash_id !pending_coinbase ~is_new
              with
              | Ok id ->
                  id
              | _ ->
                  Stack_with_state_hash_id.zero
            in
            let index =
              find_index !pending_coinbase stack_id |> Or_error.ok_exn
            in
            respond (Provide index)
        | Checked.Get_coinbase_stack_with_state_hash idx ->
            let elt =
              get_stack_with_state_hash !pending_coinbase idx
              |> Or_error.ok_exn
            in
            let path =
              (coinbase_stack_path_exn idx :> Pedersen.Digest.t list)
            in
            respond (Provide (elt, path))
        | Checked.Set_coinbase_stack_with_state_hash (idx, stack) ->
            pending_coinbase :=
              set_stack_with_state_hash !pending_coinbase idx stack
              |> Or_error.ok_exn ;
            respond (Provide ())
        | _ ->
            unhandled )
end

include Make (struct
  let depth = Snark_params.pending_coinbase_depth
end)

let%test_unit "add stack + remove stack = initial tree " =
  let pending_coinbases = ref (create () |> Or_error.ok_exn) in
  let coinbases_gen = Quickcheck.Generator.list_non_empty Coinbase.gen in
  Quickcheck.test coinbases_gen ~trials:50 ~f:(fun cbs ->
      Async.Thread_safe.block_on_async_exn (fun () ->
          let is_new = ref true in
          let init = merkle_root !pending_coinbases in
          let after_adding =
            List.fold cbs ~init:!pending_coinbases ~f:(fun acc coinbase ->
                let t =
                  add_coinbase acc ~coinbase ~is_new:!is_new |> Or_error.ok_exn
                in
                is_new := false ;
                t )
          in
          let _, after_del =
            remove_coinbase_stack_with_state_hash after_adding
            |> Or_error.ok_exn
          in
          pending_coinbases := after_del ;
          assert (Hash.equal (merkle_root after_del) init) ;
          Async.Deferred.return () ) )

let%test_unit "Checked_stack = Unchecked_stack" =
  let open Quickcheck in
  test ~trials:20 (Generator.tuple2 Stack.gen Coinbase.gen)
    ~f:(fun (base, cb) ->
      let coinbase_data = Coinbase_data.of_coinbase cb in
      let unchecked = Stack.push base cb in
      let checked =
        let comp =
          let open Snark_params.Tick in
          let cb_var = Coinbase_data.(var_of_t coinbase_data) in
          let%map res = Stack.Checked.push (Stack.var_of_t base) cb_var in
          As_prover.read Stack.typ res
        in
        let (), x = Or_error.ok_exn (run_and_check comp ()) in
        x
      in
      assert (Stack.equal unchecked checked) )

let%test_unit "Checked_tree = Unchecked_tree" =
  let open Quickcheck in
  let pending_coinbases = create () |> Or_error.ok_exn in
  test ~trials:20 Coinbase.gen ~f:(fun coinbase ->
      let max_coinbase_amount = Coda_compile_config.coinbase in
      let coinbase_data = Coinbase_data.of_coinbase coinbase in
      let state_body_hash = random_value State_body_hash.gen in
      let coinbase2 =
        Coinbase.create
          ~amount:
            ( Amount.sub max_coinbase_amount coinbase.amount
            |> Option.value_exn ?here:None ?message:None ?error:None )
          ~proposer:coinbase.proposer ~fee_transfer:None ~state_body_hash
        |> Or_error.ok_exn
      in
      let unchecked =
        if Amount.equal coinbase.amount Amount.zero then pending_coinbases
        else
          let interim_tree =
            add_coinbase pending_coinbases ~coinbase ~is_new:true
            |> Or_error.ok_exn
          in
          if Amount.equal coinbase2.amount Amount.zero then interim_tree
          else
            add_coinbase interim_tree ~coinbase:coinbase2 ~is_new:false
            |> Or_error.ok_exn
      in
      let f_add_coinbase = Checked.add_coinbase in
      let checked =
        let comp =
          let open Snark_params.Tick in
          let pk, _, _ = Coinbase_data.(var_of_t coinbase_data) in
          let%map res =
            handle
              (f_add_coinbase
                 (Hash.var_of_t (merkle_root pending_coinbases))
                 ( pk
                 , Amount.var_of_t coinbase.amount
                 , State_body_hash.(var_of_t dummy) ))
              (unstage (handler pending_coinbases ~is_new:true))
          in
          As_prover.read Hash.typ res
        in
        let (), x = Or_error.ok_exn (run_and_check comp ()) in
        x
      in
      assert (Hash.equal (merkle_root unchecked) checked) )

let%test_unit "push and pop multiple stacks" =
  let open Quickcheck in
  let max_coinbase_amount = Coda_compile_config.coinbase in
  let module Pending_coinbase = Make (struct
    let depth = 3
  end) in
  let t_of_coinbases t = function
    | [] ->
        let t' =
          Pending_coinbase.incr_index t ~is_new:true |> Or_error.ok_exn
        in
        (Pending_coinbase.Stack.empty, t')
    | initial_coinbase :: coinbases ->
        let t' =
          Pending_coinbase.add_coinbase t ~coinbase:initial_coinbase
            ~is_new:true
          |> Or_error.ok_exn
        in
        let updated =
          List.fold coinbases ~init:t'
            ~f:(fun pending_coinbases (coinbase : Coinbase.t) ->
              let coinbase2 =
                Coinbase.create
                  ~amount:
                    ( Amount.sub max_coinbase_amount coinbase.amount
                    |> Option.value_exn ?here:None ?message:None ?error:None )
                  ~proposer:coinbase.proposer
                  ~fee_transfer:None (* copy hash, value not important here *)
                  ~state_body_hash:coinbase.state_body_hash
                |> Or_error.ok_exn
              in
              if Amount.equal coinbase.amount Amount.zero then
                pending_coinbases
              else
                let interim_tree =
                  Pending_coinbase.add_coinbase pending_coinbases ~coinbase
                    ~is_new:false
                  |> Or_error.ok_exn
                in
                if Amount.equal coinbase2.amount Amount.zero then interim_tree
                else
                  Pending_coinbase.add_coinbase interim_tree
                    ~coinbase:coinbase2 ~is_new:false
                  |> Or_error.ok_exn )
        in
        let new_stack_with_state_hash =
          Or_error.ok_exn
          @@ Pending_coinbase.latest_stack_with_state_hash updated
               ~is_new:false
        in
        (new_stack_with_state_hash.stack, updated)
  in
  (*Create pending coinbase stacks from coinbase lists and add it to the pending coinbase merkle tree*)
  let add coinbase_lists pending_coinbases =
    List.fold ~init:([], pending_coinbases) coinbase_lists
      ~f:(fun (stacks, pc) coinbases ->
        let new_stack, pc = t_of_coinbases pc coinbases in
        (new_stack :: stacks, pc) )
  in
  (*remove the oldest stack and check if that's the expected one *)
  let remove_check t expected_stack =
    let popped_stack_with_state_hash, updated_pending_coinbases =
      Pending_coinbase.remove_coinbase_stack_with_state_hash t
      |> Or_error.ok_exn
    in
    assert (
      Pending_coinbase.Stack_with_state_hash.equal_stacks
        popped_stack_with_state_hash expected_stack ) ;
    updated_pending_coinbases
  in
  let add_remove_check coinbase_lists =
    let pending_coinbases = Pending_coinbase.create_exn' () in
    let rec go coinbase_lists pc =
      if List.is_empty coinbase_lists then ()
      else
        let coinbase_lists' =
          List.take coinbase_lists Pending_coinbase.max_coinbase_stack_count
        in
        let added_stacks, pending_coinbases_updated = add coinbase_lists' pc in
        let pending_coinbases' =
          List.fold ~init:pending_coinbases_updated (List.rev added_stacks)
            ~f:(fun pc expected_stack ->
              remove_check pc
                {stack= expected_stack; state_hash= State_hash.dummy} )
        in
        let remaining_lists =
          List.drop coinbase_lists Pending_coinbase.max_coinbase_stack_count
        in
        go remaining_lists pending_coinbases'
    in
    go coinbase_lists pending_coinbases
  in
  let coinbase_lists_gen = Quickcheck.Generator.(list (list Coinbase.gen)) in
  test ~trials:100 coinbase_lists_gen ~f:add_remove_check
