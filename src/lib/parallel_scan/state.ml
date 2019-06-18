open Core_kernel
open Coda_digestif

module Job = struct
  module Sequence_no = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type t = int [@@deriving sexp, bin_io, version]
        end

        include T
      end

      module Latest = V1
    end

    type t = Stable.Latest.t [@@deriving sexp]
  end

  module Merge = struct
    module Stable = struct
      module V1 = struct
        module T = struct
          type 'a t =
            | Empty
            | Lcomp of 'a
            | Rcomp of 'a
            | Bcomp of ('a * 'a * Sequence_no.Stable.V1.t)
          [@@deriving sexp, bin_io, version]
        end

        include T
      end

      module Latest = V1
    end
  end

  module Stable = struct
    module V1 = struct
      (*A merge can have zero components, one component (either the left or the right), or two components in which case there is an integer (sequence_no) representing a set of (completed)jobs in a sequence of (completed)jobs created*)

      module T = struct
        type ('a, 'd) t =
          | Merge of 'a Merge.Stable.V1.t
          | Base of ('d * Sequence_no.Stable.V1.t) option
        [@@deriving sexp, bin_io, version]
      end

      include T
    end

    module Latest = V1
  end

  type ('a, 'd) t = ('a, 'd) Stable.Latest.t =
    | Merge of 'a Merge.Stable.V1.t
    | Base of ('d * Sequence_no.Stable.V1.t) option
  [@@deriving sexp]

  let gen a_gen d_gen =
    let open Quickcheck.Generator in
    let open Quickcheck.Generator.Let_syntax in
    match%map
      variant2
        (variant4 Bool.quickcheck_generator a_gen a_gen
           (tuple3 a_gen a_gen Int.quickcheck_generator))
        (Option.quickcheck_generator (tuple2 d_gen Int.quickcheck_generator))
    with
    | `A (`A _) ->
        Merge Empty
    | `A (`B a) ->
        Merge (Lcomp a)
    | `A (`C a) ->
        Merge (Rcomp a)
    | `A (`D a) ->
        Merge (Bcomp a)
    | `B d ->
        Base d

  let gen_full a_gen d_gen =
    let open Quickcheck.Generator in
    let open Quickcheck.Generator.Let_syntax in
    match%map
      variant2
        (tuple3 a_gen a_gen Int.quickcheck_generator)
        (tuple2 d_gen Int.quickcheck_generator)
    with
    | `A (a1, a2, o) ->
        Merge (Bcomp (a1, a2, o))
    | `B (d, o) ->
        Base (Some (d, o))
end

module Completed_job = struct
  module Stable = struct
    module V1 = struct
      (* don't use version number and module registration here, because of type parameter *)
      type 'a t = Lifted of 'a | Merged of 'a [@@deriving bin_io, sexp]
    end

    module Latest = V1
  end

  include Stable.Latest
end

module Stable = struct
  module V1 = struct
    (* don't use module registration here, because of type parameters *)
    module T = struct
      type ('a, 'd) t =
        { jobs: ('a, 'd) Job.Stable.V1.t Ring_buffer.Stable.V1.t
        ; level_pointer: int array
        ; capacity: int
        ; mutable acc: int * ('a * 'd list) option sexp_opaque
        ; mutable current_data_length: int
        ; mutable base_none_pos: int option
        ; mutable recent_tree_data: 'd list sexp_opaque
        ; mutable other_trees_data: 'd list list sexp_opaque
        ; stateful_work_order: int Core.Queue.Stable.V1.t
        ; mutable curr_job_seq_no: int
        ; root_at_depth: int }
      [@@deriving sexp, bin_io, version]
    end

    include T

    let eq_buf buf1 buf2 =
      let open Bigarray.Array1 in
      let len1 = dim buf1 in
      let len2 = dim buf2 in
      assert (Int.equal len1 len2) ;
      let eq = ref true in
      let ndx = ref 0 in
      while !eq && !ndx < len1 do
        if not (Char.equal (get buf1 !ndx) (get buf2 !ndx)) then (
          Stdlib.Printf.eprintf "LEN1: %d LEN2: %d NDX: %d\n%!" len1 len2 !ndx ;
          eq := false ) ;
        incr ndx
      done ;
      !eq

    let bin_write_t wa wd buf ~pos t =
      let open Bigarray in
      let wa buf ~pos a =
        let result = wa buf ~pos a in
        let s =
          String.init (result - pos) ~f:(fun i -> Array1.get buf (pos + i))
        in
        let rec loop n =
          if n >= 0 then (
            let result' = wa buf ~pos a in
            assert (result = result') ;
            let s' =
              String.init (result' - pos) ~f:(fun i -> Array1.get buf (pos + i))
            in
            if not (String.equal s s') then (
              eprintf "WA FAILED ON ITER: %d\n%!" n ;
              assert false ) ;
            loop (n - 1) )
        in
        loop 50 ; result
      in
      let wd buf ~pos a =
        let result = wd buf ~pos a in
        let s =
          String.init (result - pos) ~f:(fun i -> Array1.get buf (pos + i))
        in
        let rec loop n =
          if n >= 0 then (
            let result' = wd buf ~pos a in
            assert (result = result') ;
            let s' =
              String.init (result' - pos) ~f:(fun i -> Array1.get buf (pos + i))
            in
            if not (String.equal s s') then (
              eprintf "WD FAILED ON ITER: %d\n%!" n ;
              assert false ) ;
            loop (n - 1) )
        in
        loop 50 ; result
      in
      bin_write_t wa wd buf ~pos t

    let bin_read_t ra rd buf ~pos_ref = bin_read_t ra rd buf ~pos_ref
  end

  module Latest = V1
end

(* bin_io omitted from deriving list intentionally *)
type ('a, 'd) t = ('a, 'd) Stable.Latest.t =
  { jobs: ('a, 'd) Job.t Ring_buffer.t
  ; level_pointer: int array
  ; capacity: int
  ; mutable acc: int * ('a * 'd list) option
  ; mutable current_data_length: int
  ; mutable base_none_pos: int option
  ; mutable recent_tree_data: 'd list sexp_opaque
  ; mutable other_trees_data: 'd list list sexp_opaque
  ; stateful_work_order: int Queue.t
  ; mutable curr_job_seq_no: int
  ; root_at_depth: int }
[@@deriving sexp]

module Hash = struct
  type t = Digestif.SHA256.t
end

(* TODO: This should really be computed iteratively *)
let hash
    { jobs
    ; acc
    ; current_data_length
    ; base_none_pos
    ; capacity
    ; level_pointer
    ; curr_job_seq_no
    ; root_at_depth
    ; _ } a_to_string d_to_string =
  let h = ref (Digestif.SHA256.init ()) in
  let add_string s = h := Digestif.SHA256.feed_string !h s in
  Ring_buffer.iter jobs ~f:(function
    | Base None ->
        add_string "Base None"
    | Base (Some (x, o)) ->
        add_string ("Base Some " ^ d_to_string x ^ " " ^ Int.to_string o)
    | Merge Empty ->
        add_string "Merge Empty"
    | Merge (Rcomp a) ->
        add_string ("Merge Rcomp " ^ a_to_string a)
    | Merge (Lcomp a) ->
        add_string ("Merge Lcomp " ^ a_to_string a)
    | Merge (Bcomp (a1, a2, o)) ->
        add_string
          ( "Merge Bcomp " ^ a_to_string a1 ^ " " ^ a_to_string a2 ^ " "
          ^ Int.to_string o ) ) ;
  let i, a = acc in
  let x = base_none_pos in
  add_string (Int.to_string capacity) ;
  add_string (Int.to_string i) ;
  add_string
    (Array.fold level_pointer ~init:"" ~f:(fun s a -> s ^ Int.to_string a)) ;
  ( match a with
  | None ->
      add_string "None"
  | Some (a, _) ->
      add_string (a_to_string a) ) ;
  add_string (Int.to_string current_data_length) ;
  ( match x with
  | None ->
      add_string "None"
  | Some a ->
      add_string (Int.to_string a) ) ;
  add_string (Int.to_string curr_job_seq_no) ;
  add_string (Int.to_string root_at_depth) ;
  Digestif.SHA256.get !h

let acc s = snd s.acc

let jobs s = s.jobs

let level_pointer s = s.level_pointer

let current_data_length s = s.current_data_length

let parallelism s = (Ring_buffer.length s.jobs + 1) / 2

let base_none_pos s = s.base_none_pos

let recent_tree_data s = s.recent_tree_data

let other_trees_data s = s.other_trees_data

let stateful_work_order s = s.stateful_work_order

let curr_job_seq_no s = s.curr_job_seq_no

let root_at_depth s = s.root_at_depth

let copy
    { jobs
    ; acc
    ; current_data_length
    ; base_none_pos
    ; capacity
    ; level_pointer
    ; recent_tree_data
    ; other_trees_data
    ; stateful_work_order
    ; curr_job_seq_no
    ; root_at_depth } =
  { jobs= Ring_buffer.copy jobs
  ; acc
  ; capacity
  ; current_data_length
  ; base_none_pos
  ; level_pointer= Array.copy level_pointer
  ; recent_tree_data
  ; other_trees_data
  ; stateful_work_order= Queue.copy stateful_work_order
  ; curr_job_seq_no
  ; root_at_depth }
