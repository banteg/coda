(* versioned.ml -- static enforcement of versioned types via ppx

   1) check that versioned type always in valid module hierarchy
   2) versioned types depend only on other versioned types or OCaml built-in types

  to use, add coda_ppx to the dune pps list, and annotate a type declaration with
  either

    [@@deriving version]

  or

    [@@deriving version { option }]

  where option is one of "wrapped", "unnumbered", "rpc", or "asserted".

  Without options (the common case), the type must be named "t", and its definition
  occurs in the module hierarchy "Stable.Vn.T", where n is a positive integer.

  If "wrapped" is true, again, the type must be named "t", but the type
  definition occurs in the hierarchy "Wrapped.Stable.Vn", where n is a positive
  integer. TODO: Anything to say about registration, translation to a latest
  version for wrapped types?

  The "unnumbered" option prevents the generation of the value

    let version = n

  to prevent warnings or errors about an unused value. That's useful for versioned types in
  modules that aren't registered. For types with type parameters, the version number is
  not generated, so the "unnumbered" option is not needed.

  The "asserted" option asserts that the type is versioned, to allow compilation
  to proceed. The types referred to in the type are not checked for versioning
  with this option. The type must be contained in the module hierarchy "Stable.Vn.T".
  Eventually, all uses of this option should be removed.

  The "for_test" option implies "asserted" and "unnumbered", for use in test code.

  If "rpc" is true, again, the type must be named "query", "response", or "msg",
  and the type definition occurs in the hierarchy "Vn.T".

  All these options are available for types within structures.

  Within signatures, the declaration

    val __versioned__ : bool

  is generated. If the "numbered" option is given, then

    val version : int

  is also generated. This option should be needed only by the internal versioning
  machinery, and not in ordinary code. No other options are available within signatures.

*)

open Core_kernel
open Ppxlib

let deriver = "version"

type generation_kind = Plain | Wrapped | Rpc

let validate_module_version module_version loc =
  let len = String.length module_version in
  if not (Char.equal module_version.[0] 'V' && len > 1) then
    Location.raise_errorf ~loc
      "Versioning module containing versioned type must be named Vn, for some \
       number n"
  else
    let numeric_part = String.sub module_version ~pos:1 ~len:(len - 1) in
    String.iter numeric_part ~f:(fun c ->
        if not (Char.is_digit c) then
          Location.raise_errorf ~loc
            "Versioning module name must be Vn, for some number n, got: \"%s\""
            module_version ) ;
    (* invariant: 0th char is digit *)
    if Int.equal (Char.get_digit_exn numeric_part.[0]) 0 then
      Location.raise_errorf ~loc
        "Versioning module name must be Vn, for a number n, which cannot \
         begin with 0, got: \"%s\""
        module_version

let validate_rpc_type_decl inner3_modules type_decl =
  match List.take inner3_modules 2 with
  | ["T"; module_version] ->
      validate_module_version module_version type_decl.ptype_loc
  | _ ->
      Location.raise_errorf ~loc:type_decl.ptype_loc
        "Versioned RPC type must be contained in module path Vn.T, for some \
         number n"

let validate_plain_type_decl inner3_modules type_decl =
  match inner3_modules with
  | ["T"; module_version; "Stable"] ->
      validate_module_version module_version type_decl.ptype_loc
  | _ ->
      Location.raise_errorf ~loc:type_decl.ptype_loc
        "Versioned type must be contained in module path Stable.Vn.T, for \
         some number n"

let validate_wrapped_type_decl inner3_modules type_decl =
  match inner3_modules with
  | [module_version; "Stable"; "Wrapped"] ->
      validate_module_version module_version type_decl.ptype_loc
  | _ ->
      Location.raise_errorf ~loc:type_decl.ptype_loc
        "Wrapped versioned type must be contained in module path \
         Wrapped.Stable.Vn, for some number n"

(* check that a versioned type occurs in valid module hierarchy and is named "t"
   (for RPC types, the name can be "query", "response", or "msg")
 *)
let validate_type_decl inner3_modules generation_kind type_decl =
  let name = type_decl.ptype_name.txt in
  let loc = type_decl.ptype_name.loc in
  match generation_kind with
  | Wrapped ->
      let valid_name = "t" in
      if not (String.equal name valid_name) then
        Location.raise_errorf ~loc
          "Wrapped versioned type must be named \"%s\", got: \"%s\"" valid_name
          name ;
      validate_wrapped_type_decl inner3_modules type_decl
  | Rpc ->
      let rpc_valid_names = ["query"; "response"; "msg"] in
      if
        List.find rpc_valid_names ~f:(fun ty -> String.equal ty name)
        |> Option.is_none
      then
        Location.raise_errorf ~loc
          "RPC versioned type must be named one of \"%s\", got: \"%s\""
          (String.concat ~sep:"," rpc_valid_names)
          name ;
      validate_rpc_type_decl inner3_modules type_decl
  | Plain ->
      let valid_name = "t" in
      if not (String.equal name valid_name) then
        Location.raise_errorf ~loc
          "Versioned type must be named \"%s\", got: \"%s\"" valid_name name ;
      validate_plain_type_decl inner3_modules type_decl

let module_name_from_plain_path inner3_modules =
  match inner3_modules with
  | ["T"; module_version; "Stable"] ->
      module_version
  | _ ->
      failwith "module_name_from_plain_path: unexpected module path"

let module_name_from_wrapped_path inner3_modules =
  match inner3_modules with
  | [module_version; "Stable"; "Wrapped"] ->
      module_version
  | _ ->
      failwith "module_name_from_wrapped_path: unexpected module path"

let module_name_from_rpc_path inner3_modules =
  match List.take inner3_modules 2 with
  | ["T"; module_version] ->
      module_version
  | _ ->
      failwith "module_name_from_rpc_path: unexpected module path"

(* generate "let version = n", when version module is Vn *)
let generate_version_number_decl inner3_modules loc generation_kind =
  (* invariant: we've checked module name already *)
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = loc
  end) in
  let open E in
  let module_name =
    match generation_kind with
    | Plain ->
        module_name_from_plain_path inner3_modules
    | Wrapped ->
        module_name_from_wrapped_path inner3_modules
    | Rpc ->
        module_name_from_rpc_path inner3_modules
  in
  let version =
    String.sub module_name ~pos:1 ~len:(String.length module_name - 1)
    |> int_of_string
  in
  [%stri let version = [%e eint version]]

let ocaml_builtin_types =
  ["bytes"; "int"; "int32"; "int64"; "float"; "char"; "string"; "bool"; "unit"]

let ocaml_builtin_type_constructors = ["list"; "array"; "option"; "ref"]

let jane_street_type_constructors = ["sexp_opaque"]

(* true iff module_path is of form M. ... .Stable.Vn, where M is Core or Core_kernel, and n is integer *)
let is_jane_street_stable_module module_path =
  let hd_elt = List.hd_exn module_path in
  let jane_street_libs = ["Core_kernel"; "Core"] in
  let is_version_module vn =
    let len = String.length vn in
    len > 1
    && Char.equal vn.[0] 'V'
    &&
    let numeric_part = String.sub vn ~pos:1 ~len:(len - 1) in
    String.for_all numeric_part ~f:Char.is_digit
    && not (Int.equal (Char.get_digit_exn numeric_part.[0]) 0)
  in
  List.mem jane_street_libs hd_elt ~equal:String.equal
  &&
  match List.rev module_path with
  | vn :: "Stable" :: _ ->
      is_version_module vn
  | vn :: "Span" :: "Stable" :: "Time" :: _ ->
      (* special case, maybe improper module structure *)
      is_version_module vn
  | _ ->
      false

let whitelisted_prefix prefix ~loc =
  match prefix with
  | Lident id ->
      String.equal id "Bitstring"
  | Ldot _ ->
      let module_path = Longident.flatten_exn prefix in
      is_jane_street_stable_module module_path
  | Lapply _ ->
      Ppx_deriving.raise_errorf ~loc
        "Type name contains unexpected application"

let rec generate_core_type_version_decls type_name core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({txt; _}, core_types) -> (
    match txt with
    | Lident id ->
        (* type t = id *)
        if String.equal id type_name (* recursion *) then []
        else if
          List.is_empty core_types
          && List.mem ocaml_builtin_types id ~equal:String.equal
        then (* no versioning to worry about *)
          []
        else if
          List.mem ocaml_builtin_type_constructors id ~equal:String.equal
          || List.mem jane_street_type_constructors id ~equal:String.equal
        then
          match core_types with
          | [_] ->
              generate_version_lets_for_core_types type_name core_types
          | _ ->
              Ppx_deriving.raise_errorf ~loc:core_type.ptyp_loc
                "Type constructor \"%s\" expects one type argument, got %d" id
                (List.length core_types)
        else
          Ppx_deriving.raise_errorf ~loc:core_type.ptyp_loc
            "\"%s\" is neither an OCaml type constructor nor a versioned type"
            id
    | Ldot (prefix, "t") ->
        (* type t = A.B.t
           if prefix not whitelisted, generate: let _ = A.B.__versioned__
        *)
        let core_type_decls =
          generate_version_lets_for_core_types type_name core_types
        in
        if whitelisted_prefix prefix ~loc:core_type.ptyp_loc then
          core_type_decls
        else
          let loc = core_type.ptyp_loc in
          let pexp_loc = loc in
          let versioned_ident =
            { pexp_desc= Pexp_ident {txt= Ldot (prefix, "__versioned__"); loc}
            ; pexp_loc
            ; pexp_attributes= [] }
          in
          [%str let _ = [%e versioned_ident]] @ core_type_decls
    | _ ->
        Ppx_deriving.raise_errorf ~loc:core_type.ptyp_loc
          "Unrecognized type constructor for versioned type" )
  | Ptyp_tuple core_types ->
      (* type t = t1 * t2 * t3 *)
      generate_version_lets_for_core_types type_name core_types
  | Ptyp_variant _ ->
      (* type t = [ `A | `B ] *)
      []
  | Ptyp_var _ ->
      (* type variable *)
      []
  | _ ->
      Ppx_deriving.raise_errorf ~loc:core_type.ptyp_loc
        "Can't determine versioning for contained type"

and generate_version_lets_for_core_types type_name core_types =
  List.fold_right core_types ~init:[] ~f:(fun core_type accum ->
      generate_core_type_version_decls type_name core_type @ accum )

let generate_version_lets_for_label_decls type_name label_decls =
  generate_version_lets_for_core_types type_name
    (List.map label_decls ~f:(fun lab_decl -> lab_decl.pld_type))

let generate_constructor_decl_decls type_name ctor_decl =
  match (ctor_decl.pcd_res, ctor_decl.pcd_args) with
  | None, Pcstr_tuple core_types ->
      (* C of T1 * ... * Tn *)
      generate_version_lets_for_core_types type_name core_types
  | None, Pcstr_record label_decls ->
      (* C of { ... } *)
      generate_version_lets_for_label_decls type_name label_decls
  | _ ->
      Ppx_deriving.raise_errorf ~loc:ctor_decl.pcd_loc
        "Can't determine versioning for constructor declaration"

let generate_contained_type_decls type_decl =
  let type_name = type_decl.ptype_name.txt in
  match type_decl.ptype_kind with
  | Ptype_abstract ->
      if Option.is_none type_decl.ptype_manifest then
        Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
          "Versioned type, not a label or variant, must have manifest \
           (right-hand side)" ;
      let manifest = Option.value_exn type_decl.ptype_manifest in
      generate_core_type_version_decls type_name manifest
  | Ptype_variant ctor_decls ->
      List.fold ctor_decls ~init:[] ~f:(fun accum ctor_decl ->
          generate_constructor_decl_decls type_name ctor_decl @ accum )
  | Ptype_record label_decls ->
      generate_version_lets_for_label_decls type_name label_decls
  | Ptype_open ->
      Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
        "Versioned type may not be open"

let generate_versioned_decls ~asserted generation_kind type_decl =
  let module E = Ppxlib.Ast_builder.Make (struct
    let loc = type_decl.ptype_loc
  end) in
  let open E in
  let versioned_current = [%stri let __versioned__ = true] in
  if asserted then [versioned_current]
  else
    match generation_kind with
    | Wrapped ->
        (* don't check whether contained types are versioned *)
        [versioned_current]
    | Rpc ->
        (* check whether contained types are versioned,
        but don't assert versioned-ness of this type *)
        generate_contained_type_decls type_decl
    | Plain ->
        (* check contained types, assert this type is versioned *)
        versioned_current :: generate_contained_type_decls type_decl

let get_type_decl_representative type_decls =
  let type_decl1 = List.hd_exn type_decls in
  let type_decl2 = List.hd_exn (List.rev type_decls) in
  ( if not (Int.equal (List.length type_decls) 1) then
    let loc =
      { loc_start= type_decl1.ptype_loc.loc_start
      ; loc_end= type_decl2.ptype_loc.loc_end
      ; loc_ghost= true }
    in
    Ppx_deriving.raise_errorf ~loc
      "Versioned type must be just one type \"t\", not a sequence of types" ) ;
  type_decl1

let check_for_option s options =
  let is_s_opt opt =
    match opt with
    | str1, {pexp_desc= Pexp_ident {txt= Lident str2; _}; _} ->
        String.equal s str1 && String.equal s str2
    | _ ->
        false
  in
  List.find options ~f:is_s_opt |> Option.is_some

let validate_options valid options =
  let get_option_name (str, _) = str in
  let is_valid opt =
    get_option_name opt |> List.mem valid ~equal:String.equal
  in
  if not (List.for_all options ~f:is_valid) then
    let exprs = List.map options ~f:snd in
    let {pexp_loc= loc1; _} = List.hd_exn exprs in
    let {pexp_loc= loc2; _} = List.hd_exn (List.rev exprs) in
    let loc =
      {loc_start= loc1.loc_start; loc_end= loc2.loc_end; loc_ghost= true}
    in
    Ppx_deriving.raise_errorf ~loc "Valid options to \"version\" are: %s"
      (String.concat ~sep:"," valid)

let generate_let_bindings_for_type_decl_str ~options ~path type_decls :
    Ppxlib.Ast.structure =
  ignore
    (validate_options
       ["wrapped"; "unnumbered"; "rpc"; "asserted"; "for_test"]
       options) ;
  let type_decl = get_type_decl_representative type_decls in
  let wrapped = check_for_option "wrapped" options in
  let unnumbered =
    check_for_option "unnumbered" options
    || check_for_option "for_test" options
  in
  let asserted =
    check_for_option "asserted" options || check_for_option "for_test" options
  in
  let rpc = check_for_option "rpc" options in
  if asserted && (wrapped || rpc) then
    Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
      "Options \"asserted\" or \"for_test\" cannot be combined with \
       \"wrapped\" or \"rpc\" options" ;
  let generation_kind =
    match (rpc, wrapped) with
    | true, false ->
        Rpc
    | false, true ->
        Wrapped
    | false, false ->
        Plain
    | true, true ->
        Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
          "RPC versioned type cannot also be wrapped"
  in
  let inner3_modules = List.take (List.rev path) 3 in
  validate_type_decl inner3_modules generation_kind type_decl ;
  let versioned_decls =
    generate_versioned_decls ~asserted generation_kind type_decl
  in
  let type_name = type_decl.ptype_name.txt in
  let has_type_params = not (List.is_empty type_decl.ptype_params) in
  (* generate version number for Rpc response, but not for query, so we
     don't get an unused value
   *)
  if
    unnumbered || has_type_params
    || (generation_kind = Rpc && String.equal type_name "query")
  then versioned_decls
  else
    generate_version_number_decl inner3_modules type_decl.ptype_loc
      generation_kind
    :: versioned_decls

let generate_val_decls_for_type_decl type_decl ~numbered =
  match type_decl.ptype_kind with
  (* the structure of the type doesn't affect what we generate for signatures *)
  | Ptype_abstract | Ptype_variant _ | Ptype_record _ ->
      let loc = type_decl.ptype_loc in
      let versioned = [%sigi: val __versioned__ : bool] in
      if numbered then [[%sigi: val version : int]; versioned] else [versioned]
  | Ptype_open ->
      (* but the type can't be open, else it might vary over time *)
      Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
        "Versioned type in a signature must not be open"

let generate_val_decls_for_type_decl_sig ~options ~path:_ type_decls =
  (* in a signature, the module path may vary *)
  ignore (validate_options ["numbered"] options) ;
  let type_decl = get_type_decl_representative type_decls in
  let numbered = check_for_option "numbered" options in
  generate_val_decls_for_type_decl type_decl ~numbered

let () =
  Ppx_deriving.(
    register
      (create deriver ~type_decl_str:generate_let_bindings_for_type_decl_str
         ~type_decl_sig:generate_val_decls_for_type_decl_sig ()))
