open Core
open Sodium

module BytesWr = struct
  include Bytes

  module Base58_check = Base58_check.Make (struct
    let version_byte = Base58_check.Version_bytes.secret_box_byteswr
  end)

  let to_yojson t = `String (Bytes.to_string t |> Base58_check.encode)

  let of_yojson = function
    | `String s ->
        Ok (Base58_check.decode_exn s |> Bytes.of_string)
    | _ ->
        Error "Bytes.of_yojson needs a string"
end

module Stable = struct
  module V1 = struct
    type t =
      { box_primitive: string
      ; pw_primitive: string
      ; nonce: Bytes.t
      ; pwsalt: Bytes.t
      ; pwdiff: Int64.t * int
      ; ciphertext: Bytes.t }
    [@@deriving sexp]
  end
end

module Json : sig
  type t [@@deriving yojson]

  val of_stable : Stable.V1.t -> t

  val to_stable : t -> Stable.V1.t
end = struct
  type t =
    { box_primitive: string
    ; pw_primitive: string
    ; nonce: BytesWr.t
    ; pwsalt: BytesWr.t
    ; pwdiff: Int64.t * int
    ; ciphertext: BytesWr.t }
  [@@deriving yojson]

  let of_stable
      {Stable.V1.box_primitive; pw_primitive; nonce; pwsalt; pwdiff; ciphertext}
      =
    {box_primitive; pw_primitive; nonce; pwsalt; pwdiff; ciphertext}

  let to_stable {box_primitive; pw_primitive; nonce; pwsalt; pwdiff; ciphertext}
      =
    {Stable.V1.box_primitive; pw_primitive; nonce; pwsalt; pwdiff; ciphertext}
end

include Stable.V1

let to_yojson t : Yojson.Safe.json = Json.to_yojson (Json.of_stable t)

let of_yojson (t : Yojson.Safe.json) =
  Result.map ~f:Json.to_stable (Json.of_yojson t)

(** warning: this will zero [password] *)
let encrypt ~(password : Bytes.t) ~(plaintext : Bytes.t) =
  let nonce = Secret_box.random_nonce () in
  let salt = Password_hash.random_salt () in
  let ({Password_hash.mem_limit; ops_limit} as diff) =
    Password_hash.moderate
  in
  let pw = Password_hash.Bytes.wipe_to_password password in
  let key = Secret_box.derive_key diff pw salt in
  let ciphertext = Secret_box.Bytes.secret_box key plaintext nonce in
  { box_primitive= Secret_box.primitive
  ; pw_primitive= Password_hash.primitive
  ; nonce= Secret_box.Bytes.of_nonce nonce
  ; pwsalt= Password_hash.Bytes.of_salt salt
  ; pwdiff= (mem_limit, ops_limit)
  ; ciphertext }

(** warning: this will zero [password] *)
let decrypt ~(password : Bytes.t)
    { box_primitive
    ; pw_primitive
    ; nonce
    ; pwsalt
    ; pwdiff= mem_limit, ops_limit
    ; ciphertext } =
  if box_primitive <> Secret_box.primitive then
    Or_error.errorf "don't know how to handle a %s secret_box" box_primitive
  else if pw_primitive <> Password_hash.primitive then
    Or_error.errorf "don't know how to handle a %s password_hash" pw_primitive
  else
    let nonce = Secret_box.Bytes.to_nonce nonce in
    let salt = Password_hash.Bytes.to_salt pwsalt in
    let diff = {Password_hash.mem_limit; ops_limit} in
    let pw = Password_hash.Bytes.wipe_to_password password in
    let key = Secret_box.derive_key diff pw salt in
    try
      Or_error.return @@ Secret_box.Bytes.secret_box_open key ciphertext nonce
    with Sodium.Verification_failure ->
      Or_error.error_string "password is wrong or ciphertext is corrupt"

let%test_unit "successful roundtrip" =
  (* 4 trials because password hashing is slow *)
  let bgen = Bytes.gen_with_length 16 Char.quickcheck_generator in
  Quickcheck.test
    Quickcheck.Generator.(tuple2 bgen bgen)
    ~trials:4
    ~f:(fun (password, plaintext) ->
      let enc = encrypt ~password:(Bytes.copy password) ~plaintext in
      let dec = decrypt enc ~password |> Or_error.ok_exn in
      [%test_eq: Bytes.t] dec plaintext )

let%test "bad password fails" =
  let enc =
    encrypt ~password:(Bytes.of_string "foobar")
      ~plaintext:(Bytes.of_string "yo")
  in
  Or_error.is_error (decrypt ~password:(Bytes.of_string "barfoo") enc)
