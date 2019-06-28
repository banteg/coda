open Signature_lib
open Core
open Async
open Async.Deferred.Let_syntax

let rec prompt_password prompt =
  let open Deferred.Or_error.Let_syntax in
  let%bind pw1 = Password.read prompt in
  let%bind pw2 = Password.read "Again to confirm: " in
  if not (Bytes.equal pw1 pw2) then (
    eprintf "Error: passwords don't match, try again\n" ;
    prompt_password prompt )
  else return pw2

(** Writes a keypair to [privkey_path] and [privkey_path ^ ".pub"] using [Secret_file] *)
let write_exn {Keypair.private_key; public_key} ~(privkey_path : string)
    ~(password : Secret_file.password) : unit Deferred.t =
  let privkey_bytes =
    Private_key.to_bigstring private_key |> Bigstring.to_bytes
  in
  let pubkey_string =
    Public_key.Compressed.to_base58_check (Public_key.compress public_key)
  in
  match%bind
    Secret_file.write ~path:privkey_path ~mkdir:true ~plaintext:privkey_bytes
      ~password
  with
  | Ok () ->
      (* The hope is that if [Secret_file.write] succeeded then this ought to
       as well, letting [handle_open] stay inside [Secret_file]. It might not
       if the environment changes underneath us, and we won't have nice errors
       in that case. *)
      let%bind pubkey_f = Writer.open_file (privkey_path ^ ".pub") in
      Writer.write_line pubkey_f pubkey_string ;
      Writer.close pubkey_f
  | Error e ->
      raise (Error.to_exn e)

(** Reads a private key from [privkey_path] using [Secret_file] *)
let read ~(privkey_path : string) ~(password : Secret_file.password) :
    Keypair.t Deferred.Or_error.t =
  let open Deferred.Or_error.Let_syntax in
  let%bind pk_bytes = Secret_file.read ~path:privkey_path ~password in
  let open Or_error.Let_syntax in
  Deferred.return
  @@ let%bind sk =
       try
         return (pk_bytes |> Bigstring.of_bytes |> Private_key.of_bigstring_exn)
       with exn ->
         Or_error.errorf
           "Error parsing decrypted private key file, is your keyfile \
            corrupt? %s"
           (Exn.to_string exn)
     in
     try return (Keypair.of_private_key_exn sk)
     with exn ->
       Or_error.errorf
         "Error computing public key from private, is your keyfile corrupt? %s"
         (Exn.to_string exn)

(** Reads a private key from [privkey_path] using [Secret_file], throws on failure *)
let read_exn ~(privkey_path : string) ~(password : Secret_file.password) :
    Keypair.t Deferred.t =
  match%map read ~privkey_path ~password with
  | Ok keypair ->
      keypair
  | Error e ->
      raise (Error.to_exn e)

let read_exn' path =
  read_exn ~privkey_path:path
    ~password:(lazy (Password.read "Secret key password: "))

module Terminal_stdin = struct
  let read_exn path =
    read_exn ~privkey_path:path
      ~password:(lazy (Password.read "Secret key password: "))

  let write_exn kp ~privkey_path =
    write_exn kp ~privkey_path
      ~password:(lazy (prompt_password "Password for new private key file: "))
end
