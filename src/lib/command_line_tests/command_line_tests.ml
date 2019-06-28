open Core
open Async

(**
 * This module tests the basic funtionalities of coda through the cli
 *)

let%test_module "Command line tests" =
  ( module struct
    let stop port =
      Process.run () ~prog:"dune"
        ~args:
          [ "exec"
          ; "coda"
          ; "client"
          ; "stop-daemon"
          ; "--"
          ; "-daemon-port"
          ; sprintf "%d" port ]

    let test_background_daemon () =
      let log_dir = "/tmp/coda-spun-test" in
      let%bind _ = Process.run_exn ~prog:"rm" ~args:["-rf"; log_dir] () in
      let%bind _ = Process.run_exn ~prog:"mkdir" ~args:["-p"; log_dir] () in
      let open Deferred.Let_syntax in
      let port = 1337 in
      Core.Printf.eprintf "STARTING DAEMON\n%!" ;
      let%bind result0 =
        Process.run_exn ~prog:"dune"
          ~args:
            [ "exec"
            ; "coda.exe"
            ; "daemon"
            ; "--"
            ; "-background"
            ; "-client-port"
            ; sprintf "%d" port
            ; "-config-directory"
            ; log_dir ]
          ()
      in
      Core.Printf.eprintf !"RESULT0: %s\n%!" result0 ;
      Core.Printf.eprintf "WAITING\n%!" ;
      let%bind () = after (Time.Span.of_sec 5.) in
      let open Deferred.Let_syntax in
      Core.Printf.eprintf "STARTING CLIENT\n%!" ;
      let%bind result =
        match%map
          Process.run ~prog:"dune"
            ~args:
              [ "exec"
              ; "coda.exe"
              ; "client"
              ; "status"
              ; "--"
              ; "-daemon-port"
              ; sprintf "%d" port ]
            ()
        with
        | Ok s ->
            Ok s
        | Error e ->
            let%bind _ = stop port in
            Error e
      in
      Core.Printf.eprintf !"RESULT: %s\n%!" result ;
      Core.Printf.eprintf "STOPPING\n%!" ;
      let%map _ = stop port in
      result

    let%test "The coda daemon performs work in the background" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          test_background_daemon () |> Deferred.map ~f:Result.is_ok )
  end )
