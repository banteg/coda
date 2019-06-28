open Core
open Async

(**
 * Test the basic functionality of the coda daemon and client through the CLI
 *)

let%test_module "Command line tests" =
  ( module struct
    (* executable location relative to src/default/lib/command_line_tests

         dune won't allow running it via "dune exec", because it's outside its 
         workspace, so we invoke the executable directly
       *)
    let coda_exe = "../../app/cli/src/coda.exe"

    let start_daemon config_dir port =
      Process.run_exn ~prog:coda_exe
        ~args:
          [ "daemon"
          ; "-background"
          ; "-client-port"
          ; sprintf "%d" port
          ; "-config-directory"
          ; config_dir ]
        ()

    let stop_daemon port =
      Process.run () ~prog:coda_exe
        ~args:["client"; "stop-daemon"; "-daemon-port"; sprintf "%d" port]

    let start_client port =
      Process.run ~prog:coda_exe
        ~args:["client"; "status"; "-daemon-port"; sprintf "%d" port]
        ()

    let create_config_directory config_dir =
      (* create empty config dir to avoid any issues with the default config dir *)
      let%bind _ = Process.run_exn ~prog:"rm" ~args:["-rf"; config_dir] () in
      let%map _ = Process.run_exn ~prog:"mkdir" ~args:["-p"; config_dir] () in
      ()

    let test_background_daemon () =
      let config_dir = "/tmp/coda-spun-test" in
      let port = 1337 in
      (* it takes awhile for the daemon to become available *)
      let client_delay = 12. in
      let%bind _ = create_config_directory config_dir in
      let%bind _ = start_daemon config_dir port in
      let%bind () = after (Time.Span.of_sec client_delay) in
      let%bind result = start_client port in
      let%map _ = stop_daemon port in
      result

    let%test "The coda daemon performs work in the background" =
      Async.Thread_safe.block_on_async_exn (fun () ->
          test_background_daemon () |> Deferred.map ~f:Result.is_ok )
  end )
