open Cmdliner

(** State of the timer *)
type state = IDLE | WORKING | SHORT_BREAK | LONG_BREAK

let state_to_string = function
  | IDLE -> "Idle"
  | WORKING -> "Work"
  | SHORT_BREAK -> "Short"
  | LONG_BREAK -> "Long"

(** Type of the server config, store state and program arguments *)
type config =
  { mutable state: state (* State of the timer *)
  ; mutable paused: bool (* Pause the timer *)
  ; mutable reset:
      bool (* Reset the timer to idle and number of work sessions *)
  ; mutable restart: bool (* Restart the current session *)
  ; work_duration: Duration.t
  ; short_break_duration: Duration.t
  ; long_break_duration: Duration.t
  ; number_work_sessions: int
  ; notify_script: string
  ; mutable work_sessions_completed: int }

(** Send a notification using a user-configured script *)
let notify config body =
  let command = Printf.sprintf "./%s \"%s\"" !config.notify_script body in
  let%lwt _ = Lwt_unix.system command in
  Lwt.return_unit

(** Write the config out to file along with the duration and length *)
let write_config config duration length =
  let home = Sys.getenv "HOME" in
  Lwt_io.with_file ~mode:Output (home ^ "/.ocaml-pomodoro") (fun channel ->
      let state_string = state_to_string !config.state in
      let first_line =
        match !config.state with
        | IDLE -> state_string
        | _ ->
            Printf.sprintf "%s %d:%02d %d" state_string (duration / 60)
              (duration mod 60) !config.work_sessions_completed
      in
      Lwt_io.write channel
      @@ Printf.sprintf "%s\n%d\n%s" first_line
           (int_of_float (float duration /. float length *. 100.0))
           (string_of_bool !config.paused) )

(** Wait for the timer to be unpaused *)
let wait_for_unpause config =
  let rec aux () =
    if !config.reset then Lwt.return_unit
    else if !config.restart then Lwt.return_unit
    else if !config.paused then
      let%lwt () = Lwt_unix.sleep 0.1 in
      let%lwt () = Lwt_unix.yield () in
      aux ()
    else Lwt.return_unit
  in
  aux ()

(** Type for the sleep function to return *)
type sleep_result = Complete | Reset | Restart

(** Sleep for the given duration and update every second check for pause or other event which cancels the timer *)
let sleep config duration =
  let sleep_length = Duration.to_sec duration in
  let write_state = write_config config in
  let rec sleep_duration duration =
    if duration > 0 then
      let%lwt () =
        if !config.paused then wait_for_unpause config else Lwt.return_unit
      in
      if !config.reset then Lwt.return Reset
      else if !config.restart then Lwt.return Restart
      else
        let%lwt () = Lwt_unix.sleep 1.0 in
        let%lwt () = write_state (duration - 1) sleep_length in
        sleep_duration (duration - 1)
    else Lwt.return Complete
  in
  let%lwt () = write_state (Duration.to_sec duration) sleep_length in
  sleep_duration @@ Duration.to_sec duration

(** Handle the states repeatedly. This is the main driver of the program *)
let handle_state config =
  let reset () =
    !config.work_sessions_completed <- 0 ;
    !config.paused <- true ;
    !config.reset <- false ;
    Lwt.return IDLE
  in
  let restart () =
    !config.restart <- false ;
    Lwt.return !config.state
  in
  let rec aux () =
    match !config.state with
    | IDLE ->
        let%lwt () = write_config config 0 0 in
        let%lwt () = wait_for_unpause config in
        !config.state <- WORKING ;
        !config.reset <- false ;
        !config.restart <- false ;
        aux ()
    | WORKING ->
        let%lwt () = notify config "Starting work session" in
        let%lwt new_state =
          match%lwt sleep config !config.work_duration with
          | Reset -> reset ()
          | Restart -> restart ()
          | Complete ->
              !config.work_sessions_completed
              <- !config.work_sessions_completed + 1 ;
              ( if
                !config.work_sessions_completed
                mod !config.number_work_sessions
                == 0
              then LONG_BREAK
              else SHORT_BREAK )
              |> Lwt.return
        in
        !config.state <- new_state ;
        aux ()
    | SHORT_BREAK ->
        let%lwt () = notify config "Starting short break" in
        let%lwt new_state =
          match%lwt sleep config !config.short_break_duration with
          | Reset -> reset ()
          | Restart -> restart ()
          | Complete -> Lwt.return WORKING
        in
        !config.state <- new_state ;
        aux ()
    | LONG_BREAK ->
        let%lwt () = notify config "Starting long break" in
        let%lwt new_state =
          match%lwt sleep config !config.long_break_duration with
          | Reset -> reset ()
          | Restart -> restart ()
          | Complete -> Lwt.return WORKING
        in
        !config.state <- new_state ;
        aux ()
  in
  aux ()

(** The server which creates and registers with the socket and then listens to handle client connections *)
let server config =
  let home = Sys.getenv "HOME" in
  Lwt_io.establish_server_with_client_address
    (Unix.ADDR_UNIX (home ^ "/.ocaml-pomodoro.sock"))
    (fun _addr (ic, _oc) ->
      let%lwt str = Lwt_io.read_line_opt ic in
      match str with
      | Some "pause" ->
          !config.paused <- not !config.paused ;
          Lwt.return_unit
      | Some "reset" ->
          !config.reset <- true ;
          Lwt.return_unit
      | Some "restart" ->
          !config.restart <- true ;
          Lwt.return_unit
      | _ -> Lwt.return_unit )

(** Stop the server and remove any other items when shutting down *)
let stop server =
  print_endline "\rStopping" ;
  Lwt_main.run
    (let%lwt () = Lwt_io.shutdown_server server in
     let home = Sys.getenv "HOME" in
     Lwt_unix.unlink (home ^ "/.ocaml-pomodoro")) ;
  exit 0

(** Given the program arguments create the config and start the server before handling the state *)
let main work_duration short_break_duration long_break_duration
    number_work_sessions notify_script =
  print_endline "Starting pomodoro server" ;
  let config =
    ref
      { state= IDLE
      ; paused= true
      ; reset= false
      ; restart= false
      ; work_duration= Duration.of_min work_duration
      ; short_break_duration= Duration.of_min short_break_duration
      ; long_break_duration= Duration.of_min long_break_duration
      ; number_work_sessions
      ; notify_script
      ; work_sessions_completed= 0 }
  in
  Lwt_main.run
    (let%lwt server = server config in
     let _ = Lwt_unix.on_signal Sys.sigint (fun _ -> stop server) in
     let _ = Lwt_unix.on_signal Sys.sigterm (fun _ -> stop server) in
     handle_state config)

let work_duration =
  let doc = "Length in minutes of the work session." in
  Arg.(value & opt int 25 & info ["w"; "work-duration"] ~doc)

let short_break_duration =
  let doc = "Length in minutes of the short break." in
  Arg.(value & opt int 5 & info ["s"; "short-break"] ~doc)

let long_break_duration =
  let doc = "Length in minutes of the long break." in
  Arg.(value & opt int 30 & info ["l"; "long-break"] ~doc)

let number_work_sessions =
  let doc = "Number of work sessions to be completed before a long break." in
  Arg.(value & opt int 3 & info ["n"; "number-work-sessions"] ~doc)

let notify_script =
  let doc = "Location of the script to handle the notifications." in
  Arg.(required & pos 0 (some file) None & info [] ~doc)

let program =
  Term.(
    const main $ work_duration $ short_break_duration $ long_break_duration
    $ number_work_sessions $ notify_script)

let info =
  let doc = "A pomodoro timing server." in
  Term.info "pomodoro" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (program, info)
