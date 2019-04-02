(** State of the timer *)
type state = IDLE | WORKING | SHORT_BREAK | LONG_BREAK [@@deriving show]

let state_to_string = function
  | IDLE -> "Idle"
  | WORKING -> "Work"
  | SHORT_BREAK -> "Short"
  | LONG_BREAK -> "Long"

(** Type of interruptions for the server part to send *)
type interruption = Pause | Reset | Restart | Skip [@@deriving show]

(** Type of the server config, store state and program arguments *)
type config =
  { mutable state: state [@default IDLE]
  ; mutable paused: bool [@default true]
  ; mutable interruption: interruption Lwt_mvar.t
         [@printer
           fun fmt interruption ->
             Format.pp_print_string fmt
               ( match Lwt_mvar.take_available interruption with
               | None -> "None"
               | Some interruption -> show_interruption interruption )]
         [@default Lwt_mvar.create_empty ()]
  ; mutable timer: int [@default 0]
  ; work_duration: Duration.t
  ; short_break_duration: Duration.t
  ; long_break_duration: Duration.t
  ; number_work_sessions: int
  ; notify_script: string
  ; mutable work_sessions_completed: int [@default 0] }
[@@deriving make, show]

(** Send a notification using a user-configured script *)
let notify config body =
  let command = Printf.sprintf "%s \"%s\"" !config.notify_script body in
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
  let aux () =
    match%lwt Lwt_mvar.take !config.interruption with
    | Pause ->
        !config.paused <- false ;
        Lwt.return_unit
    | Reset | Restart | Skip -> Lwt.return_unit
  in
  if !config.paused then aux () else Lwt.return_unit

(** Type for the sleep function to return *)
type sleep_result = Complete | Pause | Reset | Restart | Skip

(** Sleep for the given duration and update every second check for pause or other event which cancels the timer *)
let sleep config duration =
  let debug_print () =
    Lwt_io.printf "%s %d\n" (state_to_string !config.state) !config.timer
  in
  let sleep_length = Duration.to_sec duration in
  !config.timer <- sleep_length ;
  let rec sleep_or_interrupt () =
    let%lwt () = Lwt_io.printl (show_config !config) in
    let rec sleep_in_steps () =
      if !config.timer > 0 then (
        let%lwt () = debug_print () in
        let%lwt () = Lwt_unix.sleep 1. in
        !config.timer <- !config.timer - 1 ;
        sleep_in_steps () )
      else Lwt.return Complete
    in
    let wait_for_interruption () =
      match%lwt Lwt_mvar.take !config.interruption with
      | Pause ->
          !config.paused <- true ;
          let%lwt () = Lwt_io.printl (show_config !config) in
          Lwt.return Pause
      | Reset -> Lwt.return Reset
      | Restart -> Lwt.return Restart
      | Skip -> Lwt.return Skip
    in
    let%lwt sleep_result =
      Lwt.pick [sleep_in_steps (); wait_for_interruption ()]
    in
    match sleep_result with
    | Pause ->
        let%lwt () = wait_for_unpause config in
        sleep_or_interrupt ()
    | result ->
        let%lwt () = Lwt_io.printl "non pause sleep result" in
        Lwt.return result
  in
  sleep_or_interrupt ()

(** Handle the states repeatedly. This is the main driver of the program *)
let handle_state config =
  let reset () =
    !config.work_sessions_completed <- 0 ;
    !config.paused <- true ;
    Lwt.return IDLE
  in
  let restart () =
    !config.paused <- false ;
    Lwt.return !config.state
  in
  let skip () =
    match !config.state with
    | IDLE -> Lwt.return IDLE
    | WORKING | SHORT_BREAK | LONG_BREAK -> Lwt.return WORKING
  in
  let rec aux () =
    match !config.state with
    | IDLE ->
        let%lwt () = write_config config 0 0 in
        let%lwt () = wait_for_unpause config in
        !config.state <- WORKING ;
        !config.paused <- false ;
        aux ()
    | WORKING ->
        let%lwt () = notify config "Starting work session" in
        let%lwt new_state =
          match%lwt sleep config !config.work_duration with
          | Pause -> Lwt.return WORKING
          | Reset -> reset ()
          | Restart -> restart ()
          | Skip -> skip ()
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
          | Pause -> Lwt.return SHORT_BREAK
          | Reset -> reset ()
          | Restart -> restart ()
          | Skip -> skip ()
          | Complete -> Lwt.return WORKING
        in
        !config.state <- new_state ;
        aux ()
    | LONG_BREAK ->
        let%lwt () = notify config "Starting long break" in
        let%lwt new_state =
          match%lwt sleep config !config.long_break_duration with
          | Pause -> Lwt.return LONG_BREAK
          | Reset -> reset ()
          | Restart -> restart ()
          | Skip -> skip ()
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
      | Some "pause" -> Lwt_mvar.put !config.interruption Pause
      | Some "reset" -> Lwt_mvar.put !config.interruption Reset
      | Some "restart" -> Lwt_mvar.put !config.interruption Restart
      | Some "skip" -> Lwt_mvar.put !config.interruption Skip
      | Some str -> Lwt_io.printl ("Unexpected string received: " ^ str)
      | None -> Lwt.return_unit )

(** Stop the server and remove any other items when shutting down *)
let stop server =
  print_endline "\rStopping" ;
  Lwt_main.run
    (let%lwt () = Lwt_io.shutdown_server server in
     let home = Sys.getenv "HOME" in
     Lwt_unix.unlink (home ^ "/.ocaml-pomodoro")) ;
  exit 0

let test_config () =
  make_config ~work_duration:(Duration.of_min 25)
    ~short_break_duration:(Duration.of_min 5)
    ~long_break_duration:(Duration.of_min 30) ~number_work_sessions:3
    ~notify_script:"some/script/notify" ()
  |> ref

let print_config config_ref = print_endline (show_config !config_ref)

let%expect_test "initial config" =
  print_config @@ test_config () ;
  [%expect
    {|
    { Pomodoro.state = Pomodoro.IDLE; paused = true; interruption = None;
      timer = 0; work_duration = 25 minutes ; short_break_duration = 5 minutes ;
      long_break_duration = 30 minutes ; number_work_sessions = 3;
      notify_script = "some/script/notify"; work_sessions_completed = 0 } |}]

let%expect_test "wait_for_unpause" =
  let config = test_config () in
  let test state interruption =
    !config.paused <- true ;
    !config.state <- state ;
    let%lwt () = Lwt_mvar.put !config.interruption interruption in
    wait_for_unpause config
  in
  Lwt_main.run @@ test IDLE Pause ;
  print_config config ;
  [%expect
    {|
    { Pomodoro.state = Pomodoro.IDLE; paused = false; interruption = None;
      timer = 0; work_duration = 25 minutes ; short_break_duration = 5 minutes ;
      long_break_duration = 30 minutes ; number_work_sessions = 3;
      notify_script = "some/script/notify"; work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Reset ;
  print_config config ;
  [%expect
    {|
    { Pomodoro.state = Pomodoro.IDLE; paused = true; interruption = None;
      timer = 0; work_duration = 25 minutes ; short_break_duration = 5 minutes ;
      long_break_duration = 30 minutes ; number_work_sessions = 3;
      notify_script = "some/script/notify"; work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Restart ;
  print_config config ;
  [%expect
    {|
    { Pomodoro.state = Pomodoro.IDLE; paused = true; interruption = None;
      timer = 0; work_duration = 25 minutes ; short_break_duration = 5 minutes ;
      long_break_duration = 30 minutes ; number_work_sessions = 3;
      notify_script = "some/script/notify"; work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Skip ;
  print_config config ;
  [%expect
    {|
    { Pomodoro.state = Pomodoro.IDLE; paused = true; interruption = None;
      timer = 0; work_duration = 25 minutes ; short_break_duration = 5 minutes ;
      long_break_duration = 30 minutes ; number_work_sessions = 3;
      notify_script = "some/script/notify"; work_sessions_completed = 0 } |}]
