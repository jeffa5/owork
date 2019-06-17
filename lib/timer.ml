(** State of the timer *)
type state = IDLE | WORKING | SHORT_BREAK | LONG_BREAK [@@deriving show]

let state_to_string = function
  | IDLE ->
      "Idle"
  | WORKING ->
      "Work"
  | SHORT_BREAK ->
      "Short"
  | LONG_BREAK ->
      "Long"

(** Type of interruptions for the server part to send *)
type interruption = Pause | Reset | Restart | Skip [@@deriving show]

(** Interruptions to wait for when unpausing *)
type unpause_interruption = Reset | Restart | Skip

(** Type of the server config, store state and program arguments *)
type server_config =
  { mutable state: state [@default IDLE]
  ; mutable paused: bool [@default true]
  ; mutable interruption: interruption Lwt_mvar.t
        [@printer
          fun fmt interruption ->
            Format.pp_print_string fmt
              ( match Lwt_mvar.take_available interruption with
              | None ->
                  "None"
              | Some interruption ->
                  show_interruption interruption )]
        [@default Lwt_mvar.create_empty ()]
  ; mutable timer: int [@default 0]
  ; mutable session_length: int [@default 1]
  ; work_duration: Duration.t
  ; short_break_duration: Duration.t
  ; long_break_duration: Duration.t
  ; number_work_sessions: int
  ; notify_script: string option [@default None]
  ; mutable work_sessions_completed: int [@default 0] }
[@@deriving make, show]

(** Send a notification using a user-configured script *)
let notify server_config body =
  match !server_config.notify_script with
  | Some notify_script ->
      let command = Printf.sprintf "%s \"%s\"" notify_script body in
      let%lwt _ = Lwt_unix.system command in
      Lwt.return_unit
  | None ->
      Lwt.return_unit

(** Wait for the timer to be unpaused *)
let wait_for_unpause server_config =
  let aux () =
    match%lwt Lwt_mvar.take !server_config.interruption with
    | Pause ->
        !server_config.paused <- false ;
        Lwt.return_none
    | Reset ->
        Lwt.return_some Reset
    | Restart ->
        Lwt.return_some Restart
    | Skip ->
        Lwt.return_some Skip
  in
  if !server_config.paused then aux () else Lwt.return_none

(** Type for the sleep function to return *)
type sleep_result = Complete | Pause | Reset | Restart | Skip

(** Sleep for the given duration and update every second check for pause or other event which cancels the timer *)
let sleep server_config duration =
  let sleep_length = Duration.to_sec duration in
  !server_config.session_length <- sleep_length ;
  !server_config.timer <- sleep_length ;
  let rec sleep_or_interrupt () =
    let rec sleep_in_steps () =
      if !server_config.timer > 0 then (
        let%lwt () = Lwt_unix.sleep 1. in
        !server_config.timer <- !server_config.timer - 1 ;
        sleep_in_steps () )
      else Lwt.return Complete
    in
    let wait_for_interruption () =
      match%lwt Lwt_mvar.take !server_config.interruption with
      | Pause ->
          !server_config.paused <- true ;
          Lwt.return Pause
      | Reset ->
          Lwt.return Reset
      | Restart ->
          Lwt.return Restart
      | Skip ->
          Lwt.return Skip
    in
    let%lwt sleep_result =
      Lwt.pick [sleep_in_steps (); wait_for_interruption ()]
    in
    match sleep_result with
    | Pause -> (
        match%lwt wait_for_unpause server_config with
        | None ->
            sleep_or_interrupt ()
        | Some Reset ->
            Lwt.return Reset
        | Some Restart ->
            Lwt.return Restart
        | Some Skip ->
            Lwt.return Skip )
    | result ->
        Lwt.return result
  in
  sleep_or_interrupt ()

(** Handle the states repeatedly. This is the main driver of the program *)
let handle_state server_config =
  let reset () =
    !server_config.work_sessions_completed <- 0 ;
    !server_config.paused <- true ;
    Lwt.return IDLE
  in
  let restart () =
    !server_config.paused <- false ;
    Lwt.return !server_config.state
  in
  let work_complete () =
    !server_config.work_sessions_completed <- !server_config.work_sessions_completed + 1 ;
    ( if !server_config.work_sessions_completed mod !server_config.number_work_sessions == 0
    then LONG_BREAK
    else SHORT_BREAK )
    |> Lwt.return
  in
  let skip () =
    !server_config.paused <- false ;
    match !server_config.state with
    | IDLE ->
        Lwt.return IDLE
    | WORKING ->
        work_complete ()
    | SHORT_BREAK | LONG_BREAK ->
        Lwt.return WORKING
  in
  let rec aux () =
    match !server_config.state with
    | IDLE ->
        let%lwt new_state =
          match%lwt wait_for_unpause server_config with
          | None ->
              !server_config.paused <- false ;
              Lwt.return WORKING
          | Some Reset ->
              reset ()
          | Some Restart ->
              restart ()
          | Some Skip ->
              skip ()
        in
        !server_config.state <- new_state ;
        aux ()
    | WORKING ->
        let%lwt () = notify server_config "Starting work session" in
        let%lwt new_state =
          match%lwt sleep server_config !server_config.work_duration with
          | Pause ->
              Lwt.return WORKING
          | Reset ->
              reset ()
          | Restart ->
              restart ()
          | Skip ->
              skip ()
          | Complete ->
              work_complete ()
        in
        !server_config.state <- new_state ;
        aux ()
    | SHORT_BREAK ->
        let%lwt () = notify server_config "Starting short break" in
        let%lwt new_state =
          match%lwt sleep server_config !server_config.short_break_duration with
          | Pause ->
              Lwt.return SHORT_BREAK
          | Reset ->
              reset ()
          | Restart ->
              restart ()
          | Skip ->
              skip ()
          | Complete ->
              Lwt.return WORKING
        in
        !server_config.state <- new_state ;
        aux ()
    | LONG_BREAK ->
        let%lwt () = notify server_config "Starting long break" in
        let%lwt new_state =
          match%lwt sleep server_config !server_config.long_break_duration with
          | Pause ->
              Lwt.return LONG_BREAK
          | Reset ->
              reset ()
          | Restart ->
              restart ()
          | Skip ->
              skip ()
          | Complete ->
              Lwt.return WORKING
        in
        !server_config.state <- new_state ;
        aux ()
  in
  aux ()

(** The server which creates and registers with the socket and then listens to handle client connections *)
let server server_config =
  let home = Sys.getenv "HOME" in
  let server_file = home ^ "/.ocaml-productivity-timer.sock" in
  let%lwt () =
    try%lwt Lwt_unix.unlink server_file with _ -> Lwt.return_unit
  in
  Lwt_io.establish_server_with_client_address
    (Unix.ADDR_UNIX (home ^ "/.ocaml-productivity-timer.sock"))
    (fun _addr (ic, oc) ->
      let write s =
        let%lwt () = Lwt_io.write_line oc s in
        Lwt_io.flush oc
      in
      let%lwt str = Lwt_io.read_line_opt ic in
      let%lwt () =
        match str with
        | Some str -> (
          match String.split_on_char '/' str with
          | ["set"; query] -> (
            match query with
            | "pause" ->
                Lwt_mvar.put !server_config.interruption Pause
            | "reset" ->
                Lwt_mvar.put !server_config.interruption Reset
            | "restart" ->
                Lwt_mvar.put !server_config.interruption Restart
            | "skip" ->
                Lwt_mvar.put !server_config.interruption Skip
            | _ ->
                write "Invalid keyword" )
          | ["get"; query] -> (
            match query with
            | "state" ->
                write (state_to_string !server_config.state)
            | "time_left" ->
                write
                  (Printf.sprintf "%d:%02d" (!server_config.timer / 60)
                     (!server_config.timer mod 60))
            | "percent_left" ->
                write
                  (string_of_int
                     (int_of_float
                        ( float !server_config.timer
                        /. float !server_config.session_length
                        *. 100.0 )))
            | "sessions_complete" ->
                write (string_of_int !server_config.work_sessions_completed)
            | "pause" ->
                write (string_of_bool !server_config.paused)
            | keyword ->
                write ("Invalid keyword: " ^ keyword) )
          | _ ->
              write "Invalid query, format is (get|set)/(keyword)" )
        (* | Some str -> Lwt_io.printl ("Unexpected string received: " ^ str) *)
        | None ->
            Lwt.return_unit
      in
      Lwt_io.flush oc )

(** Stop the server and remove any other items when shutting down *)
let stop server =
  Lwt_main.run
    (let%lwt () = Lwt_io.printl "Stopping" in
     Lwt_io.shutdown_server server) ;
  exit 0

let test_config () =
  make_server_config ~work_duration:(Duration.of_min 25)
    ~short_break_duration:(Duration.of_min 5)
    ~long_break_duration:(Duration.of_min 30) ~number_work_sessions:3
    ~notify_script:(Some "some/script/notify") ()
  |> ref

let print_config config_ref = print_endline (show_server_config !config_ref)

let%expect_test "initial config" =
  print_config @@ test_config () ;
  [%expect
    {|
    { Timer.state = Timer.IDLE; paused = true; interruption = None; timer = 0;
      session_length = 1; work_duration = 25 minutes ;
      short_break_duration = 5 minutes ; long_break_duration = 30 minutes ;
      number_work_sessions = 3; notify_script = "some/script/notify";
      work_sessions_completed = 0 } |}]

let%expect_test "wait_for_unpause" =
  let config = test_config () in
  let test state interruption =
    !config.paused <- true ;
    !config.state <- state ;
    let%lwt () = Lwt_mvar.put !config.interruption interruption in
    let%lwt _ = wait_for_unpause config in
    Lwt.return_unit
  in
  Lwt_main.run @@ test IDLE Pause ;
  print_config config ;
  [%expect
    {|
    { Timer.state = Timer.IDLE; paused = false; interruption = None; timer = 0;
      session_length = 1; work_duration = 25 minutes ;
      short_break_duration = 5 minutes ; long_break_duration = 30 minutes ;
      number_work_sessions = 3; notify_script = "some/script/notify";
      work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Reset ;
  print_config config ;
  [%expect
    {|
    { Timer.state = Timer.IDLE; paused = true; interruption = None; timer = 0;
      session_length = 1; work_duration = 25 minutes ;
      short_break_duration = 5 minutes ; long_break_duration = 30 minutes ;
      number_work_sessions = 3; notify_script = "some/script/notify";
      work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Restart ;
  print_config config ;
  [%expect
    {|
    { Timer.state = Timer.IDLE; paused = true; interruption = None; timer = 0;
      session_length = 1; work_duration = 25 minutes ;
      short_break_duration = 5 minutes ; long_break_duration = 30 minutes ;
      number_work_sessions = 3; notify_script = "some/script/notify";
      work_sessions_completed = 0 } |}] ;
  Lwt_main.run @@ test IDLE Skip ;
  print_config config ;
  [%expect
    {|
    { Timer.state = Timer.IDLE; paused = true; interruption = None; timer = 0;
      session_length = 1; work_duration = 25 minutes ;
      short_break_duration = 5 minutes ; long_break_duration = 30 minutes ;
      number_work_sessions = 3; notify_script = "some/script/notify";
      work_sessions_completed = 0 } |}]
