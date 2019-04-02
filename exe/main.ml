open Cmdliner
open Pomodoro

(** Given the program arguments create the config and start the server before handling the state *)
let main work_duration short_break_duration long_break_duration
    number_work_sessions notify_script =
  print_endline "Starting pomodoro server" ;
  let config =
    ref
    @@ make_config
         ~work_duration:(Duration.of_min work_duration)
         ~short_break_duration:(Duration.of_min short_break_duration)
         ~long_break_duration:(Duration.of_min long_break_duration)
         ~number_work_sessions ~notify_script ()
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
  Arg.(required & pos 0 (some file) None & info [] ~docv:"NOTIFY" ~doc)

let program =
  Term.(
    const main $ work_duration $ short_break_duration $ long_break_duration
    $ number_work_sessions $ notify_script)

let info =
  let doc = "A pomodoro timing server." in
  Term.info "pomodoro" ~doc ~exits:Term.default_exits

let () = Term.exit @@ Term.eval (program, info)
