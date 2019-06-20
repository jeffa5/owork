type t =
  { mutable duration_remaining: Duration.t
  ; mutable paused: bool [@default true]
  ; mutable next_duration: unit -> unit [@default fun () -> ()] }
[@@deriving make, show]

(* Countdown the timer. When paused stop the thread and the start function will create a new countdown thread *)
let countdown_timer t =
  let rec loop () =
    if t.paused then
      let%lwt () = Lwt_unix.yield () in
      loop ()
    else if Duration.to_sec t.duration_remaining > 0 then
      let%lwt () =
        Logs_lwt.debug (fun f ->
            f "Tick: %s" (string_of_int (Duration.to_sec t.duration_remaining))
        )
      in
      let%lwt () = Lwt_unix.sleep 1. in
      let new_time =
        Duration.of_sec (Duration.to_sec t.duration_remaining - 1)
      in
      let%lwt () = Lwt.return @@ (t.duration_remaining <- new_time) in
      loop ()
    else (
      (* Call the callback to reset the duration for the next session *)
      t.next_duration () ;
      loop () )
  in
  loop ()

let create duration =
  let t = make ~duration_remaining:duration () in
  (* start an asynchronous thread to count down the value *)
  let _ = countdown_timer t in
  t

let start t = t.paused <- false

let stop t = t.paused <- true

let set_duration duration t = t.duration_remaining <- duration

let set_callback callback t = t.next_duration <- callback

let time t = t.duration_remaining

let paused t = t.paused

module Test = struct
  let print_timer timer = print_endline (show timer)

  let%expect_test "create new timer" =
    let timer = create (Duration.of_min 25) in
    print_timer timer ;
    [%expect {| { Timer.duration_remaining = 25 minutes ; paused = true } |}]

  let%expect_test "start timer" =
    let timer = create (Duration.of_min 25) in
    start timer ;
    print_timer timer ;
    [%expect {| { Timer.duration_remaining = 25 minutes ; paused = false } |}]

  let%expect_test "pause timer" =
    let timer = create (Duration.of_min 25) in
    start timer ;
    stop timer ;
    print_timer timer ;
    [%expect {| { Timer.duration_remaining = 25 minutes ; paused = true } |}]

  let%expect_test "timer created with 0 time" =
    let timer = create (Duration.of_sec 0) in
    start timer ;
    print_timer timer ;
    [%expect {| { Timer.duration_remaining = ; paused = false } |}]

  let%expect_test "timer does count down" =
    let timer = create (Duration.of_sec 5) in
    print_timer timer ;
    let _ = start timer in
    let promise = Lwt_unix.sleep 1.0 in
    Lwt_main.run promise ;
    print_timer timer ;
    [%expect
      {|
      { Timer.duration_remaining = 5 seconds ; paused = true }
      { Timer.duration_remaining = 4 seconds ; paused = false } |}]
end
