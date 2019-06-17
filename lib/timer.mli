type state [@@deriving show]

type interruption = Pause | Reset | Restart | Skip

type config =
  { mutable state: state [@default IDLE]
  ; mutable paused: bool [@default true]
  ; mutable interruption: interruption Lwt_mvar.t
        [@opaque] [@default Lwt_mvar.create_empty ()]
  ; mutable timer: int [@default 0]
  ; mutable session_length: int [@default 0]
  ; work_duration: Duration.t
  ; short_break_duration: Duration.t
  ; long_break_duration: Duration.t
  ; number_work_sessions: int
  ; notify_script: string option [@default None]
  ; mutable work_sessions_completed: int [@default 0] }
[@@deriving make, show]

val server : config ref -> Lwt_io.server Lwt.t

val stop : Lwt_io.server -> 'a

val handle_state : config ref -> 'a Lwt.t
