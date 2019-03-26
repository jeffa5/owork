# ocaml-pomodoro

An OCaml implementation of a pomodoro timer. There is a server component at the moment which accepts connections over a Unix socket. The timer can then be started, paused/resumed, reset and sessions restarted.

## Build

To download the program and run the help command.

```sh
git clone https://github.com/jeffa5/ocaml-pomodoro.git
cd ocaml-pomodoro
dune exec src/pomodoro.exe -- --help
```

To build the executable and move it to somewhere else
```sh
dune build src/pomodoro.exe
cp _build/default/src/pomodoro.exe <wherever>
```
