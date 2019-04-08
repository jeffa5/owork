# ocaml-productivity-timer

An OCaml implementation of a productivity timer. There is a server component at the moment which accepts connections over a Unix socket. The timer can then be started, paused/resumed, reset and sessions restarted.

## Build

To download the program and run the help command.

```sh
git clone https://github.com/jeffa5/ocaml-productivity-timer.git
cd ocaml-productivity-timer
dune exec src/productivity-timer.exe -- --help
```

To build the executable and move it to somewhere else
```sh
dune build src/productivity-timer.exe
cp _build/default/src/productivity-timer.exe <wherever>
```
