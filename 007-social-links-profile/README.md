## Open vscode like so

```sh
code project.code-workspace
```

## Run HTML project like so

```sh
# http://localhost:4010
live-server --no-browser --port=4010 ./html-and-css/
```


## Run Elm project like so

```sh
# http://localhost:4011
cd ./elm/
elm-live --port=4011 ./src/Main.elm
```

## Run Haskell projects like so

```sh
# http://localhost:4012
cd ./haskell
ghcid -c 'cabal repl exe:atomic-main' -T main --warnings
```

```sh
# http://localhost:4013
cd ./haskell
ghcid -c 'cabal repl exe:hyperbole-main' -T main --warnings
```
