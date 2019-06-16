# Gomoku-Game
Single-player gomoku game in Ocaml

## About
Gomoku is a small single-player (human vs computer) game written in OCaml language. Human player uses white stones and begins each game, whereas computer player uses black stones. Winner is the first player who puts 5 stones of their colour in an unbroken chain either horizontally, vertically or diagonally.

LET'S PLAY GOMOKU!

-----

## Dependencies

### Standard build & run

> **Linux-based operating system**

Build process:
+ [OCaml](https://ocaml.org) version >= 4.05.0
+ [Dune](https://dune.build)
+ [GNU Make](https://www.gnu.org/software/make)

Additional libraries:
+ OCaml Graphics

Automated formatting by:
+ Ocamlformat
+ Ocp-indent

-----

## How to build?
Gomoku can be built using **Dune** with help of **GNU Make**.

Possible Make targets are:
+ `make` - same as `make all`
+ `make all` - format source files & compile source files & link executable
+ `make compile` - compile source files & link executable
+ `make refresh` - remove additional build files & format source files & compile source files & link executable

## How to run?
Gomoku can be run directly using the executable file in the `bin` directory:
```sh
$ /path-to-project-directory/bin/gomoku
```
