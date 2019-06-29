# Gomoku-Game
Single-player gomoku game in Ocaml

## About
Gomoku is a small single-player (human vs computer) game written in OCaml language. Human player uses white stones and begins each game, whereas computer player uses black stones. Winner is the first player who puts 5 stones of their colour in an unbroken chain either horizontally, vertically or diagonally.

LET'S PLAY GOMOKU!

-----

## Dependencies

### Standard build & run
> *versions used by the author are in double parentheses and italic*

Build process:
+ Linux-based operating system *((Debian testing))*
+ [OCaml](https://ocaml.org) *((4.07.0))*
+ [Dune](https://dune.build) *((1.10.0))*
+ [GNU Make](https://www.gnu.org/software/make) *((4.2.1))*

Additional libraries:
+ OCaml Graphics *((4.07.0))*

Automated formatting by:
+ Ocamlformat *((0.10))*
+ Ocp-indent *((1.7.0))*

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
