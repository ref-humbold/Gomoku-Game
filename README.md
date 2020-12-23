# Gomoku-Game
![Tag](https://img.shields.io/github/v/tag/ref-humbold/Gomoku-Game?style=plastic)
![License](https://img.shields.io/github/license/ref-humbold/Gomoku-Game?style=plastic)

Single-player gomoku game in Ocaml

## About
Gomoku is a small single-player (human vs computer) game written in OCaml language. Human player uses white stones and begins each game, whereas computer player uses black stones. Winner is the first player who puts 5 stones of their colour in an unbroken chain either horizontally, vertically or diagonally.

LET'S PLAY GOMOKU!

-----

## Dependencies

### Standard build & run
> *versions last used by the author are in double parentheses and italic*

General:
+ Linux-based operating system \
  *((Debian testing))*
+ [OCaml](https://ocaml.org) \
  *((APT package `ocaml`, 4.11.+))*
+ [Dune](https://dune.build) \
  *((OPAM package `dune`, 2.7.+))*
+ [GNU Make](https://www.gnu.org/software/make) \
  *((APT package `make`, 4.3.+))*

Additional libraries:
+ Graphics \
  *((OPAM package `graphics`, 5.1.+))*

### Unit testing
+ OUnit2 \
  *((OPAM package `ounit2`, 2.2.+))*

### Automated formatting
+ Ocamlformat \
  *((OPAM package `ocamlformat`, 0.16.+))*
+ Ocp-indent \
  *((OPAM package `ocp-indent`, 1.8.+))*

-----

## How to build?
Gomoku can be built using **Dune** with help of **GNU Make**.

Possible Make targets are:
+ `make`, `make all` - compile source files & link executable & run unit tests
+ `make build` - format source files & compile source files & link executable & run unit tests
+ `make compile` - compile source files & link executable
+ `make test` - run unit tests
+ `make format` - format source files
+ `make clean` - remove additional build files
+ `make refresh` - remove additional build files & compile source files & link executable & run unit tests

## How to run?
Gomoku can be run directly using the executable file in the `bin` directory:
```sh
$ /path-to-project-directory/bin/gomoku
```
