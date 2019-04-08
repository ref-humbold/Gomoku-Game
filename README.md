# Gomoku
Gomoku game in Ocaml

## About
Gomoku is a small two-player (human vs computer) game written in OCaml language. Human player uses white stones and begins each game, whereas computer player uses black stones. Winner is the first player which puts 5 stones of their colour in an unbroken chain either horizontally, vertically or diagonally.

LET'S PLAY GOMOKU!

-----

## Dependencies

### Standard build & run
Build process:
+ [OCaml 4.02](https://ocaml.org)
+ [Dune](https://dune.build)
+ [GNU Make](https://www.gnu.org/software/make)

Additional libraries:
+ OCaml Graphics

-----

## How to build?
Gomoku can be built using **Dune** with help of **GNU Make**.

> Possible Make targets are:
> + `make` - same as `make all`
> + `make gomoku` - same as `make all`
> + `make all` - compile source files & create executable
> + `make refresh` - remove additional build files & compile source files & create executable

## How to run?
Gomoku can be run directly using the executable file in the `bin` directory:
```sh
$ /path-to-directory/bin/gomoku
```
