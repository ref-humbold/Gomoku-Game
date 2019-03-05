# Gomoku
Gomoku game in Ocaml

----

### About
Gomoku is a small two-player (human-versus-computer) game written in OCaml programming language. Human player uses white stones and begins each game, whereas computer player uses black stones. The winner is the first player who puts 5 stones in an unbroken chain either horizontally, vertically or diagonally.

LET'S PLAY GOMOKU!

----

### Dependencies
Gomoku requires at least **[OCaml version 4.02](https://ocaml.org/docs/install.html)** installed. There are additional libraries required:
+ OCaml Graphics

----

### How to build?
Gomoku can be built using **[Dune](https://dune.build/)** with help of **[GNU Make](https://www.gnu.org/software/make/)**. Possible Make targets are
+ `make` - same as `make all`
+ `make gomoku` - same as `make all`
+ `make refresh` - same as `make clean all`
+ `make clean` - remove additional build files
+ `make all` - compile source files, create executable

### How to run?
Gomoku can be run directly using an executable file:
```sh
$ /path/to/directory/gomoku
```
