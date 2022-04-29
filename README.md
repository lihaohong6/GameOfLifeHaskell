# Game of Life

## Setup

Download this project as a zip file and unzip it.

Install `ghc`, `stack`, and `cabal` according to instructions on the [official site](https://www.haskell.org/downloads/).

## Execution

Go to the root directory of the project. In the terminal, type in
```shell
stack build --exec GameOfLife-exe
```

This should build the project and then execute the main program, which prompts the user for input.

Type in a file name. For example, `examples/glider.txt`. This will simulate the pre-written glider pattern. 

There are a few more patterns in the `examples` folder, such as `glider_gun.txt` and `pentadecathlon.txt`.

To test out your own setup, we recommend changing the `test.txt` file by replacing some of the `0` with `1`, which 
represents living cells. Then, run the program with `stack` and type in `examples/test.txt` as your file.
