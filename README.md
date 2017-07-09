# haskell-bf
BrainFuck interpreter, transpiler and REPL written in Haskell

# Sample usage
## Compilation

Currently only compilation to C is supported.

```bash
curl -s "http://esoteric.sange.fi/brainfuck/utils/mandelbrot/mandelbrot.b" > mandelbrot.b
stack exec -- haskell-bf-exe compile mandelbrot.b -o mandelbrot.c
cc mandelbrot.c -o mandelbrot -O3
./mandelbrot
rm mandelbrot{,.b,.c}
```

## Run

Brainfuck programs can be run:
```bash
curl -s "http://www.hevanet.com/cristofd/brainfuck/sierpinski.b" > sierpinski.b
stack exec -- haskell-bf-exe run sierpinski.b
```
## REPL

REPL for Brainfuck, keeping single infinite Tape in memory. 
Currently supported commands:

- Clean - cleans tape

To run REPL just add `-i` flag:

```bash
stack exec -- haskell-bf-exe run -i
```

You can also startup with some Brainfuck program:
```bash
curl -s "http://www.hevanet.com/cristofd/brainfuck/sierpinski.b" > sierpinski.b
stack exec -- haskell-bf-exe run sierpinski.b -i
```

# RoadMap:

## REPL

- :let command to store programs in REPL memory
- :load command to load programs from file to REPL memory
