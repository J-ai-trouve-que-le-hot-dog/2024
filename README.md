# Team ~~(SII)(SII)~~ B$ L$ B$ v$ v$ L$ B$ v$ v$

We worked in OCaml and C++.

## lambdaman

We used a linear congruence generator to generate a list of directions for most of
the lambdaman problems.
Depending on the inputs, we either used the generator raw, or biased its output a bit,
by making it do always two steps in the same directions (for labyrinths), or randomly
do one or several steps in the same direction. The stop condition of the generator
is taken as having the internal state of the generator equal to a given value, to save
on the number of variables.
For some of the problems, this was not enough, and we concatenaed the outputs of the
generator with several different seeds.
We also tried to minimise the numbers involved (seeds, parameters of the LCG) to
minimise the size.

For some problems, we used handcoded solutions, and we also used base-94 compression
of the directions for some others.

We wrote our ICFP expressions using a small embedded language in our OCaml code (see
lib/miniterm.ml).

## spaceship

TODO Rafael

## 3d

This is the problem for which we wrote the most tooling. Besides the obvious simulator,
this includes:

- a TUI, which allows us to select and move parts of the solution

- a linker, which allows to specify labels in some cells as `X:0` specifying a cell
  named `X` with `0` as its initial value, and `X@x` and `X@y` used to refer to cells
  in the destination of a time warp. Files with labels are often named with a `.3dl`
  extension.

- a compiler from an embedded language, to 3d programs that always execute with only
  two different times. It works by generating a given number of widgets, and using
  a SAT solver to generate a layout packing them as closely as possible.


We wrote the small examples by hand (using only the linked and the editor), using the
compiler for the larger examples. Unfortunately, our compiled solutions hit the time
limit for both 3d8 and 3d11, but we believe they would have beat the high score at the
end of the competition if they had not.

## Efficiency

We already had written an interpreter with call-by-need, so it did not require many
changes to adapt it. For small efficiency problems, we solved them by hand,
recognizing the function that was being computed. For SAT and sudoku, we recognized
that this was what was being computed, and we used a solver. We lost some time
trying to debug the SAT solver before seeing that our solution was actually correct.


## Lightning round

At the end of the lightning round, we had:

- all efficiency problems solved
- problems 3d1-9 handwritten, using the editor for some (but no linker nor compiler),
  as well as a simulator to be able to test
- [TODO Rafael: spaceship?]
- handcoded and compressed solutions to lambdaman, but no random walks yet.