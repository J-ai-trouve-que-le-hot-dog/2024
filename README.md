# Team ~~(SII)(SII)~~ B$ L$ B$ v$ v$ L$ B$ v$ v$

We worked in OCaml and C++.

## lambdaman

We used a linear congruence generator to generate a random walk for most of
the lambdaman problems.
Depending on the inputs, we either used the generator raw, or biased its output a bit,
by making it do always two steps in the same directions (for labyrinths), or randomly
do one or several steps in the same direction. The stop condition of the generator
is taken as having the internal state of the generator equal to a given value, to save
on the number of variables.
For some of the problems, this was not enough, and we concatenated
multiple smaller random walks.
We also tried to minimize the numbers involved (seeds, parameters of the LCG) to
minimize the size.
Some multithreaded programs were written to search for the seeds and
parameters of the LCG (see `rafael/lambdaman_random1.cpp`).

For some problems, we used handcoded solutions, and we also used base-94 compression
of the directions for some others.

We wrote our ICFP expressions using a small embedded language in our OCaml code (see
`lib/miniterm.ml`).


## spaceship

For spaceship, our solution performs local optimization (simulated
annealing or hill climbing) on the list of (point from the problem input, speed when reaching that point).
We precompute a big table that can be used to compute the distances
from (point1, speed1) to (point2, speed2) in O(1).

The transitions for local optimization are:

1. Randomly change the speed at one point.
2. A 2-opt move (reverse some sublist). Because the points are visited
in the reverse order, we have to replace (speed when reaching the
point) by -(speed when leaving the point) for points in the sublist.
3. A 3-opt move (swap two consecutive sublists). The speeds of the
endpoints of the sublists can also be changed.
4. Once every 10^8 iterations: use beam search to optimize all speeds
at the same time, without changing the order of visited points.

Some programs were written to find an initial solution before
performing local optimization.

1. Beam search on (position, speed, set of visited points).
This is good when distances between points are small.
   This found the best known solution to spaceship19 (11279).
2. Solve the TSP instance optimizing the permutation of points when
   all speeds are fixed to zero.

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
- local optimization for spaceship, without initialization to a good initial solution
- handcoded and compressed solutions to lambdaman, but no random walks yet.
