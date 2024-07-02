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

<details>
<summary>Scores</summary>

```
* [lambdaman1] Your score: 33. Best score: 33.
* [lambdaman2] Your score: 44. Best score: 44.
* [lambdaman3] Your score: 58. Best score: 58.
* [lambdaman4] Your score: 111. Best score: 111.
* [lambdaman5] Your score: 111. Best score: 105.
* [lambdaman6] Your score: 73. Best score: 73.
* [lambdaman7] Your score: 111. Best score: 111.
* [lambdaman8] Your score: 113. Best score: 113.
* [lambdaman9] Your score: 111. Best score: 109.
* [lambdaman10] Your score: 113. Best score: 113.
* [lambdaman11] Your score: 132. Best score: 129.
* [lambdaman12] Your score: 132. Best score: 127.
* [lambdaman13] Your score: 132. Best score: 127.
* [lambdaman14] Your score: 132. Best score: 127.
* [lambdaman15] Your score: 132. Best score: 127.
* [lambdaman16] Your score: 220. Best score: 129.
* [lambdaman17] Your score: 113. Best score: 113.
* [lambdaman18] Your score: 113. Best score: 113.
* [lambdaman19] Your score: 237. Best score: 237.
* [lambdaman20] Your score: 150. Best score: 131.
* [lambdaman21] Your score: 113. Best score: 113.
```
</details>

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
3. A 3-opt move (swap two adjacent sublists). The speeds of the
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

We also tried writing a small GUI (see `bin/spaceship_gui.ml`) to see and
try some manual optimisations on small problems (and for fun). In practice
we only used it on `spaceship5`.
To play with it you can run `dune exec bin/spaceship_gui.exe inputs/spaceship1`

<details>
<summary>Scores</summary>

```
* [spaceship1] Your score: 5. Best score: 5.
* [spaceship2] Your score: 49. Best score: 49.
* [spaceship3] Your score: 10. Best score: 10.
* [spaceship4] Your score: 99. Best score: 99.
* [spaceship5] Your score: 116. Best score: 116.
* [spaceship6] Your score: 117. Best score: 117.
* [spaceship7] Your score: 94. Best score: 94.
* [spaceship8] Your score: 90. Best score: 90.
* [spaceship9] Your score: 208. Best score: 206.
* [spaceship10] Your score: 304. Best score: 304.
* [spaceship11] Your score: 8192. Best score: 8192.
* [spaceship12] Your score: 8192. Best score: 8192.
* [spaceship13] Your score: 23791. Best score: 23791.
* [spaceship14] Your score: 137. Best score: 137.
* [spaceship15] Your score: 39. Best score: 39.
* [spaceship16] Your score: 1364. Best score: 1358.
* [spaceship17] Your score: 416. Best score: 408.
* [spaceship18] Your score: 1952. Best score: 1765.
* [spaceship19] Your score: 11279. Best score: 11279.
* [spaceship20] Your score: 2358. Best score: 2342.
* [spaceship21] Your score: 2383. Best score: 2376.
* [spaceship22] Your score: 1131. Best score: 1118.
* [spaceship23] Your score: 174317. Best score: 152792.
* [spaceship24] Your score: 735715. Best score: 481573.
* [spaceship25] Your score: 625043. Best score: 489597.
```
</details>

## 3d

This is the problem for which we wrote the most tooling. Besides the obvious simulator,
this includes:

- a TUI, which allows us to select and move parts of the solution. (see `3d/draw/draw.ml`)
  To play with it you can use `dune exec 3d/draw/draw.exe input.3d output.3d`. The input
  file should contain at least 1 empty line first and 2 dots.

- a linker, which allows to specify labels in some cells as `X:0` specifying a cell
  named `X` with `0` as its initial value, and `X@x` and `X@y` used to refer to cells
  in the destination of a time warp. Files with labels are often named with a `.3dl`
  extension. (see `3d/link.ml`)

  This is our solution for 3d5 using it

```
      .      B      .      .     T:      .
    Su:      +      .      0      =      .
      .      .      .      .      .      .
  Acc@x      @  Acc@y      .      +      S
      .      1      .      ^      A      .
      .      .      <  Acc:B      %      .
   Su@x      @   Su@y      .      .      .
      .      1      .    T@x      @    T@y
      .      .      .      .      1      .
```
```
   .   B   .   .   .   .
   .   +   .   0   =   .
   .   .   .   .   .   .
  -2   @  -2   .   +   S
   .   1   .   ^   A   .
   .   .   <   B   %   .
   1   @   5   .   .   .
   .   1   .   0   @   7
   .   .   .   .   1   .
```
  This was a hand written example, but gave us the inspiration for patterns that
  another tool could help us produce on larger problems.

- a compiler from an embedded (very simple) synchronous language, to 3d programs
  that always execute with only two different times. It works by generating a
  given number of widgets, and using a SAT solver to generate a layout packing
  them as closely as possible.

  The code of the the compiler is `3d/compile.ml`, the interpreter is `3d/run_comp.ml`
  and the packing in `3d/layout.ml`

  A solution are in `3d/c3d*.ml` files. To generate a solution for the 3d7 for instance
  run `dune exec 3d/c3d_7.exe`

  Extracted from `3d/c3d_7.ml` here is the gist of how it's used.
```OCaml
(* T1 *)
p "V" ~i:"0" "V1" "V2";
(* p adds a copy widget from the variable V to the variables V1 and V2.
   V is initialized with 0 *)
p "U" ~i:"A" "U1" "U2";

(* T2 *)
p "U2" "U3" "U4";
a "V10" (v "V1") '*' (c "10");
(* This tells that V10 is defined as V1 * 10 *)
a "Umod" (v "U1") '%' (c "10");
a "R" (c "A") '=' (v "V2");

(* T3 *)
a "S" (v "R") '/' (c "A");
a "U" (v "U3") '/' (c "10");
a "V" (v "V10") '+' (v "Umod");
a "U0" (v "U4") '=' (c "0");

(* T4 *)

add_delay "S" (v "U0");
(* delays the output from U0 to S1 by 1 cycle *)

add_out "S"
(* The variable S will be the result *)
```
  It generates our solution which can then be linked.

```
   .    .    <   U2:    >    .    .    .    .      <   U:A      >     .    .     .
U3@x    @ U3@y     A U4@x    @ U4@y U1@x    @   U1@y    10   U2@x     @ U2@y     .
   .    1   R:     /    .    1    .  V2:    1    U1:     %      .     1   10     .
   .    0    .     S    .   10    A    =    .      .     .      .   V1:    *     .
 U4:    =    . Umod:  U3:    /    .    .    . Umod@x     @ Umod@y     .    .     .
   .    . V10:     +    .    .  R@x    @  R@y      .     1      . V10@x    @ V10@y
U0@x    @ U0@y     .  U@x    @  U@y    1    .      <   V:0      >     .    1     .
   .    1  V@x     @  V@y    1    . V1@x    @   V1@y     .   V2@x     @ V2@y     .
   .    .    .     1  U0:    >    S    .    1      .     .      .     1    .     .
```

We wrote the small examples by hand (using only the linked and the editor), using the
compiler for the larger examples. Unfortunately, our compiled solutions hit the time
limit for both 3d8 and 3d11, but we believe they would have beaten the high score at the
end of the competition if they had not.

<details>
<summary>Scores</summary>

```
* [3d1] Your score: 3168. Best score: 2982.
* [3d2] Your score: 1250. Best score: 1250.
* [3d3] Your score: 1372. Best score: 1176.
* [3d4] Your score: 2080. Best score: 1872.
* [3d5] Your score: 2592. Best score: 2592.
* [3d6] Your score: 3240. Best score: 2800.
* [3d7] Your score: 7290. Best score: 7290.
* [3d8] Your score: 37800. Best score: 28210.
* [3d9] Your score: 12760. Best score: 12760.
* [3d10] Your score: 43120. Best score: 40261.
* [3d11] Your score: 264960. Best score: 66792.
* [3d12] Your score: 22440. Best score: 21978.
```
</details>


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
