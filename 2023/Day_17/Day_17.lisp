;;;; Day17.lisp
;;;; 2023 AOC Day 17 solution
;;;; Leo Laporte
;;;; 5 March 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day17
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day17)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_17/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 17: Clumsy Crucible ---
--- Part One ---

"Each city block is marked by a single digit that represents the amount
of heat loss if the crucible enters that block. The starting point,
the lava pool, is the top-left city block; the destination, the
machine parts factory, is the bottom-right city block. (Because you
already start in the top-left block, you don't incur that block's heat
loss unless you leave that block and then return to it.)

Because it is difficult to keep the top-heavy crucible going in a
straight line for very long, it can move at most three blocks in a
single direction before it must turn 90 degrees left or right. The
crucible also can't reverse direction; after entering each city block,
it may only turn left, continue straight, or turn right.

Directing the crucible from the lava pool to the machine parts
factory, but not moving more than three consecutive blocks in the same
direction, what is the least heat loss it can incur?"

LEO'S NOTES: Finally pathfinding! And this is a simple Dijkstra except
for the twist: no more than three blocks in a single direction.

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("2413432311323"
    "3215453535623"
    "3255245654254"
    "3446585845452"
    "4546657867536"
    "1438598798454"
    "4457876987766"
    "3637877979653"
    "4654967986887"
    "4564679986453"
    "1224686865563"
    "2546548887735"
    "4322674655533"))






#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 17 Part 1 is ~a"
;;	      (day17-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 17 Part 2 is ~a"
;;	      (day17-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
