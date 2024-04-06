;;;; Day21.lisp
;;;; 2023 AOC Day 21 solution
;;;; Leo Laporte
;;;; Started: 6 April 2024, Petaluma, CA

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day21
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)               ; misc utilities
   (:ax :alexandria)             ; ditto
   (:re :cl-ppcre)               ; regex
   (:tr :trivia)                 ; pattern matching
   (:tp :trivia.ppcre)           ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day21)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_21/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 21: Step Counter ---
--- Part One ---


"he'd like to know which garden plots he can reach with exactly his
remaining 64 steps.

He gives you an up-to-date map (your puzzle input) of his starting
position (S), garden plots (.), and rocks (#).

Starting from the garden plot marked S on your map, how many garden
plots could the Elf reach in exactly 64 steps?"

LEO'S NOTES: Well this looks eerily familiar. I'll use a sparsh hash
here to represent the map because only around half the points matter.





---------------------------------------------------------------------------- |#

(defparameter *test-data*
  "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")




#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 21 Part 1 is ~a"
;;	      (day21-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 21 Part 2 is ~a"
;;	      (day21-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
