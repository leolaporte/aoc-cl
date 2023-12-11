;;;; Day03.lisp
;;;; 2023 AOC Day 03 solution
;;;; Leo Laporte
;;;; 9 December 2023

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day03
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day03)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_03/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 3: Gear Ratios ---
--- Part One ---

The engineer explains that an engine part seems to be missing from the engine,
but nobody can figure out which one. If you can add up all the part numbers in
the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of
the engine. There are lots of numbers and symbols you don't really understand,
but apparently any number adjacent to a symbol, even diagonally, is a "part
number" and should be included in your sum. (Periods (.) do not count as a
symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not
adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number
is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all of
the part numbers in the engine schematic?

NOTES: I think the easiest way to tackle this is to map the schematic text to a grid then it should be simple to test for symbol adjacency. I'll need to find part numbers, get the grid coordinates of the number, then test for adjacency.
----------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))




(5a:test Day03-1-test
  (5a:is (equal (Day03-1 *test-data*) 4361)))

#| -----------------------------------------------------------------------------
--- Part Two ---

------------------------------------------------------------------------------|#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 03 Part 1 is ~a"
;;	      (day03-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 03 Part 2 is ~a"
;;	      (day03-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------
