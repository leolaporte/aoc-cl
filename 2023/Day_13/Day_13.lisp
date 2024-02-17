;;;; Day13.lisp
;;;; 2023 AOC Day 13 solution
;;;; Leo Laporte
;;;; 16 February 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day13
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day13)
(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_13/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 13: Point of Incidence ---
--- Part One ---

"To find the reflection in each pattern, you need to find a perfect reflection across either a horizontal line between two rows or across a vertical line between two columns.

To summarize your pattern notes, add up the number of columns to the left of each vertical line of reflection; to that, also add 100 multiplied by the number of rows above each horizontal line of reflection. In the above example, the first pattern's vertical line has 5 columns to its left and the second pattern's horizontal line has 4 rows above it, a total of 405."
---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("#.##..##."
    "..#.##.#."
    "##......#"
    "##......#"
    "..#.##.#."
    "..##..##."
    "#.#.##.#."
    ""
    "#...##..#"
    "#....#..#"
    "..##..###"
    "#####.##."
    "#####.##."
    "..##..###"
    "#....#..#"))

(5a:test day13-1-test
  (5a:is (= day13-1 *test-data*) 405))


#| ----------------------------------------------------------------------------
--- Part Two ---                        ; ; ;
                                        ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 13 Part 1 is ~a"
;;	      (day13-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 13 Part 2 is ~a"
;;	      (day13-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
