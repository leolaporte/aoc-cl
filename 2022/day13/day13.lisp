;;;; Day13.lisp
;;;; 2022 AOC Day 13 solution
;;;; Leo Laporte, 26 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day13
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day13)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (speed 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day13/input.txt")  ; supplied data from AoC

#| ----------------------------------------------------------------------------------------------------
--- Part One ---

Rules:

1. If both values are integers, the lower integer should come first. If the left integer is
lower than the right integer, the inputs are in the right order. If the left integer is higher
than the right integer, the inputs are not in the right order. Otherwise, the inputs are the
same integer; continue checking the next part of the input.

2. If both values are lists, compare the first value of each list, then the second value,
and so on. If the left list runs out of items first, the inputs are in the right order. If
the right list runs out of items first, the inputs are not in the right order. If the lists
are the same length and no comparison makes a decision about the order, continue checking
the next part of the input.

3. If exactly one value is an integer, convert the integer to a list which contains that
integer as its only value, then retry the comparison. For example, if comparing [0,0,0]
and 2, convert the right value to [2] (a list containing 2); the result is then found by
instead comparing [0,0,0] and [2].

What are the indices of the pairs that are already in the right order? (The first pair has
index 1, the second pair has index 2, and so on.) In the above example, the pairs in the
right order are 1, 2, 4, and 6; the sum of these indices is 13.

Determine which pairs of packets are already in the right order. What is the sum of the
indices of those pairs?
---------------------------------------------------------------------------------------------------- |#

(defparameter *test*
  '("[1,1,3,1,1]"
    "[1,1,5,1,1]"
    ""
    "[[1],[2,3,4]]"
    "[[1],4]"
    ""
    "[9]"
    "[[8,7,6]]"
    ""
    "[[4,4],4,4]"
    "[[4,4],4,4,4]"
    ""
    "[7,7,7,7]"
    "[7,7,7]"
    ""
    "[]"
    "[3]"
    ""
    "[[[]]]"
    "[[]]"
    ""
    "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

;; NOTES these look a lot like lists, I wonder if I could replace [] with () and remove commas
;; then use eval?

(defparameter *left* (re:create-scanner "\\["))
(defparameter *right* (re:create-scanner "\\]"))
(defparameter *comma* (re:create-scanner ","))
(defparameter *digit* (re:create-scanner "\\d+"))

(defun lispify (str)
  "given a string with [] and , turn it into a proper lisp list"
  (re:regex-replace-all *left*
			(re:regex-replace-all *right*
					      (re:regex-replace-all *comma* str " ") ")") "("))



#| ----------------------------------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2022 Day 13 Part 1 is ~a"
;;	      (day13-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2022 Day 13 Part 2 is ~a"
;;	      (day13-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ----------------------------------------------------------------------------------------------------
