;;;; Day12.lisp
;;;; 2023 AOC Day 12 solution
;;;; Leo Laporte
;;;; 25 January 2023

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day12
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day12)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_12/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 12: Hot Springs ---
--- Part One ---

For each row, the condition records show every spring and whether it
is operational (.) or damaged (#). This is the part of the condition
records that is itself damaged; for some springs, it is simply
unknown (?) whether the spring is operational or damaged.

However, the engineer that produced the condition records also
duplicated some of this information in a different format! After the
list of springs for a given row, the size of each contiguous group of
damaged springs is listed in the order those groups appear in the
row. This list always accounts for every damaged spring, and number
each is the entire size of its contiguous group (that is, groups are
always separated by at least one operational spring: #### would always
be 4, never 2,2).

For each row, count all of the different arrangements of operational
and broken springs that meet the given criteria. What is the sum of
those counts?

LEO'S NOTES: The structure of the program is easy, it's writing the
code to count the possible arrangements that's tough.

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("???.### 1,1,3"
    ".??..??...?##. 1,1,3"
    "?#?#?#?#?#?#?#? 1,3,1,6"
    "????.#...#... 4,1,1"
    "????.######..#####. 1,6,5"
    "?###???????? 3,2,1"))

(defun count-possibles (str)
  "given a string reporting the condition of s series of springs, return
the number of all possible arrangements of operational and broken
springs"
  0)


(5a:test count-possibles-test
  (5a:is (= (count-possibles (nth 0 *test-data*)) 1))
  (5a:is (= (count-possibles (nth 1 *test-data*)) 4))
  (5a:is (= (count-possibles (nth 2 *test-data*)) 1))
  (5a:is (= (count-possibles (nth 3 *test-data*)) 1))
  (5a:is (= (count-possibles (nth 4 *test-data*)) 4))
  (5a:is (= (count-possibles (nth 5 *test-data*)) 10)))


(defun Day12-1 (los)
  "given a list of strings reflecting each sprint condition return the
sum of possible arrangements of operational and broken springs"
  (cond ((null los) 0)
        (t (+ (count-possibles (first los))
              (Day12-1 (rest los))))))


(5a:test Day12-1-test
  (5a:is (Day12-1 *test-data*) 21))


#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 12 Part 1 is ~a"
;;               (day12-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 12 Part 2 is ~a"
;;	      (day12-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
