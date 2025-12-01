;;;; Day01.lisp
;;;; 2021 AOC Day 01 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day01
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2021/day01/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

"Each line is a measurement of the sea floor depth as the sweep looks further
and further away from the submarine. Count the number of times a depth
measurement increases from the previous measurement.

How many measurements are larger than the previous measurement?"

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("199"
    "200"
    "208"
    "210"
    "200"
    "207"
    "240"
    "269"
    "260"
    "263"))

(defun count-increases (lon)
  "given a list of numbers return the number of times the next number is larger
than the current number"
  (cond ((null (rest lon)) 0)
        (t (+ (if (> (first (rest lon)) (first lon)) 1 0)
              (count-increases (rest lon))))))

(defun Day01-1 (depth-list)
  "given a list of number strings return the number of numbers in the list that
are larger than the previous number"
  (count-increases (mapcar #'parse-integer depth-list)))

(5a:test Day01-1-test
  (5a:is (= 7 (Day01 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"consider sums of a three-measurement sliding window. Count the number of
times the sum of measurements in this sliding window increases from the
previous sum."

---------------------------------------------------------------------------- |#

(defun count-increases-by-threes (lon)
  "given a list of numbers return the number of times the some of the next
three numbers is larger than the current  number"
  (cond ((null (fourth lon)) 0)
        (t (let ((group1 (+ (first lon) (second lon) (third lon)))
                 (group2 (+ (second lon) (third lon) (fourth lon))))
             (+ (if (> group2 group1) 1 0)
                (count-increases-by-threes (rest lon)))))))

(defun Day01-2 (depth-list)
  "given a list of number strings return the number of numbers in the list
that are larger than the previous number"
  (count-increases-by-threes (mapcar #'parse-integer depth-list)))

(5a:test Day01-2-test
  (5a:is (= 5 (Day01-2 *example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2021 Day 01 Part 1 is ~a"
              (Day01-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2021 Day 01 Part 2 is ~a"
              (Day01-2 (uiop:read-file-lines *data-file*))))

#| --------------------------------------------------------------------------
Timings with SBCL on an M4 Pro Mac mini with 64GB RAM


The answer to AOC 2021 Day 01 Part 1 is 1342
Evaluation took:
0.000 seconds of real time
0.000283 seconds of total run time (0.000249 user, 0.000034 system)
100.00% CPU
196,560 bytes consed

The answer to AOC 2021 Day 01 Part 2 is 1378
Evaluation took:
0.000 seconds of real time
0.000298 seconds of total run time (0.000287 user, 0.000011 system)
100.00% CPU
131,056 bytes consed
-------------------------------------------------------------------------- |#
