;;;; Day06.lisp
;;;; 2023 AOC Day 06 solution
;;;; Leo Laporte
;;;; 3 Jan 2023

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day06
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day06)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_06/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 6: Wait For It ---
--- Part One ---

"Determine the number of ways you could beat the record in each
race. What do you get if you multiply these numbers together?"

LEO'S NOTES: First thing, parse the data. Two strings containing three
numbers each. In line one the numbers represent times, line 2
represents distances. Units do not matter.

Time is a combination of two numbers: press and race which should
combine to make the total time. In this case for race one the total
time is 7 seconds.

T = 7

Press  Speed  Distance
0      0      0
1      1      6
2      2     10
3      3     12
7      7      0

So the relationship of the numbers is
D = (* (- T P) P)

We want to find all values of P that produce D > the record. Or WAYS-TO-WIN
----------------------------------------------------------------------------- |#

(defparameter *test-data*
  (list "Time:      7  15   30"
        "Distance:  9  40  200"))

(Defun parse-input (los)
  "given a pair of strings describing race times and distances
respectively, return a list of time/distance pairs as (cons time dist)"
  (let
      ((times
         (mapcar #'parse-integer
                 (re:all-matches-as-strings "\\d+" (first los))))
       (dists
         (mapcar #'parse-integer
                 (re:all-matches-as-strings "\\d+" (second los)))))

    (loop for time in times
          for dist in dists
          collecting (cons time dist))))

(5a:test parse-input-test
  (5a:is (equal (parse-input *test-data*)
                (list (cons 7 9) (cons 15 40) (cons 30 200)))))

(defun count-ways-to-win (race)
  "given a race as (cons time distance) return the number of ways to win
the race by exceeding the distance"
  (let ((time (car race))
        (dist (cdr race)))

    (loop for press from 0 to time
          count (> (* (- time press) press) dist))))

(defun Day06-1 (los)
  (apply #'* (mapcar #'count-ways-to-win (parse-input los))))

(5a:test Day06-1-test
  (5a:is (= (Day06-1 *test-data*) 288)))

#| -----------------------------------------------------------------------------
--- Part Two ---

Now just remove the spaces from the provided data. There's only ONE race.
------------------------------------------------------------------------------|#

(defun remove-spaces (str)
  "removes all spaces from a string"
  (remove-if (lambda (char) (char= char #\Space)) str))

(defun parse-input2 (los)
  "given a pair of strings describing race times and distances
respectively, return a list of time/distance pairs as (cons time dist)
AFTER removing all spaces!"
  (let
      ((times
         (mapcar #'parse-integer
                 (re:all-matches-as-strings "\\d+"
                                            (remove-spaces (first los)))))
       (dists
         (mapcar #'parse-integer
                 (re:all-matches-as-strings "\\d+"
                                            (remove-spaces (second los))))))

    (loop for time in times
          for dist in dists
          collecting (cons time dist))))

(defun Day06-2 (los)
  (apply #'* (mapcar #'count-ways-to-win (parse-input2 los))))

(5a:test Day06-2-test
  (5a:is (= (Day06-2 *test-data*) 71503)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 06 Part 1 is ~a"
              (day06-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 06 Part 2 is ~a"
              (day06-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 06 Part 1 is 440000
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000063 seconds of total run time (0.000026 user, 0.000037 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to AOC 2023 Day 06 Part 2 is 26187338
;; Evaluation took:
;; 0.363 seconds of real time
;; 0.362942 seconds of total run time (0.362662 user, 0.000280 system)
;; 100.00% CPU
;; 0 bytes consed
