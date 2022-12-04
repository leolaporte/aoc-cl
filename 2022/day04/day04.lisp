;;;; Day04.lisp
;;;; 2022 AOC Day 04 solution
;;;; Leo Laporte, 4 Dec 2022

;; Prologue code - just to set up the environment

(ql:quickload '(:fiveam :cl-ppcre :alexandria))

(defpackage :day04
  (:use #:cl
	#:fiveam       ; for inline testing
	#:cl-ppcre     ; regex
	#:alexandria)) ; lil utilities (unused today)

(in-package :day04)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day04/input.txt")  ; supplied data from AoC

#|

--- Day 4: Camp Cleanup ---

--- Part One ---

In how many assignment pairs does one range fully contain the other?

NOTES: this one is surprisingly straightforward using regex and sets

|#

;; I always do this first - for two reasons. It's useful to see what data I'll be
;; working with, and it really helps me to test along the way so I don't go too
;; far down the road with non-working code. It does slow down my solutions,
;; however. I do AoC to learn, not for points. I don't want to learn any bad
;; habits.
(defparameter *tst1* "2-4,6-8")
(defparameter *tst2* "2-3,4-5")
(defparameter *tst3* "5-7,7-9")
(defparameter *tst4* "2-8,3-7")
(defparameter *tst5* "6-6,4-6")
(defparameter *tst6* "2-6,4-8")
(defparameter *tst-all* (list *tst1* *tst2* *tst3* *tst4* *tst5* *tst6*))

(defparameter *pairs* (create-scanner "^(\\d+)-(\\d+),(\\d+)-(\\d+)$")
  "pre-compile the regex used to separate the numbers out of the assignment string,
 e.g. '2-4,6-8' returns four groups, one for each digit")

(defun contained? (pair)
  "returns true if one range in the pair fits entirely into the other
(I've decided that ? is more communicative than -p for predicate function names)"
  (register-groups-bind                            ; love this crazy cl-ppcre regex command
      ((#'parse-integer start1 end1 start2 end2))  ; turn each number string into an int
      (*pairs* pair)                               ; parse the pair string

    ;; now that i've got the start and end points of each range, make them into lists
    (let ((range1 (loop for i from start1 upto end1 collect i))  ; I hate loop but..
	  (range2 (loop for i from start2 upto end2 collect i))) ; ...it's clear and easy
      (or (null (set-difference range1 range2))       ; does 2 fit into 1?
	  (null (set-difference range2 range1))))))   ; or 1 fit into 2?

(test contained?-test
  (is (eq t (contained? *tst4*)))
  (is (eq t (contained? *tst5*)))
  (is (eq nil (contained? *tst1*)))
  (is (eq nil (contained? *tst2*))))

(defun day04-1 (assignments)
  "returns the number of overlapping pairs in the list"
  (length (remove-if-not #'contained? assignments)))

(test day04-1-tst
  (is (= 2 (day04-1 *tst-all*))))

#|
--- Part Two ---

In how many assignment pairs do the ranges overlap?

NOTES: Just change the test to use INTERSECTION instead
of SET-DIFFERENCE

|#

(defun overlap? (pair)
  "returns true if the ranges in the pair overlap at all"
  (register-groups-bind
      ((#'parse-integer start1 end1 start2 end2))
      (*pairs* pair)

    (let ((range1 (loop for i from start1 upto end1 collect i))
	  (range2 (loop for i from start2 upto end2 collect i)))
      (not (null (intersection range1 range2))))))   ; is there any overlap at all?

(test overlap?-test
  (is (eq nil (overlap? *tst1*)))
  (is (eq nil (overlap? *tst2*)))
  (is (eq t (overlap? *tst3*)))
  (is (eq t (overlap? *tst4*)))
  (is (eq t (overlap? *tst5*)))
  (is (eq t (overlap? *tst6*))))

(defun day04-2 (assignments)
  "returns the number of overlapping pairs in the list"
  (length (remove-if-not #'overlap? assignments)))

(test day04-2-test
  (is (= 4 (day04-2 *tst-all*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 04 Part 1 is ~a"
	      (day04-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2022 Day 04 Part 2 is ~a"
	      (day04-2 (uiop:read-file-lines *data-file*))))

;; --------------------------------------------------------------------------------
;; Timings on M2 MacBook Air with 24GB RAM
;; --------------------------------------------------------------------------------
;; The answer to AOC 2022 Day 04 Part 1 is 513
;; Evaluation took:
;; 0.012 seconds of real time
;; 0.012538 seconds of total run time (0.011519 user, 0.001019 system)
;; [ Run times consist of 0.003 seconds GC time, and 0.010 seconds non-GC time. ]
;; 108.33% CPU
;; 2,083,248 bytes consed

;; The answer to AOC 2022 Day 04 Part 2 is 878
;; Evaluation took:
;; 0.005 seconds of real time
;; 0.005122 seconds of total run time (0.004803 user, 0.000319 system)
;; 100.00% CPU
;; 1,953,200 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 4   01:01:11  15964      0   01:16:38  16172      0
;; 3   00:42:32  12585      0   01:17:33  13957      0
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
