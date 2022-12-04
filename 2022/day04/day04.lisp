;;;; Day04.lisp
;;;; 2022 AOC Day 04 solution
;;;; Leo Laporte, Dec 2022

(ql:quickload '(:fiveam :cl-ppcre :alexandria))

(defpackage :day04
  (:use #:cl
	#:fiveam       ; for inline testing
	#:cl-ppcre     ; regex
	#:alexandria)) ; lil utilities

(in-package :day04)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day04/input.txt")  ; supplied data from AoC

#|

--- Day 4: Camp Cleanup ---

--- Part One ---

In how many assignment pairs does one range fully contain the other?

|#

(defparameter *tst1* "2-4,6-8")
(defparameter *tst2* "2-3,4-5")
(defparameter *tst3* "5-7,7-9")
(defparameter *tst4* "2-8,3-7")
(defparameter *tst5* "6-6,4-6")
(defparameter *tst6* "2-6,4-8")
(defparameter *tst-all* (list *tst1* *tst2* *tst3* *tst4* *tst5* *tst6*))

(defparameter *pairs* (create-scanner "^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$")
  "pre-digest the regex to take apart the assignment string, e.g. '2-4,6-8'")

(defun contained-p (pair)
  "returns true if one range in the pair fits entirely into the other"
  (register-groups-bind      ; love this crazy cl-ppcre regex command
      ((#'parse-integer start1 end1 start2 end2))  ; turn each number string into an int
      (*pairs* pair)                               ; parse the pair string

    ;; now that i've got the start and end points of each range, make them into lists
    (let ((range1 (loop for i from start1 upto end1 collect i))
	  (range2 (loop for i from start2 upto end2 collect i)))
      (or (null (set-difference range1 range2))       ; does 2 fit into 1?
	  (null (set-difference range2 range1))))))   ; or 1 fit into 2?

(test contained-p-test
  (is (eq t (contained-p *tst4*)))
  (is (eq t (contained-p *tst5*)))
  (is (eq nil (contained-p *tst1*)))
  (is (eq nil (contained-p *tst2*))))

(defun day04-1 (assignments)
  "get a list of pairs and count how many completely overlap"
  (length (remove-if-not #'contained-p assignments)))

(test day04-1-tst
  (is (= 2 (day04-1 *tst-all*))))

#|
--- Part Two ---

In how many assignment pairs do the ranges overlap?

|#

(defun overlap-p (pair)
  "returns true if the ranges in the pair overlap at all"
  (register-groups-bind
      ((#'parse-integer start1 end1 start2 end2))
      (*pairs* pair)

    (let ((range1 (loop for i from start1 upto end1 collect i))
	  (range2 (loop for i from start2 upto end2 collect i)))
      (not (null (intersection range1 range2))))))

(test overlap-p-test
  (is (eq nil (overlap-p *tst1*)))
  (is (eq nil (overlap-p *tst2*)))
  (is (eq t (overlap-p *tst3*)))
  (is (eq t (overlap-p *tst4*)))
  (is (eq t (overlap-p *tst5*)))
  (is (eq t (overlap-p *tst6*))))

(defun day04-2 (assignments)
  (length (remove-if-not #'overlap-p assignments)))

(test day04-2-text
  (is (= 4 (day04-2 *tst-all*))))


(time (format t "The answer to AOC 2022 Day 04 Part 1 is ~a" (day04-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2022 Day 04 Part 2 is ~a" (day04-2 (uiop:read-file-lines *data-file*))))


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
