;; [[file:Day02.org::*Package Setup][Package Setup:1]]
;;;; Day02.lisp
;;;; 2017 AOC Day 2 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 05 December 2025
;;;; Finished:

(defpackage :aoc.2017.day02
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2017.day02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2017/Day02/input.txt"
  "Downloaded from the AoC problem set")
;; Package Setup:1 ends here

;; [[file:Day02.org::*Example Data][Example Data:1]]
(defparameter *example* (list $7))
;; Example Data:1 ends here

;; [[file:Day02.org::*Solution][Solution:1]]
(sr:-> day02-1 (list) number)
(defun day02-1 (input)
  "Solve Part 1 of Day 2."

  )
;; Solution:1 ends here

;; [[file:Day02.org::*Test][Test:1]]
(5a:test day02-1-test
  (5a:is (= 0 (day02-1 *example*))))
;; Test:1 ends here

;; [[file:Day02.org::*Run Part 1][Run Part 1:1]]
;; Uncomment to run:
;; (time (format t "The answer to AOC 2017 Day 2 Part 1 is ~a"
;;               (day02-1 (uiop:read-file-lines *data-file*))))
;; Run Part 1:1 ends here

;; [[file:Day02.org::*Solution][Solution:1]]
(sr:-> day02-2 (list) number)
(defun day02-2 (input)
  "Solve Part 2 of Day 2."
  0
  )
;; Solution:1 ends here

;; [[file:Day02.org::*Test][Test:1]]
(5a:test day02-2-test
  (5a:is (= 0 (day02-2 *example*))))
;; Test:1 ends here

;; [[file:Day02.org::*Run Part 2][Run Part 2:1]]
;; Uncomment to run:
;; (time (format t "The answer to AOC 2017 Day 2 Part 2 is ~a"
;;               (day02-2 (uiop:read-file-lines *data-file*))))
;; Run Part 2:1 ends here

;; [[file:Day02.org::*Performance][Performance:1]]
;; Timings with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; Performance:1 ends here
