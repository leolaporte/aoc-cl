;;;; Day###.lisp
;;;; 2024 AOC Day 01 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started:
;;;; Finished:

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

(defparameter *data-file* "~/cl/AOC/2024/Day_01/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

---------------------------------------------------------------------------- |#



#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2024 Day 01 Part 1 is ~a"
;;	      (day01-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2024 Day 01 Part 2 is ~a"
;;	      (day01-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
