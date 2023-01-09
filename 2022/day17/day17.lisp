;;;; Day###.lisp
;;;; 2022 AOC Day ### solution
;;;; Leo Laporte, ### Jan 2022

;; ---------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ---------------------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day###
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day###)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info
;; (declaim (optimize (debug 0) (speed 3) (safety 0))) ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2022/day###/input.txt")  ; supplied data from AoC

#| ---------------------------------------------------------------------------------------
--- Part One ---

--------------------------------------------------------------------------------------- |#



#| ---------------------------------------------------------------------------------------
--- Part Two ---

----------------------------------------------------------------------------------------|#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2022 Day ### Part 1 is ~a"
;;	      (day###-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2022 Day ### Part 2 is ~a"
;;	      (day###-2 (uiop:read-file-lines *data-file*))))

;; ---------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ---------------------------------------------------------------------------------------
