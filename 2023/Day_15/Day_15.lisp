;;;; Day###.lisp
;;;; 2023 AOC Day ### solution
;;;; Leo Laporte

;; ------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ------------------------------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day###
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day###)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_###/input.txt"
  "Downloaded from the AoC problem set")

#| ------------------------------------------------------------------------------------------------
--- Part One ---

------------------------------------------------------------------------------------------------ |#



#| ------------------------------------------------------------------------------------------------
--- Part Two ---

------------------------------------------------------------------------------------------------ |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day ### Part 1 is ~a"
;;	      (day###-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day ### Part 2 is ~a"
;;	      (day###-2 (uiop:read-file-lines *data-file*))))

;; ------------------------------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ------------------------------------------------------------------------------------------------
