;;;; Day10.lisp
;;;; 2024 AOC Day 10 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 02 Nov 2025
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

;; Already loaded in .sbclrc... uncomment to run on your computer
;; (ql:quickload
;;   '(:fiveam :iterate :alexandria :cl-ppcre :trivia :serapeum :str))

(defpackage :aoc.2024.day10
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                      ; pattern matching

(in-package :aoc.2024.day10)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/day10/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Part One ---



---------------------------------------------------------------------------- |#

(defparameter *example* )

(defun day10-1
  nil
  )

(5a:test day10-1-test
  (5a:is (= (day10-1 ))))

#| ----------------------------------------------------------------------------
--- Part Two --

---------------------------------------------------------------------------- |#

(defun day10-2
  nil
  )

(5a:test day10-2-test
  (5a:is (= (day10-2 ))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2024 Day 10 Part 1 is ~a"
;;	      (day10-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2024 Day 10 Part 2 is ~a"
;;	      (day10-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------
