;;;; Day09.lisp
;;;; 2015 AOC Day 9 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 18 Nov 2025 at 08:26
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2015.day09
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                      ; pattern matching

(in-package :aoc.2015.day09)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2015/day09/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

LEO'S NOTES:

---------------------------------------------------------------------------- |#

(defparameter *example* )

(defun day09-1
  nil
  )

(5a:test day09-1-test
  (5a:is (= (day09-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two --

LEO'S NOTES:

---------------------------------------------------------------------------- |#

(defun day09-2
  nil
  )

(5a:test day09-2-test
  (5a:is (= (day09-2 *example*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2015 Day 9 Part 1 is ~a"
;;	      (day09-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2015 Day 9 Part 2 is ~a"
;;	      (day09-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------
