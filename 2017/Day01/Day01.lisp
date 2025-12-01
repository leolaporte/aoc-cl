;;;; Day01.lisp
;;;; 2017 AOC Day 1 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 30 Nov 2025 at 23:21
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2017.day01
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                      ; pattern matching

(in-package :aoc.2017.day01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2017/Day01/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;
;;                              --- Part One ---
;;
;; LEO'S NOTES:
;;
;; ----------------------------------------------------------------------------


(defparameter *example* )

(defun day01-1
  nil
  )

(5a:test day01-1-test
  (5a:is (= (day01-1 *example*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Two --
;;
;; LEO'S NOTES:
;;
;; ----------------------------------------------------------------------------


(defun day01-2
  nil
  )

(5a:test day01-2-test
  (5a:is (= (day01-2 *example*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2017 Day 1 Part 1 is ~a"
;;	      (day01-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2017 Day 1 Part 2 is ~a"
;;	      (day01-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------
