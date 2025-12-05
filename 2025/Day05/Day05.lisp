;; [[file:Day05.org::*Package Definition][Package Definition:1]]
;;;; Day05.lisp
;;;; 2025 AOC Day 5 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 04 Dec 2025 at 21:00
;;;; Finished: 05 Dec 2025 at 09:45

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day05
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day05)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day05/input.txt"
  "Downloaded from the AoC problem set")
;; Package Definition:1 ends here

;; [[file:Day05.org::*Example Data][Example Data:1]]
(defparameter *example* (list "3-5"
                              "10-14"
                              "16-20"
                              "12-18"
                              ""
                              "1"
                              "5"
                              "8"
                              "11"
                              "17"
                              "32"))
;; Example Data:1 ends here
