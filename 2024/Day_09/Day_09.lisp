;;;; Day09.lisp
;;;; 2024 AOC Day 09 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 2025-06-21
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day09
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day09)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "/Volumes/Data/Source/lisp/AOC/2024/Day_09/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 9: Disk Fragmenter ---
--- Part One ---

"

"Compact the amphipod's hard drive using the process he requested. What is the resulting filesystem checksum?"

LEO'S NOTES:

So the data set is a 20000 character string! I have to read it into
memory because I'm moving files from the end, no sequential reads
here.

What's a good way of representing this data? I don't think I want to
expand it out. It will be gigantic.


---------------------------------------------------------------------------- |#

(defparameter *example* "2333133121414131402")



#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2024 Day 09 Part 1 is ~a"
;;	      (day09-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2024 Day 09 Part 2 is ~a"
;;	      (day09-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
