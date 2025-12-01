;;;; Day02.lisp
;;;; 2021 AOC Day 02 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started:
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day02
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2021/day02/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 2: Dive! ---
--- Part One ---


---------------------------------------------------------------------------- |#

(defparameter *example*
  '("forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"))

(defun Day02-1 (direction-list)
  (let ((horiz 0)
        (vert 0))
    (dolist (direction direction-list)
      (let* ((dirs (re:split " " direction))
             (dir (first dirs))
             (steps (parse-integer (second dirs))))

        (tr:match dir
          ("forward" (setf horiz (+ horiz steps)))
          ("down" (setf vert (+ vert steps)))
          ("up" (setf vert (- vert steps)))
          (otherwise (error "~%Unknown direction: ~a" dir)))))

    (* horiz vert)))

(5a:test Day02-1-test
  (5a:is (= 150 (Day02-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2021 Day 02 Part 1 is ~a"
              (Day02-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2021 Day 02 Part 2 is ~a"
;;	      (Day02-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
