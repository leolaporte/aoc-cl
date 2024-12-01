;;;; Day01.lisp
;;;; 2024 AOC Day 01 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 30 Nov 2024 9p
;;;; Finished: 30 Nov 2024 10:10p

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

(defparameter *data-file* "~/common-lisp/AOC/2024/Day_01/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 1: Historian Hysteria ---
--- Part One ---

"Pair up the numbers and measure how far apart they are. Pair up the
smallest number in the left list with the smallest number in the right
list, then the second-smallest left number with the second-smallest
right number, and so on.

To find the total distance between the left list and the right list,
add up the distances between all of the pairs you found.

What is the total distance between your lists?"

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"))

(defun parse-data (los)
  "takes a list of strings, each with two numbers, and returns a list of two lists containing the first and second digits in the strings"
  (let ((first-col '())
        (second-col '()))

    (dolist (digits los)
      (let ((nums (re:split "\\s+" digits)))
        (push (parse-integer (first nums)) first-col)
        (push (parse-integer (second nums)) second-col)))

    (values (sort first-col #'<) (sort second-col #'<))))

(defun Day_01-1 (los)
  (multiple-value-bind (col1 col2) (parse-data los)
    (iter
      (for x in col1)
      (for y in col2)
      (summing (abs (- x y))))))

(5a:test Day_01-1-test
  (5a:is (= 11 (Day_01-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"you'll need to figure out exactly how often each number from the left
list appears in the right list. Calculate a total similarity score by
adding up each number in the left list after multiplying it by the
number of times that number appears in the right list."

---------------------------------------------------------------------------- |#

(defun Day_01-2 (los)
  (multiple-value-bind (col1 col2) (parse-data los)
    (let ((res 0))
      (dolist (x col1)
        (setf res (+ res (* x (count x col2)))))

      res)))

(5a:test Day_01-2-test
  (5a:is (= 31 (Day_01-2 *example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 01 Part 1 is ~a"
              (day_01-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2024 Day 01 Part 2 is ~a"
              (day_01-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2024 Day 01 Part 1 is 1223326
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000646 seconds of total run time (0.000581 user, 0.000065 system)
;; 100.00% CPU
;; 327,536 bytes consed

;; The answer to AOC 2024 Day 01 Part 2 is 21070419
;; Evaluation took:
;; 0.003 seconds of real time
;; 0.003362 seconds of total run time (0.003342 user, 0.000020 system)
;; 100.00% CPU
;; 393,120 bytes consed
