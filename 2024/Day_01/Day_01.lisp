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

LEO'S NOTES: Streamed this solution live at
https://youtu.be/4q8jfZnRq30?si=iBwbknm8Xafu7WcD

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("3   4"
    "4   3"
    "2   5"
    "1   3"
    "3   9"
    "3   3"))

(defun parse-data (los)
  "takes a list of strings, each with two numbers, and returns two sorted
lists containing the left and right digits in the strings"
  (let ((left '())
        (right '()))

    (dolist (digits los)
      (let ((nums (re:split "\\s+" digits)))         ; split into two numbers
        (push (parse-integer (first nums)) left)     ; create a list for
        (push (parse-integer (second nums)) right))) ; each number col

    (values (sort left #'<) (sort right #'<)))) ; return sorted lists

(defun Day_01-1 (los)
  "return the sum of the differences between the left and right
 columns in a list of strings, each string containing two numbers"
  (multiple-value-bind (left right) (parse-data los) ; get each column as list
    (iter ; walk the lists
      (for l in left)
      (for r in right)
      (summing (abs (- l r)))))) ; getting the sum of the differences

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
  "given a list of strings with two numbers in each, return the sum of
 the product of the first number in each column with the number of
 times that number shows up in the second column"
  (multiple-value-bind (left right) (parse-data los)
    (let ((result 0))
      (dolist (l left)
        (setf result (+ result (* l (count l right)))))
      result)))

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
