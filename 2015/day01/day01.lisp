;;;; Day01.lisp
;;;; 2015 AOC Day 1 solution
;;;; Leo Laporte, 22 Sept 2022

(defpackage :day01
  (:import-from :fiveam :test :is) ; testing
  (:use :cl))

(in-package :day01)

#|
--- Day 1: Not Quite Lisp ---
...
An opening parenthesis, (, means he should go up one floor,
and a closing parenthesis, ), means he should go down one floor.
To what floor do the instructions take Santa?
...
|#

(defun day1_1 (str)
  (-
   (length (remove #\) str))    ; just the up moves
   (length (remove #\( str))))  ; just the down moves

(test part-1  ; test Part 1 code (examples from AoC)
  (is (= 0 (day1_1 "(())")))
  (is (= 0 (day1_1 "()()")))
  (is (= 3 (day1_1 "(((")))
  (is (= 3 (day1_1 "(()(()(")))
  (is (= 3 (day1_1 "))(((((")))
  (is (= -1 (day1_1 "())")))
  (is (= -1 (day1_1 "))(")))
  (is (= -3 (day1_1 ")))")))
  (is (= -3 (day1_1 ")())())"))))

#|
--- Part Two ---
...

first character that causes him to enter the basement (floor -1).
The first character in the instructions has position 1.
....
|#

(defun day1_2 (str)
  (do ((i 0 (1+ i))
       (total 0))
      ((< total 0) i)
    (if (equalp #\( (char str i))
	(setf total (1+ total))
	(setf total (1- total)))))

(test part-2 ; test Part 2 code
  (is (= 1 (day1_2 ")")))
  (is (= 5 (day1_2 "()())"))))

(defvar moves (uiop:read-file-line "~/cl/AOC/2015/Day01/input1.txt"))
(time (format t "The answer to AOC 2015 Day 1 Part 1 is ~a" (day1_1 moves)))
(time (format t "The answer to AOC 2015 Day 1 Part 2 is ~a" (day1_2 moves)))

;; wrote /Users/leo/Source/lisp/AOC/2015/Day01/Day01.fasl
;; compilation finished in 0:00:00.004

;; Running test PART-1 .........
;; Did 9 checks.
;; Pass: 9 (100%)
;; Skip: 0 ( 0%)
;; Fail: 0 ( 0%)


;; Running test PART-2 ..
;; Did 2 checks.
;; Pass: 2 (100%)
;; Skip: 0 ( 0%)
;; Fail: 0 ( 0%)

;; The answer to AOC 2015 Day 1 Part 1 is 138
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000229 seconds of total run time (0.000209 user, 0.000020 system)
;; 100.00% CPU
;; 47,216 bytes consed

;; The answer to AOC 2015 Day 1 Part 2 is 1771
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000025 seconds of total run time (0.000025 user, 0.000000 system)
;; 100.00% CPU
;; 0 bytes consed
