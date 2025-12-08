;;;; Day06.lisp
;;;; 2025 AOC Day 6 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 06 December 2025
;;;; Finished:

(defpackage :aoc.2025.day06
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day06)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day06/input.txt"
  "Downloaded from the AoC problem set")

(defparameter *example* (list "123 328  51 64 "
                              " 45 64  387 23 "
                              "  6 98  215 314"
                              "*   +   *   +  "))

(sr:-> parse-input (list) (values array list))
(defun parse-input (input)
  "given a list of strings, return two values, an array of integers by column and a
list of strings representing operators"
  (let* ((rows (length input))
         (cols (length (sr:words (first input)))) ; actual numbers not spaces
         (digits (make-array cols :element-type 'list :initial-element nil)) ; an array of column integers
         (operands '()))

    (iter (for row below (1- rows))
      (for row-of-nums = (sr:words (nth row input))) ; chop it up first
      (iter (for col below cols)
        (setf (aref digits col)
              ;; stuff this number into the array of columns
              (push (parse-integer (nth col row-of-nums))
                    (aref digits col)))))

    (setf operands (sr:tokens (nth (1- rows) input))) ; last line

    ;; return array of digits and list of operands
    (values digits operands)))

(multiple-value-bind (digits operands) (parse-input *example*)
  (format t "Digits: ~S~%Operands: ~S~%" digits operands))

(sr:-> day06-1 (list) number)
(defun day06-1 (input)
  (multiple-value-bind (digits operands) (parse-input input)

    (iter (for col below (length digits))
      (summing
        (ecase (char (nth col operands) 0)
          (#\+ (apply #'+ (aref digits col)))
          (#\* (apply #'* (aref digits col))))))))


(5a:test day06-1-test
  (5a:is (= 4277556 (day06-1 *example*))))
