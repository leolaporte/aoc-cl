;;;; Day03.lisp
;;;; 2022 AOC Day 03 solution
;;;; Leo Laporte, 3 Dec 2022

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day03
  (:use #:cl
	#:fiveam         ; for inline testing
	#:cl-ppcre))     ; regex

(in-package :day03)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day03/input.txt")  ; supplied data from AoC

#|
--- Day 3: Rucksack Reorganization ---

--- Part One ---

Find the item type that appears in both compartments of each rucksack. What is the
sum of the priorities of those item types?

|#

(defun common-item (str)
  "return the single character common to both halves of a string"
  (let* ((half (/ (length str) 2))                        ; find middle of string
	 (left (concatenate 'list (subseq str 0 half)))   ; split into left and right...
	 (right (concatenate 'list (subseq str half))))   ; and convert into a list of char
    (car (intersection left right))))                     ; get the single char in both left and right

(test common-item-test
  (is (equal #\p (common-item "vJrwpWtwJgWrhcsFMMfFFhFp")))
  (is (equal #\L (common-item "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")))
  (is (equal #\P (common-item "PmmdzqPrVvPwwTWBwg")))
  (is (equal #\v (common-item "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")))
  (is (equal #\t (common-item "ttgJtRGJQctTZtZT")))
  (is (equal #\s (common-item "CrZsJsPPZsGzwwsLwLmpwMDw"))))

(defun priority (c)
  "return the priority of the given character"
  (if (lower-case-p c)
      (- (char-code c) 96)
      (- (char-code c) 38 )))

(test priority-test
  (is (= 16 (priority #\p)))
  (is (= 38 (priority #\L)))
  (is (= 42 (priority #\P)))
  (is (= 22 (priority #\v)))
  (is (= 20 (priority #\t)))
  (is (= 19 (priority #\s))))

(defun day03-1 (rucksacks)
  "given a list of rucksacks return the total priority"
  (let ((priorities 0))
    (dolist (r rucksacks)
      (incf priorities (priority (common-item r))))
    priorities))

(defparameter *test-data*
  '("vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"))

(test day03-1-test
  (is (= 157 (day03-1 *test-data*))))

#|
--- Part Two ---

Find the item type that corresponds to the badges of each three-Elf group.
What is the sum of the priorities of those item types?

|#

(defun day03-2 (rucksacks)
  "given a list of rucksacks return the total priority when grouping by three at a time"
  (do ((r rucksacks (rest (rest (rest r))))          ; step through list three at a time
       (total 0))                                    ; accumulate total of priorities
      ((null r) total)                               ; done? return total

    ;; body of do loop
    (let ((r1 (concatenate 'list (first r)))         ; convert each of the next three rucksacks
	  (r2 (concatenate 'list (second r)))        ; into lists of chars
	  (r3 (concatenate 'list (third r))))
      (incf total (priority (car (intersection (intersection r1 r2) r3)))))))

(test day03-2-test
  (is (= 70 (day03-2 '("vJrwpWtwJgWrhcsFMMfFFhFp"
		       "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
		       "PmmdzqPrVvPwwTWBwg"
		       "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
		       "ttgJtRGJQctTZtZT"
		       "CrZsJsPPZsGzwwsLwLmpwMDw")))))

(time (format t "The answer to AOC 2022 Day 03 Part 1 is ~a" (day03-1 (uiop:read-file-lines *data-file*))))
(time (format t "The answer to AOC 2022 Day 03 Part 2 is ~a" (day03-2 (uiop:read-file-lines *data-file*))))

;; The answer to AOC 2022 Day 03 Part 1 is 8109
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000650 seconds of total run time (0.000569 user, 0.000081 system)
;; 100.00% CPU
;; 261,104 bytes consed

;; The answer to AOC 2022 Day 03 Part 2 is 2738
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.002044 seconds of total run time (0.001965 user, 0.000079 system)
;; 100.00% CPU
;; 260,608 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 3   00:42:32  12585      0   01:17:33  13957      0
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
