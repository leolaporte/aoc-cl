;;;; Day03.lisp
;;;; 2022 AOC Day 03 solution
;;;; Leo Laporte, 3 Dec 2022

(ql:quickload '(:fiveam))

(defpackage :day03
  (:use #:cl
	#:fiveam))         ; for inline testing

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

(defparameter *test1* "vJrwpWtwJgWrhcsFMMfFFhFp"
  "provided AOC examples")
(defparameter *test2* "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
(defparameter *test3* "PmmdzqPrVvPwwTWBwg")
(defparameter *test4* "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
(defparameter *test5* "ttgJtRGJQctTZtZT")
(defparameter *test6* "CrZsJsPPZsGzwwsLwLmpwMDw")
(defparameter *test-all* (list *test1* *test2* *test3* *test4* *test5* *test6*))

(defun common-item (str)
  "return the single character common to both halves of a string - assume there's exactly one"
  (let* ((half (/ (length str) 2))                    ; find middle of string
	 (left (coerce (subseq str 0 half) 'list))    ; split into left and right...
	 (right (coerce (subseq str half) 'list)))    ; ...and coerce into a list of char
    (car (intersection left right))))                 ; get the single char in both left and right

(test common-item-test
  (is (equal #\p (common-item *test1*)))
  (is (equal #\L (common-item *test2*)))
  (is (equal #\P (common-item *test3*)))
  (is (equal #\v (common-item *test4*)))
  (is (equal #\t (common-item *test5*)))
  (is (equal #\s (common-item *test6*))))

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
  (reduce #'+ (mapcar #'priority (mapcar #'common-item rucksacks))))

(test day03-1-test
  (is (= 157 (day03-1 *test-all*))))

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
    (let ((r1 (coerce (first r) 'list))         ; convert each of the next three rucksacks
	  (r2 (coerce (second r) 'list))        ; into lists of chars
	  (r3 (coerce (third r) 'list)))
      (incf total (priority (car (intersection (intersection r1 r2) r3)))))))

(test day03-2-test
  (is (= 70 (day03-2 *test-all*))))

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
