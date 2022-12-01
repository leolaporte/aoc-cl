;;;; Day01.lisp
;;;; 2022 AOC Day 01 solution
;;;; Leo Laporte, Dec 2022

(ql:quickload '(:fiveam :cl-ppcre :alexandria))

(defpackage :day01
  (:use #:cl
	#:fiveam       ; for inline testing
	#:alexandria   ; lil utils
	#:cl-ppcre))   ; regexp

(in-package :day01)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day01/input.txt")  ; supplied data from AoC

#|
--- Part One ---

Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

|#

(defun calorie-totals (cals-carried)
  "given a list of calories carried by each elf, separated by an empty string,
return the max calories carried by any single elf"
  (do ((data cals-carried (rest data))
       (elf-cals 0)
       (totals nil))
      ((equal data nil) totals)
    (cond
      ((equal (first data) "")
       (push elf-cals totals)
       (setf elf-cals 0))
      (t (incf elf-cals (parse-integer (first data)))))))

(defparameter *test-data* '("1000" "2000" "3000" ""
			    "4000" ""
			    "5000" "6000" ""
			    "7000" "8000" "9000" ""
			    "10000"))

(test calorie-totals-test
  (is (= 24000 (apply #'max (calorie-totals *test-data*)))))


(defun day01-1 (data)
  (let ((cal-list (uiop:read-file-lines *data-file*)))
    (apply #'max (calorie-totals cal-list))))


#|
--- Part Two ---

To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.

|#

(defun day01-2 (data)
  (let* ((cal-list (uiop:read-file-lines *data-file*))
	 (sorted-totals (sort (calorie-totals cal-list) #'>)))
    (+ (first sorted-totals) (second sorted-totals) (third sorted-totals))))

(time (format t "The answer to AOC 2022 Day 01 Part 1 is ~a" (day01-1 *data-file*)))
(time (format t "The answer to AOC 2022 Day 01 Part 2 is ~a" (day01-2 *data-file*)))

;; The answer to AOC 2022 Day 01 Part 1 is 72017
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000396 seconds of total run time (0.000330 user, 0.000066 system)
;; 100.00% CPU
;; 130,544 bytes consed

;; The answer to AOC 2022 Day 01 Part 2 is 212520
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000398 seconds of total run time (0.000345 user, 0.000053 system)
;; 100.00% CPU
;; 196,080 bytes consed
