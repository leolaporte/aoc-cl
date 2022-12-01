;;;; Day01.lisp
;;;; 2022 AOC Day 01 solution
;;;; Leo Laporte, 1 Dec 2022

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
return the list of total calories carried by each elf"
  (do ((data cals-carried (rest data))  ; the input data, a list of calorie strings
       (elf-cals 0)                     ; total cals per elf
       (totals nil))                    ; list of totals for each elf
      ((equal data nil) totals)         ; parsed all the data? return the list of totals
    (cond
      ((equal (first data) "")          ; inter-elf separator reached
       (push elf-cals totals)           ; save the total to the totals list
       (setf elf-cals 0))               ; and clear the tote for the next elf
      (t (incf elf-cals (parse-integer (first data)))))))  ; add this calorie count to elf's total

(defparameter *test-data* '("1000" "2000" "3000" ""
			    "4000" ""
			    "5000" "6000" ""
			    "7000" "8000" "9000" ""
			    "10000"))

(test calorie-totals-test
  (is (= 24000 (apply #'max (calorie-totals *test-data*)))))

(defun day01-1 (f)
  "given a data file containing a list of integer string groups separated by empty strings return
the the highest group total"
  (let ((cal-list (uiop:read-file-lines f)))     ; read in data file as a list of lines
    (apply #'max (calorie-totals cal-list))))    ; find maximum calorie total


#|
--- Part Two ---

To avoid this unacceptable situation, the Elves would instead like to know the total Calories carried by the top three Elves carrying the most Calories. That way, even if one of those Elves runs out of snacks, they still have two backups.

|#

(defun day01-2 (f)
  "returns the three highest calorie totals from a list of calorie counts"
  (let* ((cal-list (uiop:read-file-lines f))                    ; read in data file as list of lines
	 (sorted-totals (sort (calorie-totals cal-list) #'>)))  ; calculate totals and sort
    (+ (first sorted-totals) (second sorted-totals) (third sorted-totals)))) ; add the top three

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


;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 1   00:36:07  10562      0   00:46:09  10629      0
