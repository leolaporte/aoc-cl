;;;; Day01.lisp
;;;; 2022 AOC Day 01 solution
;;;; Leo Laporte, 1 Dec 2022

(ql:quickload '(:fiveam))

(defpackage :day01
  (:use #:cl
	#:fiveam))      ; for inline testing

(in-package :day01)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day01/input.txt")  ; supplied data from AoC

#|
--- Part One ---

Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

NOTES: As is often the case the only challenge here is how to parse the data. FILE-READ-LINES reads
the file into a list of integer strings separated by an empty string (that's how it interprets
the blank line). The obvious solution is nested for loops - one to total the groups, one to
stuff the totals into a list. DO loop to the rescue.

This is a uniquely lispy idiom I use all the time. The DO has three clauses, the first sets the
local variables (and advances them each time through the loop), the second is the termination
case with a return value, the third is the body of the loop.
|#

(defun calorie-totals (list-of-cals)
  "given a list of calories carried by each elf, separated by an empty string,
return the list of total calories carried by each elf sorted from greatest to lowest"
  (do ((loc list-of-cals (rest loc))      ; the input data, a list of calorie strings
       (cals-per-elf 0)                   ; total cals per elf
       (totals nil))                      ; list of totals for each elf

      ;; termination condition (empty loc)
      ((equal loc nil) (sort totals #'>)) ; done? return the sorted list of totals

    ;; body of DO loop
    (if (equal (first loc) "")            ; inter-elf separator reached
	(progn
	  (push cals-per-elf totals)      ; save the total to the totals list
	  (setf cals-per-elf 0))          ; and clear the tote for the next elf
	;; else
	(incf cals-per-elf (parse-integer (first loc)))))) ; add this calorie count to elf's total

(defparameter *test-data* '("1000" "2000" "3000" ""
			    "4000" ""
			    "5000" "6000" ""
			    "7000" "8000" "9000" ""
			    "10000"))

(test calorie-totals-test
  (is (= 24000 (first (calorie-totals *test-data*)))))

(defun day01-1 (f)
  "given a data file containing a list of integer string groups separated by empty strings return
the highest group total"
  (first (calorie-totals (uiop:read-file-lines f)))) ; read input file, return maximum calorie total

#|
--- Part Two ---

To avoid this unacceptable situation, the Elves would instead like to know the total
Calories carried by the top three Elves carrying the most Calories. That way, even
if one of those Elves runs out of snacks, they still have two backups.

|#

(defun day01-2 (f)
  "returns the three highest calorie totals from a list of calorie counts"
  (apply #'+ (subseq (calorie-totals (uiop:read-file-lines f)) 0 3))) ; add the top three

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
