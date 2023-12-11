
;;;; Day01.lisp
;;;; 2023 AOC Day 01 solution
;;;; Leo Laporte

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day01
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_01/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------

Day 1: Trebuchet?!

Part One

On each line, the calibration value can be found by combining the first digit
and the last digit (in that order) to form a single two-digit number.

NB: Some strings have only one digit - that digit becomes the first
and last. Some strings have more than two and some have exactly
two. So...

1. make a list of all the digits in each string
2. combine first and last digit from resulting list to make a number
3. read the input strings and sum the numbers

----------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("1abc2"
    "pqr3stu8vwx"
    "a1b2c3d4e5f"
    "treb7uchet"))

(defun extract-digits (input-string)
  "returns all the digits in a string as a list of strings"
  (re:all-matches-as-strings "\\d" input-string))

(defun string-to-number (input-string)
  "returns an integer composed of the first and last digits in a str"
  (let ((digits (extract-digits input-string)))  ; get the digit list
    (+ (* 10 (parse-integer (first digits)))     ; tens place
       (parse-integer (car (last digits))))))    ; ones place

(5a:test string-to-number-test
         (5a:is (equal (string-to-number (first *test-data*)) 12))
         (5a:is (equal (string-to-number (second *test-data*)) 38))
         (5a:is (equal (string-to-number (third *test-data*)) 15))
         (5a:is (equal (string-to-number (fourth *test-data*)) 77)))

(defun Day_01-pt1 (list-of-strings)
  "given a list of strings return the sum of the numbers represented by each string"
  (loop for s in list-of-strings
        sum (string-to-number s)))

(5a:test Day_01-pt1-test
         (5a:is (equal (Day_01-pt1 *test-data*) 142)))


#| -----------------------------------------------------------------------------
Part Two

Your calculation isn't quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six, seven,
eight, and nine also count as valid "digits".

What is the sum of all of the calibration values?

NB: MY first try at this failed because I was looking for the numbers
in numeric order NOT in the order of appearance in the string. So
eightwothree converted to eigh23 instead of the correct 8wo3.

I need to rewrite the replace function to look for the first
occurrence of any number of the string, convert it, then continue
through the remaining string.

OH MF! The tests do not cover something like "eighthree" which I would interpret
as "8hree" but apparently is supposed to be "83". The quick fix is not to
replace the entire text number but just the first letter. And since the overlap
can't be longer than one letter I'll preserve just the last letter of the text
number.
------------------------------------------------------------------------------|#

(defparameter *test-data2*
  '("two1nine"
    "eightwothree"     ; tricky! 823 NOT eigh23 or 8wo3
    "abcone2threexyz"
    "xtwone3four"      ; same problem - should be x2134, not xtw134
    "4nineeightseven2"
    "zoneight234"      ; and here it should be z18234
    "7pqrstsixteen"))

(defun replace-text-numbers-with-digits (input-string)
  "replaces text numbers with the equivalent digits in input-string, moving from
left to right"
  (do* (;; regex to match text numbers 1-9
        (text-num-regex
         (re:create-scanner "one|two|three|four|five|six|seven|eight|nine"))

        ;; alist for digit replacements - note: in order to preserve
        ;; subsequent overlapping text numbers I replace the first
        ;; letters with the digit and keep the last letter on all but
        ;; four and six - which end with letters that can't overlap
        (replacements '(("one" . "1e") ("two" . "2o") ("three" . "3e")
                        ("four" . "4") ("five" . "5e") ("six" . "6")
                        ("seven" . "7n") ("eight" . "8t") ("nine" . "9e")))

        ;; next text number to replace
        (text-num (re:scan-to-strings text-num-regex input-string)
                  (re:scan-to-strings text-num-regex input-string)))

       ((null text-num) input-string) ; no more text numbers - return string

    ;; loop body: replace text number with appropriate digit
    (setf input-string
          (re:regex-replace
           text-num
           input-string
           (cdr (assoc text-num replacements :test #'string=))))))

(5a:test replace-text-numbers-with-digits-test
         (5a:is (equal (replace-text-numbers-with-digits (first *test-data2*)) "2o19e"))
         (5a:is (equal (replace-text-numbers-with-digits (second *test-data2*)) "82o3e"))
         (5a:is (equal (replace-text-numbers-with-digits (third *test-data2*)) "abc1e23exyz"))
         (5a:is (equal (replace-text-numbers-with-digits (fourth *test-data2*)) "x21e34"))
         (5a:is (equal (replace-text-numbers-with-digits (fifth *test-data2*)) "49e8t7n2"))
         (5a:is (equal (replace-text-numbers-with-digits (sixth *test-data2*)) "z18t234"))
         (5a:is (equal (replace-text-numbers-with-digits (seventh *test-data2*)) "7pqrst6teen"))
         (5a:is (equal (replace-text-numbers-with-digits "eighthree") "83e"))
         (5a:is (equal (replace-text-numbers-with-digits "sevenine") "79e")))

(defun Day_01-pt2 (list-of-strings)
  (Day_01-pt1 (mapcar #'replace-text-numbers-with-digits list-of-strings)))

(5a:test Day_01-pt2-test
         (5a:is (equal (Day_01-pt2 *test-data2*) 281)))

;; Now Solve the puzzle!
(time (format t "The answer to AOC 2023 Day 01 Part 1 is ~a"
              (Day_01-pt1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 01 Part 2 is ~a"
              (Day_01-pt2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 01 Part 1 is 54968
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.002064 seconds of total run time (0.001562 user, 0.000502 system)
;; 100.00% CPU
;; 262,016 bytes consed

;; The answer to AOC 2023 Day 01 Part 2 is 54094
;; Evaluation took:
;; 0.021 seconds of real time
;; 0.021143 seconds of total run time (0.018791 user, 0.002352 system)
;; 100.00% CPU
;; 10,350,752 bytes consed
