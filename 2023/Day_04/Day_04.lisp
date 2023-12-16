;;;; Day04.lisp
;;;; 2023 AOC Day 04 solution
;;;; Leo Laporte
;;;; 15- December 2023

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day04
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day04)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_04/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 4: Scratchcards ---
--- Part One ---

"Each card has two lists of numbers separated by a vertical bar (|): a list of
winning numbers and then a list of numbers you have. You organize the
information into a table (your puzzle input).

As far as the Elf has been able to figure out, you have to figure out which of
the numbers you have appear in the list of winning numbers. The first match
makes the card worth one point and each match after the first doubles the point
value of that card.

Take a seat in the large pile of colorful cards. How many points are they worth
in total?"

Note: Uh this seems pretty easy - way too easy. I can do this a line at a time,
each line is standalone.

1. Import the line into two lists WINNERS and HAND.

2. For every number in HAND if it's a member of WINNERS add points

3. The point score is 1 for the first match and the score doubles after each
subsecquent match.

For the THIRD day in a row the test passes but not the final
result. Hmmm. What's the edgecase not represented in the test data? Duh. I
assumed the length of every hand was the same as in the test data. Re-wrote the
scanner to work with any size hand.

-----------------------------------------------------------------------------|#

(defparameter *test-data*
  '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(defun make-winners-and-hand-lists (scratchcard)
  "given a string representing a scratchcard return two lists of number strings:
 WINNERS and HAND"
  (let ((card (re:split "\\|" scratchcard))) ; split into two parts
    (values
     ;; winning cards
     (re:all-matches-as-strings "\\d+" (second (re:split "\\:" (first card))))
     ;; my hand
     (re:all-matches-as-strings "\\d+" (second card)))))

(defun score-hand (scratchcard)
  "given a scratchcard returns the score for the card"
  (let ((score 0)) ; start with no score
    (multiple-value-bind (winners hand) (make-winners-and-hand-lists scratchcard)
      (dolist (card hand score) ; for each card in the hand, returns score
        (when (member card winners :test 'string=) ; winner winner chicken dinner
          (setf score ; update the score
                (if (zerop score) 1 (* 2 score))))))))

(5a:test score-hand-test
  (5a:is (= (score-hand (first *test-data*)) 8))
  (5a:is (= (score-hand (second *test-data*)) 2))
  (5a:is (= (score-hand (third *test-data*)) 2))
  (5a:is (= (score-hand (fourth *test-data*)) 1))
  (5a:is (= (score-hand (fifth *test-data*)) 0))
  (5a:is (= (score-hand (sixth *test-data*)) 0)))

(defun Day04-1 (list-of-scratchcards)
  (loop for s in list-of-scratchcards
        summing (score-hand s)))

(5a:test Day04-1-test
  (5a:is (equal (Day04-1 *test-data*) 13)))

#| -----------------------------------------------------------------------------
--- Part Two ---

------------------------------------------------------------------------------|#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 04 Part 1 is ~a"
	      (day04-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 04 Part 2 is ~a"
;;	      (day04-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------
