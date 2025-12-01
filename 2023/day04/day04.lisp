;;;; Day04.lisp
;;;; 2023 AOC Day 04 solution
;;;; Leo Laporte
;;;; 15-16 December 2023

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

(defparameter *data-file* "~/cl/AOC/2023/day04/input.txt"
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

(defun parse-card (scratchcard)
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
    (multiple-value-bind (winners hand) (parse-card scratchcard)
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

There's no such thing as "points". Instead, scratchcards only cause you to win
more scratchcards equal to the number of winning numbers you have.

Specifically, you win copies of the scratchcards below the winning card equal to
the number of matches. So, if card 10 were to have 5 matching numbers, you would
win one copy each of cards 11, 12, 13, 14, and 15.

Copies of scratchcards are scored like normal scratchcards and have the same
card number as the card they copied. So, if you win a copy of card 10 and it has
5 matching numbers, it would then win a copy of the same cards that the original
card 10 won: cards 11, 12, 13, 14, and 15. This process repeats until none of
the copies cause you to win any more cards. (Cards will never make you copy a
card past the end of the table.)

Process all of the original and copied scratchcards until no more scratchcards
are won. Including the original set of scratchcards, how many total scratchcards
do you end up with?

NOTES:
Looks like I'm starting over. This time I need to parse each card into a record:
(defstruct card: number copies winners hand) As I pass through the list of
scratchcards I increment subsequent cards in the records to make copies. When I
get to a card I process it 'copies number of times. When I get to the end of the
list of scratchcard records I total all the COPIES for the answer.

It works but it's SLOW! Oh well. It works.
------------------------------------------------------------------------------|#

(defstruct card
  (number 0 :type integer) ; the card number as provided
  (copies 1 :type integer) ; number of copies of the card
  (winners '() :type list) ; the list of winning numbers
  (hand '() :type list))   ; the numbers held by this card

(defun parse-scratchcard (scratchcard)
  "given a string representing a scratchcard return a record for that card"
  (let* ((parts (re:split "\\|" scratchcard))
         (left-nums (re:all-matches-as-strings "\\d+" (first parts)))
         (right-nums (re:all-matches-as-strings "\\d+" (second parts))))
    (make-card :number (parse-integer (first left-nums))
               :winners (rest left-nums)
               :hand right-nums)))

(defun get-scratchcard (card scratchrecs)
  "returns the record of the scratchcard number specified"
  (find-if #'(lambda (struct) (= (card-number struct) card)) scratchrecs))

(defun incf-copies (card scratchrecs)
  "bumps the number of copies of given card in list-of-cards by 1"
  (incf (card-copies (get-scratchcard card scratchrecs))))

(defun count-winners (card scratchrecs)
  "returns the number of wins for card in scratchrecs"
  (let ((cardrec (get-scratchcard card scratchrecs))
        (wins 0))
    (dolist (c (card-hand cardrec) wins) ; for each card in the hand
      (when (member c (card-winners cardrec) :test 'string=)
        (incf wins))))) ; count the wins

(5a:test count-winners-test
  (let ((scratchers (mapcar #'parse-scratchcard *test-data*)))
    (5a:is (= (count-winners 1 scratchers) 4))
    (5a:is (= (count-winners 2 scratchers) 2))
    (5a:is (= (count-winners 3 scratchers) 2))
    (5a:is (= (count-winners 4 scratchers) 1))
    (5a:is (= (count-winners 5 scratchers) 0))
    (5a:is (= (count-winners 6 scratchers) 0))))

(defun Day04-2 (list-of-scratchcards)
  (let ((scratchers (mapcar #'parse-scratchcard list-of-scratchcards)))

    ;; for every card in the stack
    (loop for s from 1 upto (length scratchers)
          ;; and for every copy of that card
          do (loop for i from 1 upto (card-copies (get-scratchcard s scratchers))
                   ;; add a copy for the following WINNER number of cards
                   do (loop for c from (1+ s)
                              upto (+ s (count-winners s scratchers))
                            do (incf-copies c scratchers))))

    ;; then go through the cards, adding the copies
    (loop for s from 1 upto (length scratchers)
          summing (card-copies (get-scratchcard s scratchers)))))

(5a:test Day04-2-test
  (5a:is (equal (day04-2 *test-data*) 30)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 04 Part 1 is ~a"
              (day04-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 04 Part 2 is ~a"
	      (day04-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------


;; The answer to AOC 2023 Day 04 Part 1 is 20829
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001714 seconds of total run time (0.001520 user, 0.000194 system)
;; 200.00% CPU
;; 720,352 bytes consed

;; The answer to AOC 2023 Day 04 Part 2 is 12648035
;; Evaluation took:
;; 38.776 seconds of real time
;; 38.714259 seconds of total run time (38.694085 user, 0.020174 system)
;; 99.84% CPU
;; 523,552 bytes consed
