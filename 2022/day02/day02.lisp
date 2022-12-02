;;;; Day02.lisp
;;;; 2022 AOC Day 02 solution

;;;; Leo Laporte, Dec 2022

(ql:quickload '(:fiveam :cl-ppcre :alexandria))

(defpackage :day02
  (:use #:cl
	#:fiveam       ; for inline testing
	#:cl-ppcre     ; regex
	#:alexandria)) ; lil utilities

(in-package :day02)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day02/input.txt")  ; supplied data from AoC

#|
--- Day 2: Rock Paper Scissors ---

--- Part One ---

What would your total score be if everything goes exactly according to your strategy guide?

NOTES: Well this might be inelegant but I'm going to create a two-dimensional
array of possible outcomes in rock paper scissors.
|#

(defun play (move-list)
  "given a list of moves, calculate the total score after all moves are played"
  (let ((score 0))            ; this will be the total score
    (dolist (m move-list)     ; work through the list of moves one by one
      (incf score (move m)))  ; score() does the work here, add it to the total
    score))                   ; return the final tally

(defun move (the-move)
  "returns the score after opp and me are played"
  (let* ((m (convert the-move))
	 ;; this is the array I was talking about - did this by hand
	 (scoring (make-array '(3 3) :initial-contents '((3 6 0)
							 (0 3 6)
							 (6 0 3)))))

    (+ (1+ (cdr m))                       ; the points for the move I played
       (aref scoring (car m) (cdr m)))))  ; and addressing into the array for the score

(test move-test
  (is (= 8 (move "A Y")))
  (is (= 1 (move "B X")))
  (is (= 6 (move "C Z"))))

(defun convert (move)
  "converts letter moves into a cons of naturals for accessing the score array"
  (let ((opp (subseq move 0 1))
	(me (subseq move 2 3)))
    (cons
     (cond ((equalp opp "A") 0)
	   ((equalp opp "B") 1)
	   ((equalp opp "C") 2))
     (cond ((equalp me "X") 0)
	   ((equalp me "Y") 1)
	   ((equalp me "Z") 2)))))

(test convert-test
  (is (equal (cons 0 0) (convert "A X")))
  (is (equal (cons 0 1) (convert "A Y")))
  (is (equal (cons 1 0 ) (convert "B X"))))

(test play-test
  (is (= 15 (play '("A Y" "B X" "C Z")))))

(defun day02-1 (f)
  (play (uiop:read-file-lines f)))

#|
--- Part Two ---

The Elf finishes helping with the tent and sneaks back over to you. "Anyway, the second column says how the round needs to end: X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win. Good luck!"

NOTES: I just have to add a step before scoring: convert the instruction into the
appropriate move. Looks like I'll be making another array, this time of moves I
should play according to the instructions. I can use the rest of the code above
for the actual scoring.

|#

(defun play2 (move-list)
  "given a list of moves, calculate the total score after all moves are played (this time adjusting for the second instruction being an outcome instead of a move)"
  (let ((score 0))
    (dolist (m move-list)
      (incf score (move (choose-move m))))  ; choose move based on desired outcome
    score))

(defun choose-move (the-move)
  "given a move in the form of a string with two letters, return the move that I must
play to achieve the desired outcome"
  (let ((mv (convert the-move))        ; turn the letters into array indices
	(opp (subseq the-move 0 1))    ; I'll still need the letter for my opp's move

	;; and now another array with the moves to be made for the desired result
	(which-move (make-array '(3 3)
				:initial-contents '(("Z" "X" "Y")       ; rock: lose/draw/win
						    ("X" "Y" "Z")       ; paper: l/d/w
						    ("Y" "Z" "X")))))   ;scissors: l/d/w
    ;; rebuild the move with with my actual response
    (concatenate 'string opp " " (aref which-move (car mv) (cdr mv)))))

(test choose-move-test
  (is (equalp "A X" (choose-move "A Y")))
  (is (equalp "B X" (choose-move "B X")))
  (is (equalp "C X" (choose-move "C Z"))))

(defun day02-2 (f)
  (play2 (uiop:read-file-lines f)))

(time (format t "The answer to AOC 2022 Day 02 Part 1 is ~a" (day02-1 *data-file*)))
(time (format t "The answer to AOC 2022 Day 02 Part 2 is ~a" (day02-2 *data-file*)))

;; The answer to AOC 2022 Day 02 Part 1 is 14264
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000812 seconds of total run time (0.000642 user, 0.000170 system)
;; 100.00% CPU
;; 850,800 bytes consed

;; The answer to AOC 2022 Day 02 Part 2 is 12382
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001315 seconds of total run time (0.001092 user, 0.000223 system)
;; 100.00% CPU
;; 1,570,992 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
