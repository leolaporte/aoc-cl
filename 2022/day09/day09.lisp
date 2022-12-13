;;;; Day09.lisp
;;;; 2022 AOC Day 09 solution
;;;; Leo Laporte, 10 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day09
  (:use #:cl)
  (:local-nicknames
   (:5a :fiveam)
   (:re :cl-ppcre)))

(in-package :day09)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *file* "~/cl/AOC/2022/day09/input.txt")  ; supplied data from AoC

#| ----------------------------------------------------------------------------------------------------
--- Day 9: Rope Bridge ---
--- Part One ---

"Consider a rope with a knot at each end; these knots mark the head and the tail of the rope.
If the head moves far enough away from the tail, the tail is pulled toward the head.

The head (H) and tail (T) must always be touching (diagonally adjacent and even
overlapping both count as touching):

If the head is ever two steps directly up, down, left, or right from the tail, the tail
must also move one step in that direction so it remains close enough:

Otherwise, if the head and tail aren't touching and aren't in the same row or column,
the tail always moves one step diagonally to keep up.

You just need to work out where the tail goes as the head follows a series of motions.
Assume the head and the tail both start at the same position, overlapping.

After each step, you'll need to update the position of the tail if the step means the
head is no longer adjacent to the tail.

Simulate your complete hypothetical series of motions. How many positions does the tail
of the rope visit at least once?"

NOTES:

Rules:
1. if H is ever two steps U D L or R, T must move 1 step in that direction
2. If the head moves off axis, tail moves 1 diagonally to get back adjacent

First thing I'm going to do is unroll the move list - it will be much easier to
analyze and act on one move at a time. It's run-length encoding. So R 4 is really
RRRR.

---------------------------------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("R 4"
    "U 4"
    "L 3"
    "D 1"
    "R 4"
    "D 1"
    "L 5"
    "R 2"))

(defparameter *test-data-pt2*
  '("R 5"
    "U 8"
    "L 8"
    "D 3"
    "R 17"
    "D 10"
    "L 25"
    "U 20"))

;; ----------------------------------------------------------------------------------------------------
;; PARSE DATA
;; ----------------------------------------------------------------------------------------------------

(defun expand-move (str)
  "expands a command from R 4 into four Rs"
  (let* ((s (re:split " " str))              ; split the string on space "R 3" > '("R" "3")
	 (dir (move-to-dir (uiop:first-char (first s))))  ; turn the letter string into cons
	 (cnt (parse-integer (second s))))   ; turn the number string into an integer
    (loop for i below cnt collect dir)))     ; expand into list

(defun move-to-dir (move)
  "converts the move instructions into directions"
  (ecase move
    (#\U (cons 0 -1))
    (#\D (cons 0 1))
    (#\L (cons -1 0))
    (#\R (cons 1 0))))

(defun expand-moves (los)
  "expands a list of command strings into a single list of individual commands"
  (loop for s in los append (expand-move s)))

(5a:test expand-moves-test
  (5a:is (equal
	  '((1 . 0) (1 . 0) (1 . 0) (1 . 0) (0 . -1) (0 . -1) (0 . -1) (0 . -1) (-1 . 0)
	    (-1 . 0) (-1 . 0) (0 . 1) (1 . 0) (1 . 0) (1 . 0) (1 . 0) (0 . 1) (-1 . 0)
	    (-1 . 0) (-1 . 0) (-1 . 0) (-1 . 0) (1 . 0) (1 . 0))
	  (expand-moves *test-data*))))

;; ----------------------------------------------------------------------------------------------------
;; MOVE PRIMITIVES
;; ----------------------------------------------------------------------------------------------------

;; a point on the grid is a dotted pair: (x . y)

(defun x (point)
  "returns the x value of a point"
  (car point))

(defun y (point)
  "returns the y value of a point"
  (cdr point))

(defun adjacent? (head tail)
  "returns true if head and t are adjacent or overlapping"
  (or (and (<= (abs (- (x head) (x tail))) 1)         ; left or right 1? or overlapping?
	   (<= (abs (- (y head) (y tail))) 1))        ; up or down 1? or overlapping?
      (and (= (abs (- (x head) (x tail))) 1)          ; one step away diagonally
	   (= (abs (- (y head) (y tail))) 1))))

(5a:test adjacent?-test
  (5a:is-true (adjacent? (cons 1 1) (cons 2 1)))  ; right
  (5a:is-false (adjacent? (cons 5 1) (cons 1 1))) ; off by 4
  (5a:is-true (adjacent? (cons 1 1) (cons 2 2)))  ; diagonal
  (5a:is-false (adjacent? (cons 1 1) (cons 2 3))) ; far away
  (5a:is-true (adjacent? (cons 1 2) (cons 1 2)))  ; overlapping
  (5a:is-false (adjacent? (cons 1 1) (cons 2 3))) ; nope
  (5a:is-true (adjacent? (cons 3 3) (cons 2 2))))  ; diagonal

;; prep done, now the actual move functions
(defun move-leader (point dir)
  "moves the HEAD one unit in the direction dir"
  (cons (+ (x point) (x dir))
	(+ (y point) (y dir))))

(5a:test move-leader-test
  (5a:is (equal (cons 5 4) (move-leader (cons 5 5) (cons 0 -1))))
  (5a:is (equal (cons 5 6) (move-leader (cons 5 5) (cons 0 1))))
  (5a:is (equal (cons 4 5) (move-leader (cons 5 5) (cons -1 0))))
  (5a:is (equal (cons 6 5) (move-leader (cons 5 5) (cons 1 0)))))

;; a little bit magical - try to follow along
(defun move-follower (leader follower)
  "returns new follower position, by moving a following segment toward the leader
according to the rules"
  (cond  ((adjacent? leader follower) follower)    ; if they're adjacent there's no move necessary

	 ((= (x leader) (x follower))                        ; both on the x-xis
	  (cons (x follower)                                 ; stay on the x-axis
		(+ (y follower)
		   (if (> (y leader) (y follower)) 1 -1))))  ; move +-1 on the y axix

	 ((= (y leader) (y follower))                        ; both on the y-axis
	  (cons (+ (x follower)
		   (if (> (x leader) (x follower)) 1 -1))    ; move +-1 on the x axis
		(y follower)))                               ; stay on the y-axis

	 ((> (abs (- (x leader) (x follower))) 1)            ; diagonal on the x axis
	  (cons (+ (x follower)                              ; bump one closer on the x axis
		   (if (> (x leader) (x follower)) 1 -1))
		(y leader)))                                 ; jump over to the leader's y axis

	 ((> (abs (- (y leader) (y follower))) 1)            ; diagonal on the y axis
	  (cons (x leader)                                   ; jump over to the leader's x axis
		(+ (y follower)
		   (if (> (y leader) (y follower)) 1 -1))))  ; and move 1 closer

	 ((and (> (abs (- (x leader) (x follower))) 1)       ; pathological case only possible
	       (> (abs (- (y leader) (y follower))) 1))      ; with a longer rope (pt 2)
	  (cons (+ (x follower)                              ;
		   (if (> (x leader) (x follower)) 1 -1))
		(+ (y follower)
		   (if (> (y leader) (y follower)) 1 -1))))

	 (t (error "can't get here"))))

(5a:test move-follower-test
  (5a:is (equal (cons 0 0) (move-follower (cons 0 1) (cons 0 0))))     ; adj  (mine)
  (5a:is (equal (cons 0 1) (move-follower (cons 0 2) (cons 0 0))))     ; in-line (mine)
  (5a:is (equal (cons 2 -2) (move-follower (cons 1 -2) (cons 3 -2))))  ; in-line (AoC)
  (5a:is (equal (cons -1 -1) (move-follower (cons -1 -2) (cons 0 0)))) ; diag (AoC)
  (5a:is (equal (cons 4 -1) (move-follower (cons 4 -2) (cons 3 0))))   ; diag (AoC)
  (5a:is (equal (cons 3 -4) (move-follower (cons 2 -4) (cons 4 -3))))  ; diag (AoC)
  (5a:is (equal (cons 4 -3) (move-follower (cons 3 -4) (cons 4 -3))))  ; adj (AoC)
  (5a:is (equal (cons 3 -3) (move-follower (cons 4 -3) (cons 2 -4))))  ; diag (AoC)
  (5a:is (equal (cons 3 -2) (move-follower (cons 2 -2) (cons 4 -3))))) ; diag (AoC)

;; ----------------------------------------------------------------------------------------------------

;; this is the workhorse function - a factory that processes the moves
;; keeps track of the points TAIL visits and returns the final answer
(defun play (moves)
  "given a list of moves, move HEAD and TAIL on a grid, return the list of unique points
TAIL visited"
  (do
   ;; set up local variables
   ((m moves (rest m))                                     ; loop through the moves
    (head (cons 0 0))                                      ; starting position of HEAD
    (tail (cons 0 0))                                      ; starting position of TAIL
    (visited nil (push tail visited)))                     ; a list of positions visted by tail

   ;; finally
   ((null m)                                               ; done with moves?
    (length (remove-duplicates visited :test #'equal)))    ; return unique list of positions

    ;; repeat body
    (setf head (move-leader head (first m)))               ; move head
    (setf tail (move-follower head tail))))                ; move tail

(defun day09-1 (moves)
  (play (expand-moves moves)))

(5a:test day09-1-test
  (5a:is (= 13 (day09-1 *test-data*))))

#| ----------------------------------------------------------------------------------------------------
                                             --- Part Two ---

Simulate your complete series of motions on a larger rope with ten knots. How many positions does the tail of the rope visit at least once?

NOTES: Same rules different game. Instead of HEAD and TAIL we have a snake of 10 segments. I can use
the same move algorithm I just have to move the head (once) then move the 9 followers sequentially,
then save the TAIL position. Same move rules though, right? So re-write PLAY as PLAY-SNAKE.

---------------------------------------------------------------------------------------------------- |#

(defparameter *segs* 10) ; number of segments in the new rope (call it a snake, shall we?)

(defun play-snake (moves)
  "given a list of moves, move each segment in a snake sequentially, return the list of unique points
the last segment of the snake visited"
  (do
   ;; set up local variables
   ((m moves (rest m))                                  ; for m in moves (updates each loop)
    (snake                                              ; make a snake of SEGS segments
     (loop for s below *segs* collect (cons 0 0)))      ; with all segs at (0,0)
    (visited '()))                                      ; a list of points visted by the tail

   ;; finally
   ((null m)                                            ; out of moves to process?
    (length (remove-duplicates visited :test #'equal))) ; return number of unique points visited

    ;; repeat body - make each move in loop
    (setf (first snake)
	  (move-leader (first snake) (first m))) ; set the leader (there's only one)

    (dotimes (s (1- *segs*))                            ; set all the followers
      (setf (elt snake (1+ s)) (move-follower (elt snake s) (elt snake (1+ s)))))

    (setf visited (cons (elt snake (1- *segs*)) visited))))

(defun day09-2 (moves)
  (play-snake (expand-moves moves)))

(5a:test day09-2-test
  (5a:is (= 36 (day09-2 *test-data-pt2*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 09 Part 1 is ~a"
	      (day09-1 (uiop:read-file-lines *file*))))

(time (format t "The answer to AOC 2022 Day 09 Part 2 is ~a"
	      (day09-2 (uiop:read-file-lines *file*))))

;; --------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; --------------------------------------------------------------------------------
