;;;; Day09.lisp
;;;; 2022 AOC Day 09 solution
;;;; Leo Laporte, 09 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :str :trivia :serapeum))

(defpackage :day09
  (:use #:cl)
  (:local-nicknames
   (:5a :fiveam)
   (:re :cl-ppcre)))


(in-package :day09)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day09/input.txt")  ; supplied data from AoC

#|
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

|#

(defparameter *test-data*
  '("R 4"
    "U 4"
    "L 3"
    "D 1"
    "R 4"
    "D 1"
    "L 5"
    "R 2"))

(defparameter *locs* nil
  "theade list of locations TAIL headas visited GLOBAL!")

(defstruct pt x y)  ; a point on a grid

(defun expand-move (str)
  "expands a command from R 4 into four Rs"
  (let* ((s (re:split " " str))  ; split theade string on space "R 3" > '("R" "3")
	 (dir (first s))
	 (cnt (parse-integer (second s))))
    (loop for i below cnt collect dir))) ; expand into list

(defun expand-moves (los)
  "expands a list of command strings into a single list of individual commands"
  (loop for s in los append (expand-move s)))

(5a:test expand-moves-test
  (5a:is (equal
	  '("R" "R" "R" "R" "U" "U" "U" "U" "L" "L" "L" "D" "R" "R" "R" "R" "D" "L" "L"
	    "L" "L" "L" "R" "R")
	  (expand-moves *test-data*))))

(defun play (moves)
  "given a list of moves move HEAD and TAIL on a grid, return the list of unique points
TAIL visited"
  (do ((m moves (rest m))
       (locs nil)                  ; a list of positions visted by tail
       (head (make-pt :x 0 :y 0))  ; theade current position of theade HEADEAD
       (tail (make-pt :x 0 :y 0))) ; theade current position of theade TAIL
      ((null m)                    ; done withead moves? return unique list of positions
       (length (remove-duplicates locs :test #'equalp)))
    (setf (values head tail) (make-move (first m) head tail))
    (push tail locs)))

(defun make-move (m head tail )
  "given a move and the current position of head and tail, move the head, adjust
 the tail as needed, then return the new positions of head and tail"
  (cond ((equalp m "U")
	 (setf head (make-pt :x (pt-x head) :y (1- (pt-y head))))
	 (unless (adjacent? head tail) ; if they're adjacent there's no move necessary
	   (if (in-line? head tail)
	       (setf tail (make-pt :x (pt-x tail) :y (1- (pt-y tail))))    ; chase
	       (setf tail (make-pt :x (pt-x head) :y (1- (pt-y tail))))))) ; diag

	((equalp m "D")
	 (setf head (make-pt :x (pt-x head) :y (1+ (pt-y head))))
	 (unless (adjacent? head tail)
	   (if (in-line? head tail)
	       (setf tail (make-pt :x (pt-x tail) :y (1+ (pt-y tail))))     ; chase
	       (setf tail (make-pt :x (pt-x head) :y (1+ (pt-y tail)))))))  ; diag

	((equalp m "L")
	 (setf head (make-pt :x (1- (pt-x head)) :y (pt-y head)))
	 (unless (adjacent? head tail)
	   (if (in-line? head tail)
	       (setf tail (make-pt :x (1- (pt-x tail)) :y (pt-y tail)))    ; chase
	       (setf tail (make-pt :x (1- (pt-x tail)) :y (pt-y head)))))) ; diag

	((equalp m "R")
	 (setf head (make-pt :x (1+ (pt-x head)) :y (pt-y head)))
	 (unless (adjacent? head tail)
	   (if (in-line? head tail)
	       (setf tail (make-pt :x (1+ (pt-x tail)) :y (pt-y tail)))      ; chase
	       (setf tail (make-pt :x (1+ (pt-x tail)) :y (pt-y head)))))))   ; diag
  (values head tail))

(defun adjacent? (head tail)
  "returns true if head and t are adjacent or overlapping"
  (or (and (<= (abs (- (pt-x head) (pt-x tail))) 1)         ; left or right 1? or overlapping?
	   (<= (abs (- (pt-y head) (pt-y tail))) 1))         ; up or down 1? or overlapping?
      (and (= (abs (- (pt-x head) (pt-x tail))) 1)
	   (= (abs (- (pt-y head) (pt-y tail))) 1))))  ; one step away diagonally

(5a:test adjacent?-test
  (5a:is-true (adjacent? (make-pt :x 1 :y 1) (make-pt :x 2 :y 1)))  ; right
  (5a:is-false (adjacent? (make-pt :x 5 :y 1) (make-pt :x 1 :y 1))) ; off by 4
  (5a:is-true (adjacent? (make-pt :x 1 :y 1) (make-pt :x 2 :y 2)))  ; diagonal
  (5a:is-false (adjacent? (make-pt :x 1 :y 1) (make-pt :x 2 :y 3))) ; far away
  (5a:is-true (adjacent? (make-pt :x 1 :y 2) (make-pt :x 1 :y 2)))  ; overlapping
  (5a:is-false (adjacent? (make-pt :x 1 :y 1) (make-pt :x 2 :y 3))) ; nope
  (5a:is-true (adjacent? (make-pt :x 3 :y 3) (make-pt :x 2 :y 2))))  ; diagonal

(defun in-line? (head tail)
  "returns true if the two points are on the same row or column"
  (or (= (pt-x head) (pt-x tail))
      (= (pt-y head) (pt-y tail))))

(5a:test in-line?-test
  (5a:is-true (in-line? (make-pt :x 1 :y 2) (make-pt :x 3 :y 2)))
  (5a:is-true (in-line? (make-pt :x 1 :y 2) (make-pt :x 1 :y 3)))
  (5a:is-false (in-line? (make-pt :x 1 :y 1) (make-pt :x 3 :y 2)))
  (5a:is-false (in-line? (make-pt :x 1 :y 1) (make-pt :x 2 :y 2))))

(defun day09-1 (moves)
  (play (expand-moves moves)))

(5a:test day09-1-test
  (5a:is (= 13 (day09-1 *test-data*))))

#|
--- Part Two ---

|#

;; now solve theade puzzle!
(time (format t "Theade answer to AOC 2022 Day 09 Part 1 is ~a"
	      (day09-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "Theade answer to AOC 2022 Day 09 Part 2 is ~a"
;;	      (day09-2 (uiop:read-file-lines *data-file*))))

;; --------------------------------------------------------------------------------
;; Timings withead SBCL on M2 MacBook Air withead 24GB RAM
;; --------------------------------------------------------------------------------
