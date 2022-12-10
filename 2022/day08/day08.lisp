;;;; Day08.lisp
;;;; 2022 AOC Day 08 solution
;;;; Leo Laporte, 09 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day08
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day08)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day08/input.txt")  ; supplied data from AoC

#| ----------------------------------------------------------------------------------------------------

--- Day 8: Treetop Tree House ---

--- Part One ---

Consider your map; how many trees are visible from outside the grid?

NOTES:
The naive solution would be to expand out from each point in the grid - if we hit the edge
in all four directions count it. Terminate the minute we hit a higher number in any direction.
I can speed it up a bit by getting all the values in any direction then checking if the point's
value is higher than the values up, down, left, right.

First thing is to parse the input (and decide if I want to represent the points as a single
array as I did last year (which makes addressing a calculation) or as an x.y coordinate
in a 2D array. Let's do 2D this year.

---------------------------------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("30373"
    "25512"
    "65332"
    "33549"
    "35390"))

(defconstant +x-dim+ 1) ; to prevent stupid misteaks
(defconstant +y-dim+ 0)

(defstruct pt  x y) ; a point in a 2D array

(defun make-grid (input-strings)
  "turn a list of strings into a 2D array"
  (let* ((x-dim (length (first input-strings)))
	 (y-dim (length input-strings))
	 (grid (make-array (list y-dim x-dim))))
    (dotimes (y y-dim)
      (dotimes (x x-dim)
	(setf (aref grid y x)
	      (- (char-code (char (elt input-strings y) x)) 48)))) ; convert # string to integer
    grid))

(5a:test make-grid-test
  (5a:is (equalp #2A((3 0 3 7 3) (2 5 5 1 2) (6 5 3 3 2) (3 3 5 4 9) (3 5 3 9 0))
		 (make-grid *test-data*))))

(defun edge? (pt grid)
  "returns true if point is on an edge"
  (or (= (pt-x pt) 0)
      (= (pt-y pt) 0)
      (= (pt-x pt) (1- (array-dimension grid +x-dim+)))
      (= (pt-y pt) (1- (array-dimension grid +y-dim+)))))

(5a:test edge?-test
  (let ((grid (make-grid *test-data*)))
    (5a:is-true  (edge? (make-pt :x 0 :y 0) grid))
    (5a:is-true  (edge? (make-pt :x 4 :y 0) grid))
    (5a:is-false (edge? (make-pt :x 1 :y 1) grid))
    (5a:is-true (edge? (make-pt :x 4 :y 4) grid))
    (5a:is-true (edge? (make-pt :x 3 :y 0) grid))
    (5a:is-true (edge? (make-pt :x 0 :y 2) grid))
    (5a:is-true (edge? (make-pt :x 0 :y 4) grid))))

(defun val (pt grid)
  "returns the value at pt"
  (aref grid (pt-y pt) (pt-x pt)))

(5a:test val-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (= 3 (val (make-pt :x 0 :y 0) grid)))
    (5a:is (= 7 (val (make-pt :x 3 :y 0) grid)))
    (5a:is (= 5 (val (make-pt :x 2 :y 1) grid)))
    (5a:is (= 4 (val (make-pt :x 3 :y 3) grid)))))

(defun above (pt grid)
  "returns a list of values of points above pt"
  (loop for y from (1- (pt-y pt)) downto 0                ; from above y to the top edge
	collect (val (make-pt :x (pt-x pt) :y y) grid)))  ; get the values of the points

(5a:test above-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (equalp '(5 3)
		   (above (make-pt :x 2 :y 2) grid)))))

(defun below (pt grid)
  "returns a list of values of points below pt"
  (loop for y from (1+ (pt-y pt)) below (array-dimension grid +y-dim+) ; from just below y to edge
	collect (val (make-pt :x (pt-x pt) :y y) grid)))               ; get the values of the points

(5a:test below-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (equalp '(5 3)
		   (below (make-pt :x 2 :y 2) grid)))))

(defun left (pt grid)
  "returns a list of values of points left pt"
  (loop for x from (1- (pt-x pt)) downto 0                ; from just left of x to the left edge
	collect (val (make-pt :x x :y (pt-y pt)) grid)))  ; get the values of the points

(5a:test left-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (equalp '(5 6)
		   (left (make-pt :x 2 :y 2) grid)))))

(defun right (pt grid)
  "returns a list of values of points right pt"
  (loop for x from (1+ (pt-x pt)) below (array-dimension grid +x-dim+) ; from the right of x to edge
	collect (val (make-pt :x x :y (pt-y pt)) grid)))               ; get the values of the points

(5a:test right-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (equalp '(3 2)
		   (right (make-pt :x 2 :y 2) grid)))))

(defun visible? (pt grid)
  "Returns true if a point has no higher points in at least one of up, down, left, right"
  (or (edge? pt grid) ; points on the edge are visible
      (> (val pt grid) (apply #'max (above pt grid)))
      (> (val pt grid) (apply #'max (below pt grid)))
      (> (val pt grid) (apply #'max (left pt grid)))
      (> (val pt grid) (apply #'max (right pt grid)))))

(defun count-visible (grid)
  "count how many trees in the grid can be seen from the outside"
  (let ((total 0))
    (dotimes (y (array-dimension grid +y-dim+))
      (dotimes (x (array-dimension grid +x-dim+))
	(incf total
	      (if (visible? (make-pt :x x :y y) grid)
		  1
		  0))))
    total))

(defun day08-1 (input-strings)
  (count-visible (make-grid input-strings)))

(5a:test day08-1-test
  (5a:is (= 21 (day08-1 *test-data*))))

#|
--- Part Two ---

A tree's scenic score is found by multiplying together its viewing distance in each of
the four directions.

Consider each tree on your map. What is the highest scenic score possible for any tree?

NOTES: this took longer than it should have for two reasons: I didn't preserve the order
of the trees in part 1 because I didn't have to. In part two the order of the trees is
obviously critical. And two, I keep trying to use t as a variable name. D'oh!

|#

(defun scenic-score (pt grid)
  "returns a view score for the point in the grid - the score is the number of unobscured positions
multiplied together"
  (flet ((measure-view (tree-list)   ; local function to count the tree view in one direction
	   (let ((tree-height (val pt grid)))    ; the height of the current tree
	     (do ((tr tree-list (rest tr))       ; step through the list of values
		  (cnt 0))                       ; count how many trees we can see
		 ((null tr) cnt)                 ; when we're done with the list, return the count
	       (incf cnt)                        ; so there's at least 1 tree, increment the count
	       (when (>= (first tr) tree-height) ; we've reached a blocking tree
		 (return cnt))))))               ;  stop! return the count

    (*
     (measure-view (above pt grid))            ; how many trees can we see above us?
     (measure-view (right pt grid))            ; ditto to the right
     (measure-view (left pt grid))             ; and left
     (measure-view (below pt grid)))))         ; and down

(5a:test scenic-score-test
  (let ((grid (make-grid *test-data*)))
    (5a:is (= 4 (scenic-score (make-pt :x 2 :y 1) grid)))
    (5a:is (= 8 (scenic-score (make-pt :x 2 :y 3) grid)))))

(defun make-scenic-score-list (grid)
  "make a list of all the scenic scores"
  (loop for y below (array-dimension grid +y-dim+)
	append (loop for x below (array-dimension grid +x-dim+)
		     collect (scenic-score (make-pt :x x :y y) grid))))

(defun day08-2 (input-strings)
  (apply #'max (make-scenic-score-list (make-grid input-strings)))) ; return the best scenic score

(5a:test day08-2-test
  (5a:is (= 8 (day08-2 *test-data*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 08 Part 1 is ~a"
	      (day08-1 (uiop:read-file-lines *data-file*))))


(time (format t "The answer to AOC 2022 Day 08 Part 2 is ~a"
	      (day08-2 (uiop:read-file-lines *data-file*))))

;; --------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; --------------------------------------------------------------------------------

;; The answer to AOC 2022 Day 08 Part 1 is 1812
;; Evaluation took:
;; 0.045 seconds of real time
;; 0.045333 seconds of total run time (0.038123 user, 0.007210 system)
;; [ Run times consist of 0.003 seconds GC time, and 0.043 seconds non-GC time. ]
;; 100.00% CPU
;; 81,859,280 bytes consed

;; The answer to AOC 2022 Day 08 Part 2 is 315495
;; Evaluation took:
;; 0.047 seconds of real time
;; 0.048237 seconds of total run time (0.038165 user, 0.010072 system)
;; [ Run times consist of 0.005 seconds GC time, and 0.044 seconds non-GC time. ]
;; 102.13% CPU
;; 92,897,648 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 8       >24h  75284      0       >24h  69823      0
;; 7       >24h  79100      0       >24h  77516      0
;; 6   01:02:38  19233      0   01:07:16  18804      0
;; 5   03:01:38  23370      0   03:55:49  26420      0
;; 4   01:01:11  15964      0   01:16:38  16172      0
;; 3   00:42:32  12585      0   01:17:33  13957      0
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
