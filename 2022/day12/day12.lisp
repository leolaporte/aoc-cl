;;;; Day12.lisp
;;;; 2022 AOC Day 12 solution
;;;; Leo Laporte, 26 Dec 2022

;; -----------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day12
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day12)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code
(declaim (optimize (debug 3)))          ; max debugging info

#| -----------------------------------------------------------------------------------

--- Day 12: Hill Climbing Algorithm ---

--- Part One ---

the elevation of each square of the grid is given by a single lowercase letter,
where a is the lowest elevation, b is the next-lowest, and so on up to the
highest elevation, z.

Also included on the heightmap are marks for your current position (S) and the
location that should get the best signal (E). Your current position (S) has
elevation a, and the location that should get the best signal (E) has elevation z.

You'd like to reach E, but to save energy, you should do it in as few steps as
possible. During each step, you can move exactly one square up, down, left, or
right. To avoid needing to get out your climbing gear, the elevation of the
destination square can be at most one higher than the elevation of your current
square; that is, if your current elevation is m, you could step to elevation n,
but not to elevation o.

*** (This also means that the elevation of the destination
square can be much lower than the elevation of your current square.) ***

NOTES: Well that's a problem. If it can be *lower* it will be easy to go
backwards, so there will be many infinite loops. I will have to quickly
reject routes that get out of hand. In other words, Dijkstra.

Also, just a note to myself Array addresses are row,col - not x y. So
I'll do everything row/col.
----------------------------------------------------------------------------------- |#

;; -----------------------------------------------------------------------------------
;; DATA
;; -----------------------------------------------------------------------------------

(defparameter *data-file* "~/cl/AOC/2022/day12/input.txt")  ; supplied data from AoC

;; AOC example data
(defparameter *tst*
  '("Sabqponm"
    "abcryxxl"
    "accszExk"
    "acctuvwj"
    "abdefghi"))

(defconstant +START+ -1)  ; value of start position
(defconstant +END+ 26)    ; value of end position
(defconstant +ROW+ 0)     ; Lisp array refs start with ROW
(defconstant +COL+ 1)     ; then COL

(defun create-grid (los)
  "given a list of strings returns a two-dimensional array of chars coverted to numbers
by subtracting the char-code of #\a, a is 0, b is 1, S and E are special values"
  (let* ((col-dim (length (first los)))           ; each string is COL wide
	 (row-dim (length los))                   ; there are as many strings as ROW
	 (arr (make-array (list row-dim col-dim))))
    (dotimes (row row-dim)                        ; now go through all the rows
      (dotimes (col col-dim)                      ; and cols pupulating the array
	(setf (aref arr row col)
	      (height-to-num (elt (nth row los) col)))))  ; with height values
    arr))

(defun height-to-num (height)
  "turns a height letter into a digit - a=0 b=1 etc."
  (cond ((equal height #\S) +START+)  ; make start -1 so look-around works
	((equal height #\E) +END+)    ; make end 26 for the same reason
	(t (- (char-code height) (char-code #\a))))) ; convert a-z to 0-25

(5a:test create-grid-test
  (5a:is (equalp (create-grid '("abcde" "fghiS" "jklmE"))
		 #2A((0 1 2 3 4)(5 6 7 8 -1)(9 10 11 12 26))))
  (5a:is (equalp (create-grid *tst*)
		 #2A((-1 0 1 16 15 14 13 12)
		     (0 1 2 17 24 23 23 11)
		     (0 2 2 18 25 26 23 10)
		     (0 2 2 19 20 21 22 9)
		     (0 1 3 4 5 6 7 8)))))

(defparameter test (create-grid *tst*)) ; for tests

;; ----------------------------------------------------------------------------------
;; MAIN
;;-----------------------------------------------------------------------------------

(defun row (pt)
  "a synonym for car just so I don't get confused"
  (car pt))

(defun col (pt)
  "a synonym for cdr"
  (cdr pt))

(defun on-grid-p (pt grid)
  "returns true if point is on grid"
  (and (> (array-dimension grid +ROW+) (row pt) -1)
       (> (array-dimension grid +COL+) (col pt) -1)))

(defun get-value (grid pt)
  "returns the value at a point"
  (aref grid (row pt) (col pt)))

(5a:test get-value-test
  (5a:is (= -1 (get-value test (cons 0 0))))
  (5a:is (= 17 (get-value test (cons 1 3))))
  (5a:is (= 26 (get-value test (cons 2 5)))))

(defun find-val (grid val)
  "returns the coordinates of the point that matches val"
  (loop named outer for row below (array-dimension grid +ROW+) do
    (loop for col below (array-dimension grid +COL+) do
      (when (= (aref grid row col) val)
	(return-from outer (cons row col))))))

(5a:test find-val-test
  (5a:is (equal (find-val test +START+) (cons 0 0)))
  (5a:is (equal (find-val test +END+) (cons 2 5))))

(defun list-surrounds (grid pt)
  "given a point on a grid returns a list of surrounding points we can move to
in UP DOWN LEFT RIGHT order"
  (labels ((add-points (p1 p2)
	     (cons (+ (row p1) (row p2)) (+ (col p1) (col p2))))

	   (scalable-p (dest)  ; can it be climbed?
	     (let ((val (get-value grid dest)))
	       (<= val (1+ (get-value grid pt)))))

	   (valid-p (dest)
	     (and (on-grid-p dest grid)  ; is it on the grid?
		  (scalable-p dest))))   ; and not too high to climb

    (remove-if-not #'valid-p ; remove invalid surrounds
		   ;; create list of surrounding points
		   (loop for
			 offset in (list (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1))
			 collect (add-points pt offset)))))

(5a:test list-surrounds-test
  (5a:is (equal (list-surrounds test (cons 0 0)) (list (cons 1 0) (cons 0 1))))
  (5a:is (equal (list-surrounds test (cons 4 2))
		(list (cons 3 2) (cons 4 1) (cons 4 3))))
  (5a:is (equal (list-surrounds test (cons 4 7)) (list (cons 3 7) (cons 4 6)))))

;; it all happens here
(defun find-shortest-path (start end grid)
  "uses Dijkstra's algorithm to find the shortest distance from start to end on the grid,
returns the distance"

  ;; first make a hash of every point using an "infinite" distance to start
  (let* ((grid-size (* (array-dimension grid +ROW+) (array-dimension grid +COL+)))
	 (distances (make-hash-table :size grid-size :test 'equal)))
    (dotimes (row (array-dimension grid +ROW+))
      (dotimes (col (array-dimension grid +COL+))
	(setf (gethash (cons row col) distances) grid-size)))

    (do
     ;; set up locals for do loop
     ((curr-pos start)      ; where we are so far
      (distance 0)          ; distance traveled along best route
      (visited '(start))    ; lists of seen points
      (q '()))              ; min-priority queue

     ;; all done?
     ((equal curr-pos end) distance)  ; return distance traveled

      ;; body of do loop
      ;; first, check all the neighbors of the current position
      (dolist (neighbor (remove-if #'(lambda (pt) (find pt visited)) ; if unvisited
				   (list-surrounds grid curr-pos)))  ; get neighbors

	(let ((new-dist (1+ distance)))    ; we're always just one away fron neighbor
	  (when (< new-dist (gethash neighbor distances))  ; is this a better route?
	    (setf (gethash neighbor distances) new-dist)   ; save as new dist to here
	    (push (cons neighbor new-dist) q))))           ; save new best

      ;; now process the next closest point
      (when (null q) (return 9999)) ; dead end, return MAX value (for pt 2)
      (setf q (sort q #'(lambda (x y) (< (cdr x) (cdr y)))))  ; re-sort queue
      (let ((next-nearest (pop q)))            ; point with the lowest distance so far
	(setf curr-pos (car next-nearest))     ; make it the current point
	(setf distance (cdr next-nearest))     ; the distance to the nearest point
	(push curr-pos visited)))))            ; add the point to the visited list

(defun day12-1 (grid)
  "returns the shortest path from +START+ to +END+ on grid"
  (find-shortest-path (find-val grid +START+) (find-val grid +END+) grid))

(5a:test day12-1-test
  (5a:is (= 31 (day12-1 test))))

#| -----------------------------------------------------------------------------------
--- Part Two ---

"Find the shortest path from ANY square at elevation a to the square marked E."

NOTES: This is simple. Make a list of all squares with elevation "a" then
run FIND-SHORTEST-PATH on each and return the smallest.

One little problem - some of the start points could be dead ends - I need to account
for those. I'll modify FIND-SHORTEST-PATH to abandon the search when there
are no more points in the q - and return "infinity"

Turns out there are 831 "a" points in the problem set and nearly all of them
are dead ends. Maybe I could speed this up by short-circuiting those paths?

----------------------------------------------------------------------------------- |#

(defun find-a-pts (grid)
  "return a list of points in grid with elevation a"
  (let ((matches '()))
    (dotimes (row (array-dimension grid +ROW+))
      (dotimes (col (array-dimension grid +COL+))
	(when (= (get-value grid (cons row col)) (height-to-num #\a)) ; found an "a"
	  (push (cons row col) matches))))
    matches))

(defun day12-2 (grid)
  "returns the shortest path from any starting points at elevation a, or +START+, to +END"
  (let* ((end (find-val grid +END+))
	 (routes (loop for start in (find-a-pts grid)
		       collect (find-shortest-path start end grid))))
    (reduce #'min (push (day12-1 grid) routes)))) ; add dist fron +START+ just in case

(5a:test day12-2-test
  (5a:is (= 29 (day12-2 test))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 12 Part 1 is ~a"
	      (day12-1 (create-grid (uiop:read-file-lines *data-file*)))))

(time (format t "The answer to AOC 2022 Day 12 Part 2 is ~a"
	      (day12-2 (create-grid (uiop:read-file-lines *data-file*)))))

;; -----------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; -----------------------------------------------------------------------------------
;; The answer to AOC 2022 Day 12 Part 1 is 361
;; Evaluation took:
;; 0.256 seconds of real time
;; 0.257123 seconds of total run time (0.255447 user, 0.001676 system)
;; 100.39% CPU
;; 2,226,352 bytes consed

;; The answer to AOC 2022 Day 12 Part 2 is 354
;; Evaluation took:
;; 10.974 seconds of real time (OUCH - maybe a real min-priority-q would be faster)
;; 10.976534 seconds of total run time (10.855514 user, 0.121020 system)
;; [ Run times consist of 0.013 seconds GC time, and 10.964 seconds non-GC time. ]
;; 100.03% CPU
;; 271,699,280 bytes consed
