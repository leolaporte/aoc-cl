;;;; Day14.lisp
;;;; 2022 AOC Day 14 solution
;;;; Leo Laporte, 6 Jan 2022

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day14
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day14)

(setf fiveam:*run-test-when-defined* t) ; test when compiling
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day14/input.txt")  ; AoC input file

#| ------------------------ Day 14: Regolith Reservoir -------------------------
--- Part One ---

Your scan traces the path of each solid rock structure and reports the x,y
coordinates that form the shape of the path, where x represents distance to the
right and y represents distance down. Each path appears as a single line of text
in your scan. After the first point of each path, each point indicates the end
of a straight horizontal or vertical line to be drawn from the previous point.

The sand is pouring into the cave from point 500,0.

Sand is produced one unit at a time, and the next unit of sand is not produced
until the previous unit of sand comes to rest. A unit of sand is large enough to
fill one tile of air in your scan.

A unit of sand always falls down one step if possible. If the tile immediately
below is blocked (by rock or sand), the unit of sand attempts to instead move
diagonally one step down and to the left. If that tile is blocked, the unit of
sand attempts to instead move diagonally one step down and to the right. Sand
keeps moving as long as it is able to do so, at each step trying to move down,
then down -left, then down-right. If all three possible destinations are
blocked, the unit of sand comes to rest and no longer moves, at which point the
next unit of sand is created back at the source.

Using your scan, simulate the falling sand. How many units of sand come to rest
before sand starts flowing into the abyss below?

NOTES:

Seems simple enough. Create a list of rock points, then drop sand along a
trajectory that "flows" around the rock until it gets to a point below the rock
(aka "the abyss"). Return the number of iterations it took to get there.

I think I don't actually have to create a grid. A list of rock points will be
sufficient. Then a function that creates the path of the sand. When the sand
stops falling, add that point to the rock list. I will have to establish the
lowest rock point to know when the abyss begins.  Also important, the supplied
data is in col - row order.

OK list takes too long - about 8 seconds for part 1 - I'm going to try a hash
table instead.  OK yeah it's literally 666 times faster. Hash Tables FTW!
----------------------------------------------------------------------------- |#

;; sample data for tests
(defparameter sample '("498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9")) ; AoC Example
(defparameter *start* (cons 500 0)) ; where the sand grains start from

(defun print-grid (formation)
  "a little utility to display an ASCII version of the formation for debugging"
  (let ((width 200)
	(height 170)
	(offset 480))
    (dotimes (row height)
      (format t "~%")
      (dotimes (col width)
	(cond ((equalp (gethash (cons (+ offset col) row) formation) :rock)
	       (format t "#"))
	      ((equalp (gethash (cons (+ offset col) row) formation) :sand)
	       (format t "o"))
	      ((equalp (gethash (cons (+ offset col) row) formation) :floor)
	       (format t "="))
	      ((equal (cons (+ offset col) row) *start*)
	       (format t "+"))
	      (t (format t ".")))))
    (format t "~&~&~&")))

;; First, build rock formation

(defun col (pt)
  "just to avoid confusion - the column is the first item in a point - points are (cons col row)"
  (car pt))

(defun row (pt)
  "and a row is the second item in a point"
  (cdr pt))

(defun string-to-point (str)
  "given a point in string form 'col,row' return (cons col row)"
  (let ((pt (re:split "," str)))
    (cons (parse-integer (first pt)) (parse-integer (second pt)))))

(5a:test string-to-point-test
  (5a:is (equal (string-to-point "24,25") (cons 24 25)))
  (5a:is (equal (string-to-point "-100,-200") (cons -100 -200))))

(defun build-rock (pt1 pt2)
  "given a pair of points (cons col row) return a list of all the points
in-between inclusive, we do not know if the points are ordered, ASSUME either
row or colum is static"
  (do*
   ;; find the direction we're moving
   ;; is the row increasing? And is it up or down?
   ((r-step (if (= (col pt1) (col pt2))
		(if (< (row pt1) (row pt2)) 1 -1)   ; row grows, but which way?
		0))                                 ; col stays static

    ;; is the col increasing? And is it left or right?
    (c-step (if (zerop r-step)
		(if (< (col pt1) (col pt2)) 1 -1)   ; growth along col, which way?
		0))                                 ; row stays static

    (row (row pt1) (+ row r-step))                  ; add r-step to row each iteration
    (col (col pt1) (+ col c-step))                  ; add c-step to col each iteration

    ;; how many rocks to add
    (rocks (+ (abs (- (row pt1) (row pt2))) (abs (- (col pt1) (col pt2))))
	   (1- rocks))  ; count down to 0

    ;; the list of points
    (points '()))

   ;; terminate
   ((< rocks 0)  points) ; when we've built the rock, return all the points

    ;; loop, add points to the list
    (push (cons col row) points))) ; add a point, repeat until done

(defun rock-pts-list (str)
  "given a string describing a rock formation, return a list of all
points (as (cons col row)) in that formation - no order is promised, and there
will be duplicated points where rocks overlap - but it doesn't matter for our
purposes"
  (let ((pt-strings (map 'list #'string-to-point  (re:split " -> " str)))

	(points '()))
    (dotimes (p (1- (length pt-strings)))
      (setf points (nconc (build-rock (elt pt-strings p) (elt pt-strings (1+ p))) points)))
    points))  ; order is unpredictable, and points will be duplicated where there's overlap

(defun build-formation (list-of-rocks)
  "given a list of strings indicating the shape of a rock structure, create a hash-table of
points occupied by rock"
  (let ((rocks (make-hash-table :test 'equal :size 1000)))  ; a hash-table to hold rock points
    (dolist (description list-of-rocks)                     ; go through data strings
      (dolist (pt (rock-pts-list description))              ; creating a list of points containing rocks
	(setf (gethash pt rocks) :rock)))                   ; add each rock to the array of rocks
    rocks))                                                 ; return the hash

(defparameter form1 (build-formation sample))
(defparameter form2 (build-formation '("500,0 -> 500,5 -> 505,5" "507,5 -> 509,5 -> 509,0")))

(defun abyss (formation)
  "given a rock formation find lowest point of rock - the abyss begins anywhere
lower although because on a grid the HIGHEST row number is the lowest point, we
actually want the highest row"
  (apply #'max (mapcar #'(lambda (f) (row f))
		       (loop for key being the hash-keys of formation collect key))))

(5a:test abyss-test
  (5a:is (= 5 (abyss form2)))
  (5a:is (= 9 (abyss form1))))

;; Now start dropping sand
(defun next-location (loc formation)
  "given the location of a grain of sand, return the next position in its
trajectory"
  (flet  ; some mini functions only used here
      ((pt+ (pt offset) ; adds an offset to a point, returns result
	 (cons (+ (col pt) (col offset)) (+ (row pt) (row offset))))

       (blocked-p (pt formation) ; returns true if the point is blocked
    	 (gethash pt formation)))

    (let ((down (pt+ loc (cons 0 1)))    ; next point down
	  (left (pt+ loc (cons -1 1)))   ; next point down-left
	  (right (pt+ loc (cons 1 1))))  ; next point down-right

      (cond ((not (blocked-p down formation))
	     down)
	    ((not (blocked-p left formation))
	     left)
	    ((not (blocked-p right formation))
	     right)
	    (t loc)))))  ; can't move, return original location

(5a:test next-location-test
  (5a:is (equal (next-location (cons 508 4) form2) (cons 508 4)))    ; stuck
  (5a:is (equal (next-location (cons 501 4) form2) (cons 501 4)))    ; stuck
  (5a:is (equal (next-location (cons 501 0) form2) (cons 501 1)))    ; down
  (5a:is (equal (next-location (cons 507 4) form2) (cons 506 5)))    ; left
  (5a:is (equal (next-location (cons 505 4) form2) (cons 506 5)))    ; right
  (5a:is (equal (next-location (cons 506 4) form2) (cons 506 5))))   ; abyss

(defun drop-grains (start formation)
  "given a rock formation drop grains of sand from starting point until the sand
 falls into the abyss then return the number of grains that got blocked"
  (let ((loc start)               ; starting point of sand grain
	(abyss (abyss formation)) ; lowest point of formation, beyond this is the abyss
	(grains 0))               ; number of grains dropped BEFORE falling through
    (loop
      (let ((next (next-location loc formation))) ; drop
	(cond
	  ((> (row next) abyss) (return grains))   ; we've fallen into the abyss

	  ((equal loc next)                      ; can't move
	   (setf (gethash loc formation) :sand)  ; add stuck grain to formation
	   (setf loc start)                      ; reset loc
	   (incf grains))                        ; drop next grain

	  (t (setf loc next)))))))           ; otherwise, keep dropping

(defun day14-1 (list-of-rocks)
  "given a rock hash, drop grains of sand until they hit the abyss, return the
number of iterations it took to get there"
  (drop-grains *start* (build-formation list-of-rocks)))

(5a:test day14-1-test
  (5a:is (= 24 (day14-1 sample))))

#| -----------------------------------------------------------------------------
--- Part Two ---

assume the floor is an infinite horizontal line with a y coordinate equal to two
plus the highest y coordinate of any point in your scan.

To find somewhere safe to stand, you'll need to simulate falling sand until a
unit of sand comes to rest at 500,0, blocking the source entirely and stopping
the flow of sand into the cave.

NOTES:

So now the abyss limit is abyss + 2. And the stopping point is when the grain
hits 500,0.  I think a simple modification of DROP-GRAINS will work. Oh, I also
have to change next-location to keep grains from going past floor.

And since hash-tables are so fast I might as well simplify things by adding the
floor to the rock points.
----------------------------------------------------------------------------- |#

(defun next-location2 (loc formation)
  "given the location of a grain of sand, return the next position in its
trajectory"
  (flet  ; some mini functions only used here
      ((pt+ (pt offset) ; adds an offset to a point, returns result
	 (cons (+ (col pt) (col offset)) (+ (row pt) (row offset))))

       (blocked-p (pt formation) ; returns true if the point is blocked
	 (gethash pt formation)))

    (let ((down (pt+ loc (cons 0 1)))    ; next point down
	  (left (pt+ loc (cons -1 1)))   ; next point down-left
	  (right (pt+ loc (cons 1 1))))  ; next point down-right

      (cond ((not (blocked-p down formation))
	     down)
	    ((not (blocked-p left formation))
	     left)
	    ((not (blocked-p right formation))
	     right)
	    (t loc))))) ;can't move, return original location

(defun fill-cave (start formation)
  "given a rock formation drop grains of sand from starting point until the sand
fills it up and blocks the start, return the number of grains it took"
  (let ((floor (+ 2 (abyss formation)))) ; point beyond which sand cannot fall
    (dolist (r (rock-pts-list
		(str:concat "0," (write-to-string floor) " -> 1000,"
			    (write-to-string floor))))
      (setf (gethash r formation) :floor)))  ; add a floor structure stretching from 0 to 1000

  (let ((loc start)                     ; starting point of sand grain
	(grains 0))                     ; number of grains dropped BEFORE getting blocked
    (loop
      (let ((next (next-location2 loc formation))) ; drop
	(cond
	  ((equal next start)
	   ;; (print-grid formation)
	   (return (1+ grains)))   ; we've blocked the hole!

	  ((equal loc next)                       ; can't move
	   (setf (gethash loc formation) :sand)   ; add stuck grain to formation
	   (setf loc start)                       ; reset loc
	   (incf grains))                         ; drop next grain

	  (t (setf loc next)))))))                  ; otherwise, keep dropping

(defun day14-2 (list-of-rocks)
  "given a rock hash, drop grains of sand until they plug the origin then return
the number of grains it took"
  (fill-cave *start* (build-formation list-of-rocks)))

(5a:test day14-2-test
  (5a:is (= 93 (day14-2 sample))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 14 Part 1 is ~a"
	      (day14-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2022 Day 14 Part 2 is ~a"
	      (day14-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2022 Day 14 Part 1 is 873
;; Evaluation took:
;; 0.011 seconds of real time
;; 0.011774 seconds of total run time (0.010964 user, 0.000810 system)
;; 109.09% CPU
;; 8,195,040 bytes consed

;; The answer to AOC 2022 Day 14 Part 2 is 24813
;; Evaluation took:
;; 0.359 seconds of real time
;; 0.360661 seconds of total run time (0.329281 user, 0.031380 system)
;; [ Run times consist of 0.029 seconds GC time, and 0.332 seconds non-GC time. ]
;; 100.56% CPU
;; 268,171,696 bytes consed
