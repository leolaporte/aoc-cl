;;;; Day14.lisp
;;;; 2022 AOC Day 14 solution
;;;; Leo Laporte, 14 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day14
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day14)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day14/input.txt")  ; supplied data from AoC

#| ----------------------------------------------------------------------------------------------------
--- Day 14: Regolith Reservoir ---
--- Part One ---

Your scan traces the path of each solid rock structure and reports the x,y coordinates that form the
shape of the path, where x represents distance to the right and y represents distance down. Each path
appears as a single line of text in your scan. After the first point of each path, each point
indicates the end of a straight horizontal or vertical line to be drawn from the previous point.

The sand is pouring into the cave from point 500,0.

Sand is produced one unit at a time, and the next unit of sand is not produced until the previous unit
of sand comes to rest. A unit of sand is large enough to fill one tile of air in your scan.

A unit of sand always falls down one step if possible. If the tile immediately below is blocked (by
rock or sand), the unit of sand attempts to instead move diagonally one step down and to the left. If
that tile is blocked, the unit of sand attempts to instead move diagonally one step down and to the
right. Sand keeps moving as long as it is able to do so, at each step trying to move down, then down
-left, then down-right. If all three possible destinations are blocked, the unit of sand comes to rest
and no longer moves, at which point the next unit of sand is created back at the source.

Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts
flowing into the abyss below?

NOTES: Seems simple enough. Create an array of rock points, then drop sand along a trajectory
that "flows" around the rock until it gets to a point below the rock
(aka "the abyss"). Return the number of iterations it took to get there.

I think I don't actually have to create a grid. A vector of rock points will be sufficient. Then
a function that creates the path of the sand. When the sand stops falling, add that point to
the roch vector. I will have to establish the lowest rock point to know when the abyss begins.
Also important, the supplied data is in col - row order.

---------------------------------------------------------------------------------------------------- |#

(defparameter *sample* '("498,4 -> 498,6 -> 496,6" "503,4 -> 502,4 -> 502,9 -> 494,9")) ; AoC Example

(defun col (pt)
  (car pt))

(defun row (pt)
  (cdr pt))

(defun make-rock-vector (list-of-rocks)
  "given a list of strings indicating the shape of a rock structure, create a vector of
points occupied by rock"
  (let ((rocks (make-array 5000 :fill-pointer 0 :adjustable t)))
    (dolist (rock list-of-rocks)
      (dolist (pt (rock-pts-list rock))
	(vector-push pt rocks)))
    rocks))

(defun rock-pts-list (str)
  "given a string describing a rock formation, return a list of all points (as (cons row col))
in that formation"
  (let ((pt-strings (re:split " -> " str)))
    ()

    ))

(defun extend-points (pt1 pt2)
  "given a pair of points (cons col row) return a list of all the points in between inclusive"
  (let ())
  (loop
    for col from (col start) upto (col end)
    for row from (row start) upto (row end)
    collect (cons col row)))

(defun string-to-point (str)
  "given a point in string form 'col,row' return (cons col row)"
  (let ((pt (re:split "," str)))
    (cons (parse-integer (col pt)) (parse-integer (row pt)))))

(defun drop-grain (rocks)
  "given a rock hash drop a grain of sand return a new rock hash with the sand added, or
:abyss when the sand falls into the abyss")

(defun next-location (sand rocks)
  "given the location of a grain of sand, return the next position in its trajectory")

(defun day14-1 (rocks)
  "given a rock hash, drop grains of sand until they hit the abyss, return
the number of iterations it took to get there")

(5a:test day14-1-test
  (5a:is (= 24 (day14-1 *sample*))))

#| ----------------------------------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2022 Day 14 Part 1 is ~a"
;;	      (day14-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2022 Day 14 Part 2 is ~a"
;;	      (day14-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ----------------------------------------------------------------------------------------------------
