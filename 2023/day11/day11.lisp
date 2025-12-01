;;;; Day11.lisp
;;;; 2023 AOC Day 11 solution
;;;; Leo Laporte
;;;; 23-25 January 2023

;; --------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; --------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day11
  (:use #:cl #:iterate)              ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)                   ; for regex
   (:5a :fiveam)))                   ; for testing

(in-package :day11)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/day11/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 11: Cosmic Expansion ---
--- Part One ---

The researcher has collected a bunch of data and compiled the data
into a single giant image (your puzzle input). The image includes
empty space (.) and galaxies (#).

The researcher is trying to figure out the sum of the lengths of the
shortest path between every pair of galaxies. However, there's a
catch: the universe expanded in the time it took the light from those
galaxies to reach the observatory.

Due to something involving gravitational effects, only some space
expands. In fact, the result is that any rows or columns that contain
no galaxies should all actually be twice as big.

Expand the universe, then find the length of the shortest path between
every pair of galaxies. What is the sum of these lengths?

LEO'S NOTES:

OK two parts to this:

1. Expand the universe. Might be easier to ingest the data as a list
of rows at first, just to make the expansion easier. Then turn it into
a 2D array for part 2.

Take two: now that I've seen part two I'm going to refactor
this. Instead of manually expanding the universe I'm going to just
keep track of the rows and cols that are expanded - I'll call these
WORMHOLES. So now:

1. Collect the galaxy points

2. Collect the wormhole rows and cols

3. Sum the Manhattan distance between all the galaxies. Adding
WORMHOLE-SIZE for every time the route crosses a wormhole.

Seems simple enough. And if it works in part one (which I've already
solved by expanding the universe) it will work in part two!

Except it didn't. The refactored code worked with part one but not
with the provided tests for part two. Hmmm. Why would it work with a
wormhole size of 1 but not 10 or 100? OH! The manhattan distance
already includes the first step into the wormhole! So I'll reduce the
wormhole size by one (for everything except the wormhole size of 1)
and bingo!

The universe is represented in three ways depending on the operation
a list of strings LOS (as provided by the problem set), a list of
lists of characters UNI (needed to rotate the universe), and finally
UNIVERSE a 2D array for the manhattan distances.

--------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("...#......"
    ".......#.."
    "#........."
    ".........."
    "......#..."
    ".#........"
    ".........#"
    ".........."
    ".......#.."
    "#...#....."))

(defconstant +galaxy+ #\#)

(defun make-uni-list (los)
  "given a list of strings return a list of lists of characters (a UNI)"
  (mapcar #'(lambda (l) (coerce l 'list)) los))


(defun make-universe-array (uni)
  "given a list of strings describing a universe turn it into a 2D
array (hereinafter referred to as UNIVERSE)"
  (let* ((width (length (first uni)))
         (height (length uni))
         (universe (make-array (list height width))))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref universe row col) (elt (nth row uni) col))))

    universe))


(defun find-wormholes (uni)
  "given a UNI map return a list of rows that contain wormholes"
  (iter (for row below (length uni))
    (unless (member +galaxy+ (nth row uni)) ; any galaxies in row?
      (collect row))))                      ; no! it's a wormhole

(5a:test find-wormholes-test
  (5a:is (equal (find-wormholes (make-uni-list *test-data*)) (list 3 7))))


(defun rotate-uni (uni)
  "turns a list of list of characters describing a universe on its side
so we can find the wormhole columns"
  (let ((rotated-uni '())
        (new-row '()))

    (iter (for col below (length (first uni)))
      (iter (for row below (length uni))
        (push (nth col (nth row uni)) new-row))
      (push (reverse new-row) rotated-uni)
      (setf new-row '()))
    (reverse rotated-uni)))

(5a:test rotate-universe-test
  (let ((uni (make-uni-list *test-data*)))
    (5a:is (equal (rotate-uni (rotate-uni uni)) uni))
    (5a:is (equal (rotate-uni (list '(1 2 3) '(4 5 6) '(7 8 9)))
                  (list '(1 4 7) '(2 5 8) '(3 6 9))))))


(defun collect-galaxies (universe)
  "given a universe map as a 2D array, return a list of all galaxy
locations as (cons row col)"
  (let ((galaxies '()))
    (iter (for row below (array-dimension universe 0))
      (iter (for col below (array-dimension universe 1))
        (when (char= +galaxy+ (aref universe row col))
          (push (cons row col) galaxies))))
    (reverse galaxies)))

(5a:test collect-galaxies-test
  (5a:is (equal (collect-galaxies (parse-universe *test-data*))
                (list (cons 0 3) (cons 1 7) (cons 2 0)
                      (cons 4 6) (cons 5 1) (cons 6 9)
                      (cons 8 7) (cons 9 0) (cons 9 4)))))


(defun manhattan-distance (x y)
  ;; function to return manhattan distance between two points on a grid
  (+ (abs (- (car x) (car y)))
     (abs (- (cdr x) (cdr y)))))


(defun count-wormhole-traverses (x y rows cols)
  "given two points and two lists of row and column indexes, return the
number of times the path between x and y will cross any of the rows or
columns"
  (let ((row-start (min (car x) (car y)))
        (row-end (max (car x) (car y)))
        (col-start (min (cdr x) (cdr y)))
        (col-end (max (cdr x) (cdr y))))

    (+
     (iter (for r from row-start to row-end)
       (summing
        (if (member r rows) 1 0)))
     (iter (for c from col-start to col-end)
       (summing
        (if (member c cols) 1 0))))))

(5a:test count-wormhole-traverses-test
  (5a:is (= (count-wormhole-traverses (cons 4 4) (cons 1 1) '(1 4) '(2 3)) 4)))


(defun add-distances (uni galaxies wormhole-size)
  "given a list of galaxy locations, a UNI map, and the size of the
wormhole, return the sum of all the distances between galaxies"
  (let ((wh-rows (find-wormholes uni))               ; rows containing wormholes
        (wh-cols (find-wormholes (rotate-uni uni)))) ; cols containing wormholes

    ;; loop
    (do ((distances 0)                  ; total distance traveled
         (g galaxies (rest g)))         ; for each galaxy

        ((null g) distances) ; all galaxies accounted for, return total

      ;; loop body - add the inter-galaxy distances
      (incf distances
            (iter (for gp in (rest g))
              (summing (+ (manhattan-distance (first g) gp) ; base distance
                          ;; add wormhole distances
                          (* (count-wormhole-traverses (first g) gp wh-rows wh-cols)
                             (if (= wormhole-size 1)
                                 1
                                 ;; first step is already accounted for in
                                 ;; the manhattan distance
                                 (1- wormhole-size))))))))))


(defun Day11 (los wormhole-size)
  "given a list of strings representing a universe and the size of each
wormhole (1 for part one, 1 million for part two) return the sum total
of the shortest distances between each galaxy in the universe"
  (let* ((uni (make-uni-list los))
         (galaxies (collect-galaxies (make-universe-array uni))))

    (add-distances uni galaxies wormhole-size)))


(5a:test Day11-test
  (5a:is (= (Day11 *test-data* 1) 374))
  (5a:is (= (Day11 *test-data* 10) 1030))
  (5a:is (= (Day11 *test-data* 100) 8410)))

#| -----------------------------------------------------------------------------
--- Part Two ---

Now, instead of the expansion you did before, make each empty row or
column one million times larger. That is, each empty row should be
replaced with 1000000 empty rows, and each empty column should be
replaced with 1000000 empty columns.

LEO'S NOTES:

Uh oh. I'm going to need a way to simplify this. My part 1 solution
clearly won't do. The problem is not figuring the distances - that's
simple aritimetic - it's expanding the universe. There's not enough
memory for an array that big.

So maybe instead of literally expanding the universe, I somehow keep
track of where the expansions are and add that many millions to the
manhattan-distance. Hmmm.

1. Make a list of galaxy locations
2. Make a list of wormhole locations (maybe just rows and cols)
3. Get manhattan distance between galaxies
4. Add a million for every time the path crosses a wormhole
5. Profit!

But wait - I could have done this in part 1. Worth refactoring? Yeah,
and it works when the wormhole size is 1 but it's too high
otherwise. Sounds like the wormholes mess up the "shortest distance" -
is it not always the manhattan distance?

------------------------------------------------------------------------------|#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 11 Part 1 is ~a"
              (day11 (uiop:read-file-lines *data-file*) 1)))

(time (format t "The answer to AOC 2023 Day 11 Part 2 is ~a"
              (day11 (uiop:read-file-lines *data-file*) 1000000)))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM

;; The answer to AOC 2023 Day 11 Part 1 is 9233514
;; Evaluation took:
;; 0.176 seconds of real time
;; 0.176196 seconds of total run time (0.176071 user, 0.000125 system)
;; 100.00% CPU
;; 1,167,312 bytes consed

;; The answer to AOC 2023 Day 11 Part 2 is 363293506944
;; Evaluation took:
;; 0.181 seconds of real time
;; 0.180788 seconds of total run time (0.180628 user, 0.000160 system)
;; 100.00% CPU
;; 1,290,048 bytes consed
