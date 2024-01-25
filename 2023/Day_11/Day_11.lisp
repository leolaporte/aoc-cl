;;;; Day11.lisp
;;;; 2023 AOC Day 11 solution
;;;; Leo Laporte
;;;; 23 January 2023

;; --------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; --------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day11
  (:use #:cl #:iterate)
  (:local-nicknames
   (:re :cl-ppcre)
   (:tr :trivia)
   (:5a :fiveam)))

(in-package :day11)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_11/input.txt"
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

2. Add the Manhattan distance between all the galaxies.

Seems simple enough.
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

(defconstant +space+ #\.)
(defconstant +galaxy+ #\#)

(defun big-bang (universe)
  "given a list of lists of characters describing a universe with dots
for space and # for galaxies, return the expanded universe -
duplicating lines that consist only of space"
  (let ((expanded-universe '()))

    (dolist (row universe)
      (push row expanded-universe)      ; save row to new universe

      (when (not (member +galaxy+ row)) ; when there are no galaxies in row
        (push row expanded-universe)))  ; duplicate it

    (reverse expanded-universe)))       ; return expanded universe

(defun rotate-universe (universe)
  "turns a universe on its side so we can big-bang the columns"
  (let ((rotated-universe '())
        (new-row '()))

    (iter (for col below (length (first universe)))
      (iter (for row below (length universe))
        (push (nth col (nth row universe)) new-row))
      (push (reverse new-row) rotated-universe)
      (setf new-row '()))
    (reverse rotated-universe)))

(5a:test rotate-universe-test
  (let ((uni (mapcar #'(lambda (l) (coerce l 'list)) *test-data*)))
    (5a:is (equal (rotate-universe (rotate-universe uni)) uni))
    (5a:is (equal (rotate-universe (list '(1 2 3) '(4 5 6) '(7 8 9)))
                  (list '(1 4 7) '(2 5 8) '(3 6 9))))))

(defun expand-universe (universe)
  "given a list of lists of characters describing a universe expand it in
both dimensions"
  (rotate-universe                      ; restore original rotation
   (big-bang                            ; expand cols
    (rotate-universe                    ; rotate
     (big-bang universe)))))            ; expand rows

(defun parse-universe (los)
  "given a list of strings describing a universe expand it then turn it
into a 2D array"
  (let* ((uni (expand-universe (mapcar #'(lambda (l) (coerce l 'list)) los)))
         (width (length (first uni)))
         (height (length uni))
         (map (make-array (list height width))))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref map row col) (elt (nth row uni) col))))
    map))

(defun manhattan-distance (x y)
  "given two points on a grid expressed as (cons x y) return the
manhattan distance between them"
  (+ (abs (- (car x) (car y)))
     (abs (- (cdr x) (cdr y)))))

(defun collect-galaxies (map)
  "given a universe map as a 2D array, return a list of all galaxy
locations as (cons row col)"
  (let ((galaxies '()))
    (iter (for row below (array-dimension map 0))
      (iter (for col below (array-dimension map 1))
        (when (char= +galaxy+ (aref map row col))
          (push (cons row col) galaxies))))
    (reverse galaxies)))

(5a:test collect-galaxies-test
  (5a:is (equal (collect-galaxies (parse-universe *test-data*))
                (list (cons 0 4) (cons 1 9) (cons 2 0)
                      (cons 5 8) (cons 6 1) (cons 7 12)
                      (cons 10 9) (cons 11 0) (cons 11 5)))))

(defun add-distances (galaxies)
  "given a list of galaxy locations as (cons row col) return the sum of
all the distances between galaxies"
  (do ((sum 0)                          ; total distance
       (g galaxies (rest g)))           ; for each galaxy

      ((null g) sum)  ; all galaxies accounted for, return total

    ;; loop body - multiply each galaxy against the rest
    (incf sum
          (iter (for gp in (rest g))
            (summing (manhattan-distance (first g) gp))))))

(defun Day11-1 (los)
  "given a list of strings representing a universe, return the sum total
of the shortest distances between each galaxy"
  (add-distances (collect-galaxies (parse-universe los))))

(5a:test Day11-1-test
  (5a:is (= (Day11-1 *test-data*) 374)))

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

------------------------------------------------------------------------------|#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 11 Part 1 is ~a"
              (day11-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 11 Part 2 is ~a"
;;	      (day11-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 11 Part 1 is 9233514
;; Evaluation took:
;; 0.012 seconds of real time
;; 0.012254 seconds of total run time (0.012189 user, 0.000065 system)
;; 100.00% CPU
;; 1,799,696 bytes consed
