;;;; Day03.lisp
;;;; 2023 AOC Day 03 solution
;;;; Leo Laporte
;;;; 9 December 2023

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day03
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)  ; for regular expressions
   (:5a :fiveam)))  ; for inline tests

(in-package :day03)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_03/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 3: Gear Ratios ---
--- Part One ---

"The engineer explains that an engine part seems to be missing from the engine,
but nobody can figure out which one. If you can add up all the part numbers in
the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of
the engine. There are lots of numbers and symbols you don't really understand,
but apparently any number adjacent to a symbol, even diagonally, is a "part
number" and should be included in your sum. (Periods (.) do not count as a
symbol.)

What is the sum of all of the part numbers in the engine schematic?"

NOTES:

It's the usual AoC two step here.

1. Go through each line recording the location of all the symbols as a
point on the grid, resulting in a set of points containing symbols.

2. Collect the part numbers, checking the part's adjacent points as I
go. The only tricky part is that we don't know the length of the part
numbers in advance: the number of adjacent points will vary. So,
generate the set of adjacent points for any given part number. If that
set intersects with the symbol set add the part number to the sum.
-----------------------------------------------------------------------------
|#

(defparameter *test-data*
  ;; 0123456789
  '("467..114.." ; 0
    "...*......" ; 1
    "..35..633." ; 2
    "......#..." ; 3
    "617*......" ; 4
    ".....+.58." ; 5
    "..592....." ; 6
    "......755." ; 7
    "...$.*...." ; 8
    ".664.598..")) ; 9

(defun map-grid-symbols (list-of-lines)
  "given a list of lines defining a grid, return a list of points that hold
symbols. A point is represented as (x . y) - symbols are anything not a . or
digit."
  ;; loop through each line
  (do* ((y 0 (1+ y))                          ;; current line as y coordinate
        (list-of-symbol-posn '())             ;; building this as we go
        (line list-of-lines (rest line)))     ;; go through each line

       ((null line) list-of-symbol-posn)      ;; all done, return the list

    ;; loop body - go through each char in line
    (let ((chars (coerce (first line) 'list)))    ;; convert line into list of chars
      (dotimes (x (length chars))                 ;; for each char's x posn
        (let ((c (nth x chars)))                  ;; get the char
          (when (and (not (char= c #\.))          ;; neither a period
                     (not (digit-char-p c)))      ;; or a digit
            (push (cons x y) list-of-symbol-posn))))))) ; save the point

(5a:test map-grid-symbols-test
  (5a:is (equal (map-grid-symbols *test-data*)
                '((5 . 8) (3 . 8) (5 . 5) (3 . 4) (6 . 3) (3 . 1)))))

(defun adjacent-points (pt)
  "given a point returns a list of adjacent points. doesn't test for
points off the grid"
  (let ((x (car pt))
        (y (cdr pt)))
    (list (cons (1- x) y)
          (cons (1- x) (1- y))
          (cons x (1- y))
          (cons (1+ x) (1- y))
          (cons (1+ x) y)
          (cons (1+ x) (1+ y))
          (cons x (1+ y))
          (cons (1- x) (1+ y)))))

(5a:test adjacent-points-test
  (5a:is (equal (adjacent-points (cons 5 5))
                '((4 . 5) (4 . 4) (5 . 4) (6 . 4)
                  (6 . 5) (6 . 6) (5 . 6) (4 . 6)))))

(defun all-adjacent-points (start end row)
  "returns the set of all points adjacent to a sequence of points on a
row with the x coordinate ranging from start to end-1 - doesn't check
for points off grid"
  (remove-duplicates
   (loop for x from start below end
         append (adjacent-points (cons x row)))
   :test 'equal))

(5a:test all-adjacent-points-test
  (5a:is (equal (intersection (all-adjacent-points 0 3 0)
                              (map-grid-symbols *test-data*) :test 'equal)
                '((3 . 1))))
  (5a:is (equal (intersection (all-adjacent-points 5 8 0)
                              (map-grid-symbols *test-data*) :test 'equal)
                nil))
  (5a:is (equal (intersection (all-adjacent-points 2 4 2)
                              (map-grid-symbols *test-data*) :test 'equal)
                '((3 . 1))))
  (5a:is (equal (intersection (all-adjacent-points 0 3 4)
                              (map-grid-symbols *test-data*) :test 'equal)
                '((3 . 4))))
  (5a:is (equal (intersection (all-adjacent-points 7 9 5)
                              (map-grid-symbols *test-data*) :test 'equal)
                nil))
  (5a:is (equal (intersection (all-adjacent-points 5 8 9)
                              (map-grid-symbols *test-data*) :test 'equal)
                '((5 . 8)))))

(defparameter digits (re:create-scanner "\\d+")
  "I use this a lot in the next function - so create a pre-scanned regex for
 efficiency")

(defun Day03-1 (list-of-lines)
  "given a list of lines returns the sum of the numbers that touch a symbol"
  (do* ((symbol-pts (map-grid-symbols list-of-lines))  ; set of symbol points
        (y 0 (1+ y))                          ; line number - aka y coord
        (lines list-of-lines (rest lines))    ; step through lines

        ;; list the start and end+1 x-coords of each part on this line
        (part-posns (re:all-matches digits (first lines))
                    (re:all-matches digits (first lines)))

        ;; list the part number of each part as a string
        (part-nums (re:all-matches-as-strings digits (first lines))
                   (re:all-matches-as-strings digits (first lines)))

        (total 0)) ; the sum total of all the proper parts (our solution)

       ((null lines) total)  ; all lines done - return solution

    ;; loop body - find the symbol-adjacent part numbers on current line
    (unless (null part-nums) ; don't bother unless there's parts on this line
      (do* ((posn part-posns (cdr (cdr posn))) ; go 2 by 2 through x coords
            (start (first posn) (first posn))  ; first x on the line
            (end (second posn) (second posn))  ; last+1 x on the line
            (part part-nums (rest part)))      ; the remaining part numbers

           ((null part)) ; no more parts to check on this line

        ;; check for symbol adjacency
        (when ;; the current part touches a symbol
            (intersection (all-adjacent-points start end y)
                          symbol-pts :test 'equal)
          ;; so add it to the total
          (incf total (parse-integer (first part))))))))

(5a:test Day03-1-test
  (5a:is (equal (Day03-1 *test-data*) 4361)))

#| -----------------------------------------------------------------------------
--- Part Two ---

A gear is any * symbol that is adjacent to exactly two part numbers. Its
gear ratio is the result of multiplying those two numbers together.

This time, you need to find the gear ratio of every gear and add them all up so
that the engineer can figure out which gear needs to be replaced.

What is the sum of all of the gear ratios in your engine schematic?

NOTE: Well it looks like the strategy I chose for part one isn't a great match
for part two.

First I'll make a new set of points - all the gears on the map.

------------------------------------------------------------------------------|#

(defparameter *pt2-test*
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))

(defun map-gear-symbols (list-of-lines)
  "given a list of lines defining a grid, return a list of points that hold
a gear '*'. A point is represented as (x . y)"
  ;; loop through each line
  (do* ((y 0 (1+ y))                          ;; current line as y coordinate
        (list-of-symbol-posn '())             ;; building this as we go
        (line list-of-lines (rest line)))     ;; go through each line

       ((null line) list-of-symbol-posn)      ;; all done, return the list

    ;; loop body - go through each char in line
    (let ((chars (coerce (first line) 'list)))    ;; convert line into list of chars
      (dotimes (x (length chars))                 ;; for each char's x posn
        (let ((c (nth x chars)))                  ;; get the char
          (when (char= c #\*)                     ;; it's a gear
            (push (cons x y) list-of-symbol-posn))))))) ; save the point

(5a:test map-gear-symbols-test
  (5a:is (equal (map-gear-symbols *test-data*)
                '((5 . 8) (3 . 4) (3 . 1)))))


(5a:test Day03-1-test
  (5a:is (equal (Day03-2 *pt2-test*) 467835)))


;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 03 Part 1 is ~a"
              (day03-1 (uiop:read-file-lines *data-file*))))


;; (time (format t "The answer to AOC 2023 Day 03 Part 2 is ~a"
;;	      (day03-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------
