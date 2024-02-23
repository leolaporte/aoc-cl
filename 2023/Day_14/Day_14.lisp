;;;; Day14.lisp
;;;; 2023 AOC Day 14 solution
;;;; Leo Laporte
;;;; 22 February 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day14
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day14)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_14/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
                    --- Day 14: Parabolic Reflector Dish ---
                                --- Part One ---

"Tilt the platform so that the rounded rocks all roll north. Afterward,
what is the total load on the north support beams?

The amount of load caused by a single rounded rock (O) is equal to the
number of rows from the rock to the south edge of the platform,
including the row the rock is on. (Cube-shaped rocks (#) don't
contribute to load.)

Return the the sum of the load caused by all of the rounded
rocks."

LEO'S NOTES: Still no pathfinding? I was told this would be hard. It's
a lot like the dropping sand problem from last year.

Darren Oakey in our Club TWiT AoC Discord says he always uses a
hash-table for his 2D grids. Maybe today's the day to try it. He is a
50* coder after all. Nope. I need a 2D array because I have to
calculate the rock movement from row 1 to the bottom in order.  Oh
well, maybe another day, another grid.

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("O....#...."
    "O.OO#....#"
    ".....##..."
    "OO.#O....O"
    ".O.....O#."
    "O.#..O.#.#"
    "..O..#O..O"
    ".......O.."
    "#....###.."
    "#OO..#...."))

(defun parse-input (los)
  "given a list of strings representing rows in a grid, create a hash
table with (x y) as key and contents of the point as value"
  (let* ((width (length (first los)))
         (height (length los))
         (grid (make-array (list height width))))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref grid row col) (elt (nth row los) col))))

    grid))

(defun row (posn)
  "returns the row of a posn expressed as (cons row col)"
  (car posn))

(defun col (posn)
  "returns the col of posn expressed as (cons row col"
  (cdr posn))

(defun look-up (posn grid)
  "returns the value of the position directly above posn or #\X if the
posn is on row 0"
  (if (zerop (row posn))
      #\X
      (aref grid (1- (row posn)) (col posn))))

(5a:test look-up-test
  (let ((*test-grid* (parse-input *test-data*)))
    (5a:is (equal (look-up (cons 0 0) *test-grid*) #\X))
    (5a:is (equal (look-up (cons 1 0) *test-grid*) #\O))
    (5a:is (equal (look-up (cons 3 5) *test-grid*) #\#))
    (5a:is (equal (look-up (cons 5 9) *test-grid*) #\.))))

(defun roll-up (posn grid)
  "given a round rock at posn, move it up the grid until it encounters an
obstacle (O or #)"
  (iter (while (equal #\. (look-up posn grid)))
    (setf posn (cons (1- (row posn)) (col posn))))
  posn)

(5a:test roll-up-test
  (let ((*test-grid* (parse-input *test-data*)))
    (5a:is (equal (roll-up (cons 0 0) *test-grid*) (cons 0 0))) ; top row
    (5a:is (equal (roll-up (cons 1 0) *test-grid*) (cons 1 0))) ; can't move
    (5a:is (equal (roll-up (cons 3 0) *test-grid*) (cons 2 0)))
    (5a:is (equal (roll-up (cons 3 4) *test-grid*) (cons 2 4)))
    (5a:is (equal (roll-up (cons 9 2) *test-grid*) (cons 7 2)))))

(defun tilt-north (grid)
  "rolls all the rounded rocks toward the top of the grid until each
encounters an obstacle, returns the final grid"
  (iter (for row from 1 below (array-dimension grid 0))
    (iter (for col below (array-dimension grid 1))
      (when (char= (aref grid row col) #\O) ; it's a round rock

        (let* ((old-posn (cons row col))
               (new-posn (roll-up old-posn grid)))

          (when (not (equal old-posn new-posn))
            (setf (aref grid row col) #\.)
            (setf (aref grid (row new-posn) (col new-posn)) #\O))))))
  grid)

(defun Day14-1 (los)
  (let* ((grid (tilt-north (parse-input los)))
         (len (array-dimension grid 0)))

    (iter (for row below len)
      (summing
       (iter (for col below (array-dimension grid 1))
         (when (char= (aref grid row col) #\O)
           (sum (- len row))))))))

(5a:test Day14-1-test
  (5a:is (= (Day14-1 *test-data*) 136)))

#| ----------------------------------------------------------------------------
                                --- Part Two ---

"Each cycle tilts the platform four times so that the rounded rocks roll north,
then west, then south, then east. After each tilt, the rounded rocks
roll as far as they can before the platform tilts in the next
direction. After one cycle, the platform will have finished rolling
the rounded rocks in those four directions in that order.

Calculate the total load on the north support beams after
1,000,000,000 cycles."

LEO'S NOTES: The only thing concerning here is the number of
cycles. I'll want to make the tilting as efficient as possible. Maybe
Darren wasn't so far off. To speed this up I *will* use a hash
table. I'll just have to maintain a sorted vector of rows for the
reference. And while I'm about that, let's make it a sparse-array -
only tracking rocks. This is going to take some re-writing for the
parse and tilt routines.

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 14 Part 1 is ~a"
              (day14-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 14 Part 2 is ~a"
;;	      (day14-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
