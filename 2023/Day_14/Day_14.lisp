;;;; Day14.lisp
;;;; 2023 AOC Day 14 solution
;;;; Leo Laporte
;;;; 22-26 February 2024

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

LEO'S NOTES: Still no pathfinding?

Darren Oakey in our Club TWiT AoC Discord says he always uses a
hash-table for his 2D grids. Maybe today's the day to try it. He is a
50 star coder after all. Using the sparse hash recommended by
ChocolateMilkMinisip also in da Club, also a 50 star coder.

----------------------------------------------------------------------------|#

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

(defun make-sparse-hash (los)
  "returns a sparse hash of round and square rocks, keys: (cons x y)
values: O or #"
  (let* ((width (length (first los)))
         (height (length los))
         (grid (make-hash-table :test 'equal :size (* width height))))

    (iter (for row below height)
      (iter (for col below width)
        (let ((rock (elt (nth row los) col)))
          (when (or (char= rock #\O) (char= rock #\#))
            (setf (gethash (cons row col) grid) (elt (nth row los) col))))))

    grid))

(defun pht (hash)
  "little utility to print a hash"
  (iter (for (key value) in-hashtable hash)
    (format t "~%~A => ~A" key value)))

(defun sorted-keys (hash)
  "returns a list of hash keys sorted by row and col"
  (sort
   (sort
    (iter (for (key value) in-hashtable hash)
      (collect key))
    #'< :key #'cdr)
   #'< :key #'car))

(defun nchange-hash-key (old new hash)
  "changes a hash key from old to new - DESTRUCTIVE"
  (let ((value (gethash old hash)))
    (remhash old hash)
    (setf (gethash new hash) value)))

(defun nroll-north (grid)
  "roll all round stones as far as possible toward row 0 on the grid -
return the modified grid - DESTRUCTIVE"
  (let ((rocks (remove-if ; make a sorted list of movable rocks
                #'(lambda (key) (char= #\# (gethash key grid)))
                (sorted-keys grid))))

    (dolist (r rocks)
      (let ((row (car r))
            (col (cdr r))
            (value (gethash r grid)))

        ;; roll it north
        (iter (while
               (and (> row 0)     ; while we're still on the grid
                    (not (gethash (cons (1- row) col) grid)))) ; and empty
          (setf row (1- row)))    ; keep moving north

        ;; the rock has rolled as far north as it can so...
        (remhash r grid)                             ; delete old location
        (setf (gethash (cons row col) grid) value))) ; set new location

    grid))

(defun get-load (grid height)
  "given a hash of a rock field (grid) and the height of the grid
return the total load on the north bearing timber"
  (iter (for (key value) in-hashtable grid)
    (when (char= #\O value)             ; only count round rocks
      (summing (- height (car key)))))) ; load = height

(defun Day14-1 (los)
  (let ((height (length los))
        (grid (make-sparse-hash los)))
    (get-load (nroll-north grid) height)))

(5a:test Day14-1-test
  (5a:is (= (Day14-1 *test-data*) 136)))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

"Each cycle tilts the platform four times so that the rounded rocks
roll north, ; then west, then south, then east. After each tilt, the
rounded rocks roll as far as they can before the platform tilts in the
next direction. After one cycle, the platform will have finished
rolling the rounded rocks in those four directions in that order.

Calculate the total load on the north support beams after
1,000,000,000 cycles."

LEO'S NOTES: Wait. What? 1 billion cycles? There's no way to do 1
billion anything in under a second. There has to be shortcut. Maybe
there's some sort of cycle? Perhaps the load repeats after so many
turns?

To simplify the cycle, I'll rotate the grid 90 degrees right, then
roll north each time rather than write four different roll functions.

---------------------------------------------------------------------------- |#

;; After 1 cycle:
(defparameter *cycle1*
  '(".....#...."
    "....#...O#"
    "...OO##..."
    ".OO#......"
    ".....OOO#."
    ".O#...O#.#"
    "....O#...."
    "......OOOO"
    "#...O###.."
    "#..OO#...."))

;; After 2 cycles:
(defparameter *cycle2*
  '(".....#...."
    "....#...O#"
    ".....##..."
    "..O#......"
    ".....OOO#."
    ".O#...O#.#"
    "....O#...O"
    ".......OOO"
    "#..OO###.."
    "#.OOO#...O"))

;; After 3 cycles:
(defparameter *cycle3*
  '(".....#...."
    "....#...O#"
    ".....##..."
    "..O#......"
    ".....OOO#."
    ".O#...O#.#"
    "....O#...O"
    ".......OOO"
    "#...O###.O"
    "#.OOO#...O"))

;; it's easier to rotate the grid and roll the rocks than roll the
;; rocks in four different directions - and it gives the same result
(defun rotate-right (grid width)
  "creates a new sparse hash which is the original grid rotated 90
degrees to the right - requires square grid"
  (let ((new-grid (make-hash-table :test 'equal)))
    (iter (for (key value) in-hashtable grid)
      (setf (gethash
             (cons (cdr key) (abs (- (car key) (1- width))))
             new-grid)
            value)
      (finally (return new-grid)))))

;; a 360 degree rotation should get us back where we started
(5a:test rotate-right-test
  (let ((grid (make-sparse-hash *test-data*))
        (w (length (first *test-data*))))
    (5a:is (equalp (rotate-right
                    (rotate-right
                     (rotate-right
                      (rotate-right grid w) w) w) w)
                   (make-sparse-hash *test-data*)))))

(defun cycle (grid width &optional (step 1))
  "rolls a rock grid in a complete circle STEP times then returns the
resulting grid"
  (iter (repeat (* 4 step))
    (setf grid (rotate-right (nroll-north grid) width))
    (finally (return grid))))

;; this test just rolls the rocks N W S E then sees if it matches the
;; example grids provided by AoC
(5a:test cycle-test
  (let ((w (length (first *test-data*))))
    (5a:is (equalp (cycle (make-sparse-hash *test-data*) w 1)
                   (make-sparse-hash *cycle1*)))
    (5a:is (equalp (cycle (make-sparse-hash *test-data*) w 2)
                   (make-sparse-hash *cycle2*)))
    (5a:is (equalp (cycle (make-sparse-hash *test-data*) w 3)
                   (make-sparse-hash *cycle3*)))))

(defun find-repeat (los)
  (let ((grid (make-sparse-hash los))
        (width (length (first los)))
        (history (make-hash-table :test 'equal)))

    (iter (for cycles from 1)
      (cycle grid width)
      (let* ((rock-map
               ;; make a list of round rock locations
               (iter (for (key value) in-hashtable grid)
                 (when (char= value #\O)
                   (collect key))))
             (exists (gethash rock-map history)))

        (if exists
            (return (values exists cycles))
            (setf (gethash rock-map history) cycles))))))

(defun build-result-vector (los cycles)
  "given a rock field as a list of strings, create a vector of CYCLES length, containing the results of get-load after each cycle"
  (let ((grid (make-sparse-hash los))
        (width (length (first los)))
        (height (length los))
        (vec (make-array cycles :fill-pointer 0 :adjustable t)))

    (iter (for i to cycles)
      (setf grid (cycle grid width))
      (vector-push (get-load grid height) vec)
      (finally (return vec)))))

(defun find-longest-repeat-block (cycles)
  "given a vector of results, find the (non-greedy) longest series of repeating
numbers, returns two values: the index in which the repeating series
begins and its length"

  (do ((len (length cycles))
       (index 0)          ; current position in vector
       (window-size 1)    ; current size of pattern

       (pattern-start 0)  ; longest pattern start
       (pattern-len 1))   ; longest pattern len

      ;; we've checked entire vector - return longest block
      ((>= (+ index window-size) len) (values pattern-start pattern-len))

    ;; repeatedly search vector
    (let* ((start index)                   ; current source starting point
           (end (+ index window-size))     ; current source end point
           (seq (subseq cycles start end)) ; location of source in vector
           (found (search seq cycles :start2 (1+ end)))) ; search for repeat

      (cond ((equal found (1+ end))              ; repeats immediately
             (when (> window-size pattern-len)   ; it's a longer pattern
               (setf pattern-start start)        ; so save it
               (setf pattern-len (1+ window-size)))

             ;; initially I tried expanding the window size here but
             ;; it ended up making the search greedy - which I don't
             ;; really need - so I'll just restart the search one past
             ;; the pattern
             (incf index))                   ; can we find a bigger one?

            ((not (null found))   ; found something but it's down the vector
             (incf window-size))  ; so try a bigger window

            (t                         ; no match
             (incf index)              ; move to next position in vec
             (setf window-size 1)))))) ; and start with small window

(defun Day14-2 (los tests iterations)
  "given a rock field, calculate the final load after rolling the
 rocks in each cardinal direction ITERATIONS times"
  (let ((results (build-result-vector los tests)))

    (multiple-value-bind (repeat-start repeat-size)
        (find-longest-repeat-block results)

      ;; index into results to find answer
      (aref results
            (1- (+ repeat-start                     ; skip to the block repeat
                   (mod (- iterations repeat-start) ; add the number of leftovers
                        repeat-size)))))))          ; subtract 1 because zero based

(5a:test Day14-2-test
  (5a:is (= (Day14-2 *test-data* 50 1000000000) 64)))

;; now solve the puzzle!

(time (format t "The answer to AOC 2023 Day 14 Part 1 is ~a"
              (day14-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 14 Part 2 is ~a"
              (day14-2 (uiop:read-file-lines *data-file*) 300 1000000000)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 14 Part 1 is 112048
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.002280 seconds of total run time (0.002207 user, 0.000073 system)
;; 100.00% CPU
;; 646,320 bytes consed

;; The answer to AOC 2023 Day 14 Part 2 is 105606
;; Evaluation took:
;; 1.978 seconds of real time
;; 1.976981 seconds of total run time (1.966152 user, 0.010829 system)
;; [ Real times consist of 0.032 seconds GC time, and 1.946 seconds non-GC time. ]
;; [ Run times consist of 0.032 seconds GC time, and 1.945 seconds non-GC time. ]
;; 99.95% CPU
;; 1,040,204,176 bytes consed
