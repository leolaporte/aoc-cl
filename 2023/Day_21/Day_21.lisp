;;;; Day21.lisp
;;;; 2023 AOC Day 21 solution
;;;; Leo Laporte
;;;; Started: 6 April 2024, Petaluma, CA

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day21
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)               ; misc utilities
   (:ax :alexandria)             ; ditto
   (:re :cl-ppcre)               ; regex
   (:tr :trivia)                 ; pattern matching
   (:tp :trivia.ppcre)           ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day21)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_21/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
                     --- Day 21: Step Counter ---
                           --- Part One ---


"he'd like to know which garden plots he can reach with exactly his
remaining 64 steps.

He gives you an up-to-date map (your puzzle input) of his starting
position (S), garden plots (.), and rocks (#).

Starting from the garden plot marked S on your map, how many garden
plots could the Elf reach in exactly 64 steps?"

LEO'S NOTES: Well this looks familiar. I'll use a sparsh hash
here to represent the map because only around half the points
matter. Looks like a depth first search or DFS will do the
trick. Apparently backtracking is allowed, so I won't have to track
visited posns.

After carefully studying the problem and considering why my DFS
doesn't work, I realized that we're only looking for the positions
that can be visited next after each move - not all the visited
positions. That makes it a bit simpler - and faster. The function
EXPAND-NEIGHBORS returns an ever lengthening list of positions that
can be visited after each move. After de-duplicating them return the
number of unique positions.

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("..........."
    ".....###.#."
    ".###.##..#."
    "..#.#...#.."
    "....#.#...."
    ".##..S####."
    ".##..#...#."
    ".......##.."
    ".##.#.####."
    ".##..##.##."
    "..........."))

(defun row (posn)
  "mnemonic"
  (car posn))

(defun col (posn)
  "mnemonic"
  (cdr posn))

(defun make-map-hash (los)
  "returns two values: a sparse hash of walkable positions as (cons row col) each with a value of nil representing unvisited and the location of the S position."
  (let* ((width (length (first los)))
         (height (length los))
         (map (make-hash-table :test 'equal :size (* width height)))
         (start (cons 0 0)))

    (iter (for row below height)
      (iter (for col below width)
        (let ((char-at-pos (elt (nth row los) col)))
          (cond ((char= char-at-pos #\.)   ; garden plot?
                 (setf (gethash (cons row col) map) 'NO)) ; not visited

                ((char= char-at-pos #\S)
                 (setf (gethash (cons row col) map) 'YES) ; visited
                 (setf start (cons row col)))))))         ; starting posn

    (setf (gethash 'DIMS map) (cons height width)) ; just for print-map

    (values map start)))

(defun print-map (map)
  "debugging tool - prints the current state of the map with . for
unvisited garden plots, # for rocks, and O for visited plots"
  (let ((height (car (gethash 'DIMS map)))
        (width (cdr (gethash 'DIMS map))))

    (iter (for row below height)
      (format t "~&")
      (iter (for col below width)
        (let ((value (gethash (cons row col) map)))
          (cond ((null value) (format t "#"))
                ((equal value 'NO) (format t "."))
                ((equal value 'YES) (format t "O"))
                (t (error "There's something weird going on. ~A" value))))))))

(defun add-posns (x y)
  "adds two positions together to give a third position: e.g. (add-posns (cons 5 5) (cons 0 -1)) returns (cons 5 4)"
  (cons (+ (row x) (row y))
        (+ (col x) (col y))))

(defun next-garden-plots (posn map)
  "returns surrounding positions UP DOWN LEFT and RIGHT that are on the
map and are garden plots (not rocks) - backtracking allowed"
  (let ((dirs (list (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1))))
    (iter (for d in dirs) ; UP DOWN LEFT RIGHT
      (let ((new-posn (add-posns d posn)))
        (multiple-value-bind (value exists) (gethash new-posn map)
          (declare (ignore value))
          (when exists ; on map
            (collect new-posn)))))))           ; add it to neighbors

(5a:test next-garden-plots-test
  (let ((map (make-map-hash *test-data*)))
    (5a:is (equal (next-garden-plots (cons 0 0) map)
                  (list (cons 1 0) (cons 0 1))))
    (5a:is (equal (next-garden-plots (cons 5 5) map)
                  (list (cons 4 5) (cons 5 4))))
    (5a:is (equal (next-garden-plots (cons 5 4) map)
                  (list (cons 6 4) (cons 5 3) (cons 5 5))))))

(defun expand-neighbors (neighbors map)
  "given a list of positions as NEIGHBORS, return a list of unique
positions on MAP that can be reached in one move"
  (iter (for n in neighbors)
    ;; (setf (gethash n map) 'YES) ; track visited but just for debugging
    (appending (next-garden-plots n map) into ns)
    (finally (return (remove-duplicates ns :test 'equalp)))))

(defun day21-1 (los steps)
  "given a map of garden plots described by a list of strings LOS, a
starting position described in LOS, and the number of steps from START
to take, return the number of garden plots that can be reached after
that number of steps in any direction - backtracking allowed"
  (multiple-value-bind (map start) (make-map-hash los)
    (let ((neighbors (next-garden-plots start map))) ; first step
      (iter (for s from 2 to steps)                 ; all the rest
        ;; (print-map map)
        (setf neighbors (expand-neighbors neighbors map))
        (finally (return (length neighbors)))))))

(5a:test day21-1-test
  (5a:is (= 16 (day21-1 *test-data* 6))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

"The actual number of steps he needs to get today is exactly 26501365."

Sunnuva b.

"He also points out that the garden plots and rocks are set up so that
the map repeats infinitely in every direction."

Sobbing quietly.

"Starting from the garden plot marked S on your infinite map, how many
garden plots could the Elf reach in exactly 26501365 steps?"

The large number of example solutions is clearly a hint.

---------------------------------------------------------------------------- |#

(defun day21-2 (los steps)
  )

(5a:test dat21-2-test
  (5a:is (= 16 (day21-2 los 6)))
  (5a:is (= 50 (day21-2 los 10)))
  (5a:is (= 1594 (day21-2 los 50)))
  (5a:is (= 6536 (day21-2 los 100)))
  (5a:is (= 167004 (day21-2 los 500)))
  (5a:is (= 668697 (day21-2 los 1000)))
  (5a:is (= 16733044 (day21-2 los 5000))))


;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 21 Part 1 is ~a"
              (day21-1 (uiop:read-file-lines *data-file*) 64)))


;; (time (format t "The answer to AOC 2023 Day 21 Part 2 is ~a"
;;	      (day21-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
