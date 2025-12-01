;;;; Day21.lisp
;;;; 2023 AOC Day 21 solution
;;;; Leo Laporte
;;;; Started: 6 April 2024, Petaluma, CA
;;;; Finished: 27 April 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day21
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)                      ; misc utilities
   (:ax :alexandria)                    ; ditto
   (:re :cl-ppcre)                      ; regex
   (:tr :trivia)                        ; pattern matching
   (:tp :trivia.ppcre)                  ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day21) ; synchronize package and dir with C-c ~

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "input.txt"
  "Downloaded from the AoC problem set")

(defparameter *data-file2* "input2.txt"
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

(defun add-posns (x y)
  "adds two positions together to give a third position:
e.g. (add-posns (cons 5 5) (cons 0 -1)) returns (cons 5 4)"
  (cons (+ (row x) (row y))
        (+ (col x) (col y))))

(defun make-map-hash (los)
  "returns two values: a sparse hash of walkable positions as (cons row
col) each with a value of 'NO representing unvisited, and the location
of the S position."
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

(defun garden-plot? (posn map)
  "a special version of (gethash posn map) which always refers back to
the original base map regardless of where the position is. In effect,
this creates an infinite map in all directions with start at the
center.  Returns false if position is a rock, returns the VALUE if
it's a garden-plot"
  (let ((height (car (gethash 'DIMS map)))
        (width (cdr (gethash 'DIMS map))))
    (gethash (cons (mod (row posn) height) (mod (col posn) width)) map)))

(defun next-garden-plots (posn map)
  "returns surrounding positions UP DOWN LEFT and RIGHT that are garden
plots (not rocks) - backtracking allowed!"
  (let ((dirs (list (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1))))
    (iter (for d in dirs) ; UP DOWN LEFT RIGHT
      (let ((new-posn (add-posns d posn)))
        (when (garden-plot? new-posn map)  ; it's a garden plot
          (collect new-posn))))))          ; add it to neighbors

(5a:test next-garden-plots-test
  (let ((map (make-map-hash *test-data*)))
    (5a:is (equal (next-garden-plots (cons 0 0) map) ; goes off map
                  (list (cons -1 0) (cons 1 0)
                        (cons 0 -1) (cons 0 1))))
    (5a:is (equal (next-garden-plots (cons 5 5) map)
                  (list (cons 4 5) (cons 5 4))))
    (5a:is (equal (next-garden-plots (cons 16 16) map)
                  (list (cons 15 16) (cons  16 15))))
    (5a:is (equal (next-garden-plots (cons 5 4) map)
                  (list (cons 6 4) (cons 5 3) (cons 5 5))))
    (5a:is (equal (next-garden-plots (cons 16 15) map)
                  (list (cons 17 15) (cons 16 14)
                        (cons 16 16))))
    (5a:is (equal (next-garden-plots (cons 11 11) map) ; goes off map
                  (list (cons 10 11) (cons 12 11)
                        (cons 11 10) (cons 11 12))))))

(defun expand-neighbors (neighbors map)
  "given a list of positions as NEIGHBORS, return a list of unique
positions on MAP that can be reached on the next move"
  (iter (for n in neighbors)
    (appending (next-garden-plots n map) into ns)
    (finally (return (remove-duplicates ns :test 'equalp)))))

(defun day21-1 (los steps)
  "given a map of garden plots described by a list of strings LOS, a
starting position described in LOS, and the number of steps from START
to take, return the number of garden plots that can be reached after
that number of steps in any direction - backtracking allowed"
  (multiple-value-bind (map start) (make-map-hash los)
    (let ((neighbors (next-garden-plots start map))) ; first step
      (iter (for s from 2 to steps)                  ; all the rest
        (setf neighbors (expand-neighbors neighbors map))
        (finally (return (length neighbors)))))))

(5a:test day21-1-test
  (5a:is (= 16 (day21-1 *test-data* 6))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

"The actual number of steps he needs to get today is exactly 26501365."

Sunnuva beyotch.

"He also points out that the garden plots and rocks are set up so that
the map repeats infinitely in every direction."

Sobbing quietly.

"Starting from the garden plot marked S on your infinite map, how many
garden plots could the Elf reach in exactly 26501365 steps?"

The large number of example solutions is clearly a hint. There must be
a pattern of some kind. First step is to rewrite neighbors from part 1
to reflect in infinite grid... That wasn't too bad, and it didn't break
part 1.

So, can I generate a polynomial that I can fit to the sequence of
results?  I'm going to assume there is (otherwise this problem is
intractable in a reasonable time - sound familiar?).

(study study study and watch
https://www.youtube.com/watch?v=4AuV93LOPcE
also this python notebook explanation is VERY helpful:
https://github.com/derailed-dash/Advent-of-Code/blob/master/src/AoC_2023/Dazbo's_Advent_of_Code_2023.ipynb)

---------------------------------------------------------------------------- |#

;; By examining the geometry of the provided input data it turns out
;; because S is smack dab in the center of the tile it takes (floor
;; width 2) steps to reach the edge, call it steps-to-edge. To get to
;; the next edge we add the width of the square to that number
;; (because each tile is the same), to get to the edge after that add
;; (* width 2) and so on. It also turns out - entirely by chance I'm
;; sure - that this progression can be solved by a polynomial for any
;; number of edges.

;; Also in an amazing coincidence, the provided number of steps is on
;; an edge! and which edge? 2023 00. Wow!

;; Use part 1 to provide the number of reachable plots for each of the
;; first three edges - all we need for the polynomial is three points
;; this is the slowest part of the solution.
(defun reachable-plots (los count)
  "given a map of garden plots that can repeat infinitely in every
direction, return a list of the number of reachable plots for COUNT
tiles in the infinite set"
  (let* ((width (length los))             ; size of base tile
         (steps-to-edge (floor width 2))) ; starting at center
    (iter (for i from 0 below count)      ; for COUNT tiles starting from S
      (collect (day21-1 los (+ steps-to-edge (* width i)))))))

(defun solve-quadratic (los steps)
  "derive and solve the quadratic equation defined by the given plot map"
  (let* ((rps (reachable-plots los 3))  ; results for plots 0 1 2
         (width (length los))           ; grid width

         ;; decompose the quadratic ax^2 + bx + c
         (c (first rps))
         (b (floor (- (* 4 (second rps))
                      (* 3 c)
                      (third rps))
                   2))
         (a (- (second rps) c b))

         ;; how many tiles to go?
         (tiles (floor (- steps (floor width 2)) width)))

    (+ (* a (expt tiles 2))
       (* b tiles)
       c)))

(defun day21-2 (los steps)
  "given a map of garden plots and the number of steps to take, return
 the reachable garden plots at the end of the steps"
  (solve-quadratic los steps))

(5a:test day21-2-test
  (5a:is (= 639051580070841 (day21-2 (uiop:read-file-lines *data-file2*) 26501365))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 21 Part 1 is ~a"
              (day21-1 (uiop:read-file-lines *data-file*) 64)))

(time (format t "The answer to AOC 2023 Day 21 Part 2 is ~a"
	      (day21-2 (uiop:read-file-lines *data-file*) 26501365)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 21 Part 1 is 3574
;; Evaluation took:
;; 0.042 seconds of real time
;; 0.042829 seconds of total run time (0.041792 user, 0.001037 system)
;; [ Real times consist of 0.002 seconds GC time, and 0.040 seconds non-GC time. ]
;; [ Run times consist of 0.002 seconds GC time, and 0.041 seconds non-GC time. ]
;; 102.38% CPU
;; 39,276,144 bytes consed

;; The answer to AOC 2023 Day 21 Part 2 is 600090522932119
;; Evaluation took:
;; 7.673 seconds of real time
;; 7.681426 seconds of total run time (7.502104 user, 0.179322 system)
;; [ Real times consist of 0.829 seconds GC time, and 6.844 seconds non-GC time. ]
;; [ Run times consist of 0.828 seconds GC time, and 6.854 seconds non-GC time. ]
;; 100.10% CPU
;; 6,376,053,776 bytes consed
