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

I wonder if my sequence solver from Day 9 can help? Better yet - is
there a polynomial that I can fit to the sequence of results? Turns
out the Lagrange Polynomial algo is exactly what I want.

---------------------------------------------------------------------------- |#

(5a:test day21-1-test
  (5a:is (= 16   (day21-1 *test-data* 6)))
  (5a:is (= 50   (day21-1 *test-data* 10)))
  (5a:is (= 1594 (day21-1 *test-data* 50)))
  (5a:is (= 6536 (day21-1 *test-data* 100))))
;; (5a:is (= 167004 (day21-1 *test-data* 500)))    ; takes too long
;; (5a:is (= 668697 (day21-1 *test-data* 1000)))   ;   "
;; (5a:is (= 16733044 (day21-1 *test-data* 5000))) :   "

;; let's generate some starting values to feed (and test) the Newton
(day21-1 (uiop:read-file-lines *data-file*) 65)               ; 3719
(day21-1 (uiop:read-file-lines *data-file*) (+ 65 131))       ; 33190
(day21-1 (uiop:read-file-lines *data-file*) (+ 65 (* 2 131))) ; 91987
(day21-1 (uiop:read-file-lines *data-file*) (+ 65 (* 3 131))) ; 180110
(day21-1 (uiop:read-file-lines *data-file*) (+ 65 (* 4 131))) ; 297559
(day21-1 (uiop:read-file-lines *data-file*) (+ 65 (* 5 131))) ; 444334

(defun next-in-sequence (history)
  "given a list of numbers return the next number in the sequence - using
a Newton polynomial - this is the code from Day 9. Requires an even
number of items in history"
  (labels
      ((build-sequence (loi)
         ;; given a list of integers return a list of the
         ;; differences between each number"
         (cond ((null (rest loi)) nil)
               (t (cons (- (second loi) (first loi))
                        (build-sequence (rest loi)))))))

    ;; loop updating seq and result each time
    (do* ((seq history (build-sequence seq))
          (sub-seqs (list seq) (push seq sub-seqs)))

         ((every #'zerop seq)  ; all zeros?
          ;; then return the sum of the last digits in all the
          ;; sequences
          (reduce #'+ (mapcar #'(lambda (l) (car (last l)))
                              sub-seqs))))))

(5a:test next-in-sequence-test
  (5a:is (= 10 (next-in-sequence '(2 4 6 8))))
  (5a:is (= 297559 (next-in-sequence '(3719 33190 91987 180110))))) ; works!

(defun generate-steps (los count)
  "given a square grid defined by LOS and the number of steps to
generate, return a list of step counts up to COUNT"
  (let* ((side (length los))
         (base (floor side 2)))

    (iter (for step from 0 below count)
      (collecting (+ base (* step side))))))

(5a:test generate-steps-test
  (5a:is (equal (generate-steps *test-data* 10)
                (list 5 16 27 38 49 60 71 82 93 104))))

;; Now the code to use Lagrange to generate a polynomial function that
;; will generate the next points in a series
;;
(defun lj (x j xs)
  (let ((num 1)
        (denom 1))
    (iter (for m from 1 to (length xs))
      (unless (= m j)
        (setf num (* num (- x (nth m xs))))
        (setf denom (* denom (- (nth j xs) (nth m xs)))))
      (finally (return (/ num denom))))))

(defun lagrange-poly (x xs ys)
  (let ((result 0))
    (iter (for j from 0 below (length ys))
      (setf result (+ result (* (nth j ys) (lj x j xs))))
      (finally (return result)))))


(5a:test lagrange-poly-test
  (let ((xs '(1 2 3 4))
        (ys '(1 4 9 16)))
    (lagrange-poly 2.5 xs ys)))

(5a:test day21-2-test
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
