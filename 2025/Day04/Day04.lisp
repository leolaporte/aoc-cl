;;;; Day04.lisp
;;;; 2025 AOC Day 4 solution
;;;; Common Lisp solutions by Leo Laporte
;;;; Started: 03 Dec 2025 at 22:30
;;;; Finished: 04 Dec 2025 at 01:13

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day04
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day04)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day04/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;                     --- Day 4: Printing Department ---
;;                              --- Part One ---
;;
;; LEO'S NOTES: A roll of paper can be accessed only if there are fewer than
;; four rolls in the eight adjacent positions. How many rolls on the map can be
;; accessed?
;;
;; The grid can be represented in many ways, a 2D array, 1D vector,
;; hash-table. Mostly this first part isn't very demanding, but I'll opt for the
;; two dimensional array of the map called GRID. I'll make a predicate called
;; ACCESSIBLE? that given a position on the grid, returns true if there are
;; fewer than four rolls of paper adjacent. We've been here before many times,
;; it's pretty simple.
;;
;; ----------------------------------------------------------------------------

(defparameter *example* (list "..@@.@@@@."
                              "@@@.@.@.@@"
                              "@@@@@.@.@@"
                              "@.@@@@..@."
                              "@@.@@@@.@@"
                              ".@@@@@@@.@"
                              ".@.@.@.@@@"
                              "@.@@@.@@@@"
                              ".@@@@@@@@."
                              "@.@.@@@.@."))

(defparameter *adjacencies*
  (list '(-1 0) '(-1 -1) '(0 -1) '(1 -1) '(1 0) '(1 1) '(0 1) '(-1 1))
  "the eight directions starting with North and moving clockwise")

(sr:-> parse-input (list) array)
(defun parse-input (input)
  "given a list of strings return a 2D array of chars as they appear in the
list - array indices are row/col"
  (let* ((rows (length input))
         (columns (length (first input)))
         (grid (make-array (list rows columns)
                           :initial-element #\.
                           :element-type 'standard-char)))

    (iter (for y below rows)
      (iter (for x below columns)
        (when (char= #\@ (char (nth y input) x))
          (setf (aref grid y x) #\@))))
    grid))

(sr:-> accessible? (cons array) boolean)
(defun accessible? (posn grid)
  "given a position POSN expressed as (CONS Y X) and a 2D array called
GRID, return true if there are fewer than four occupied positions in the
eight directions."
  (iter (for loc in *adjacencies*)
    (let ((y (+ (car posn) (first loc)))
          (x (+ (cdr posn) (second loc))))

      (when                             ; off the grid
          (or (not (< -1 y (array-dimension grid 0)))
              (not  (< -1 x (array-dimension grid 1))))
        (next-iteration))               ; so skip this direction

      ;; keep track of adjacent paper rolls
      (counting (char= #\@ (aref grid y x)) into trues)

      (when (>= trues 4)                ; found four
        (return nil))                   ; so fail

      (finally (return t)))))

(5a:test accessible?-test
  (let ((grid (parse-input *example*)))
    (5a:is-true (accessible? (cons 0 2) grid))
    (5a:is-true (accessible? (cons 9 8) grid))
    (5a:is-false (accessible? (cons 1 1) grid))
    (5a:is-false (accessible? (cons 1 4) grid))))

(sr:-> day04-1 (list) number)
(defun day04-1 (input)
  "counts the number of paper rolls on a grid that have fewer than four adjacent rolls"
  (let ((grid (parse-input input)))
    (iter (for y below (array-dimension grid 0))
      (summing
        (iter (for x below (array-dimension grid 1))
          (when (char= #\@ (aref grid y x))
            (count (accessible? (cons y x) grid))))))))

(5a:test day04-1-test
  (5a:is (= 13 (day04-1 *example*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Two --
;;
;; LEO'S NOTES: Now we iterate removing rolls until no more can be removed and
;; return the number of rolls removed.
;;
;; ----------------------------------------------------------------------------

(sr:-> collect-accessibles (array) list)
(defun collect-accessibles (grid)
  "make a list of all the paper roll locations that can be removed"
  (iter (for y below (array-dimension grid 0))
    (appending
     (iter (for x below (array-dimension grid 1))
       (when (char= #\@ (aref grid y x))
         (when (accessible? (cons y x) grid)
           (collecting (cons y x))))))))

(sr:-> day04-2 (list) number)
(defun day04-2 (input)
  "remove paper rolls until no more are accessible then return the number
of rolls removed"
  (do* ((grid (parse-input input))
        (accessibles (collect-accessibles grid) (collect-accessibles grid))
        (removed (length accessibles) (+ removed (length accessibles))))

       ((null accessibles) removed) ; no more accessible rolls, return total

    (iter (for loc in accessibles) ; remove the rolls and repeat
      (setf (aref grid (car loc) (cdr loc)) #\.))))

(5a:test day04-2-test
  (5a:is (= 43 (day04-2 *example*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to AOC 2025 Day 4 Part 1 is ~a"
              (day04-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2025 Day 4 Part 2 is ~a"
              (day04-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 4 Part 1 is 1508
;; Evaluation took:
;; 0.005 seconds of real time
;; 0.005414 seconds of total run time (0.004867 user, 0.000547 system)
;; [ Real times consist of 0.001 seconds GC time, and 0.004 seconds non-GC time. ]
;; [ Run times consist of 0.001 seconds GC time, and 0.005 seconds non-GC time. ]
;; 100.00% CPU
;; 326,640 bytes consed

;; The answer to AOC 2025 Day 4 Part 2 is 8538
;; Evaluation took:
;; 0.037 seconds of real time
;; 0.037208 seconds of total run time (0.037175 user, 0.000033 system)
;; 100.00% CPU
;; 4,389,408 bytes consed
