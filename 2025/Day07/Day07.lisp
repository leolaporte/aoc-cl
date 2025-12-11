;;;; Day07.lisp
;;;; 2025 AOC Day 7 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 08 Dec 2025 at 15:42
;;;; Finished: Thu Dec 11 14:53:16 2025

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day07
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day07)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day07/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;                        --- Day 7: Laboratories ---
;;                              --- Part One ---
;;
;; LEO'S NOTES: Funny, Eric even gives us links to the two similar problems from
;; 2024-6 and 2018-8. I solved the former but my answer to part 2 took 198
;; seconds. Let's see if I can do better this time. Looks like a sparse hash for
;; the parsing.
;;
;; Start at S, travel down until ^ at which point split to spaces left and right
;; and continue down. How many times will the beam be split?
;;
;; I can't just exponentiate because the splitters are irregularly
;; distributed. Looks like the best way to do this is to create a list of active
;; beams and add to them every split. Then count the splits at the end.
;;
;; OK that worked fine, but now that I see Part 2 I realize I can't brute force
;; the path count. The process I used in part 1 would never finish for part
;; two. But here's the good news, thanks to a hint on Reddit I realized that
;; this is a perfect example of a Pascal's Triangle. Which means I can write
;; some code that works for both parts in a single pass. (I don't think I've
;; ever experienced this in an AOC problem. It's going to take a full rewrite of
;; my part 1 but it's worth it for the elegance of the solution.)
;;
;; Step one is to re-write the PARSE-INPUT into MAKE-GRID which makes
;; a 2D array instead of a sparse hash because I need to track all the
;; possible paths through the grid.

;; Then, working a line at a time using MOVE-DOWN-ONE, I'll pass through the
;; array doing two things, filling in the Pascal's triangle to count the paths
;; for part 2. Part one wants the number of splits which I can do by changing
;; touched splitters from #\^ to #\X, then counting the Xs. The whole thing can
;; be done in a single pass through the array! Nice! And blindingly fast, to
;; boot!
;;
;; ----------------------------------------------------------------------------

(defparameter *example* (list ".......S......."
                              "..............."
                              ".......^......."
                              "..............."
                              "......^.^......"
                              "..............."
                              ".....^.^.^....."
                              "..............."
                              "....^.^...^...."
                              "..............."
                              "...^.^...^.^..."
                              "..............."
                              "..^...^.....^.."
                              "..............."
                              ".^.^.^.^.^...^."
                              "..............."))

(sr:-> make-grid (list) array)
(defun make-grid (input)
  "given a list of strings representing a map of splitters, #\^, with a starting
point marked with #\S, and empty points marked with #\., return a 2D
array of those characters with the #\S replaced with 1 - the number of
active paths at the beginning of the run"
  (let* ((height (length input))
         (width (length (first input)))
         (grid (make-array (list height width))))

    (iter (for row below height)       ; index into list of strings
      (iter (for col below width)      ; index into each string
        (let ((ch (char (nth row input) col)))
          (if (char= ch #\S)
              (setf (aref grid row col) 1)  ; replase S with 1 - first beam!
              ;; else
              (setf (aref grid row col) ch)))))

    grid))                             ; return the array

(format t "~%Example grid: ~a~%" (make-grid *example*))

(sr:-> in-grid? (cons array) boolean)
(defun in-grid? (beam grid)
  "returns true if beam is on grid - I don't need this in the *example* but who
knows what evil lurks in the real data"
  (let ((height (array-dimension grid 0))
        (width (array-dimension grid 1)))

    (and (< -1 (car beam) height)       ; y is inside height
         (< -1 (cdr beam) width))))     ; x is inside width

(5a:test in-grid?-test
  (let ((grid (make-grid *example*)))
    (5a:is-true (in-grid? (cons 0 7) grid))
    (5a:is-true (in-grid? (cons 0 14) grid))
    (5a:is-true (in-grid? (cons 15 7) grid))
    (5a:is-false (in-grid? (cons 16 7) grid))
    (5a:is-false (in-grid? (cons 0 15) grid))
    (5a:is-false (in-grid? (cons -1 7) grid))))

;; Now the base function MOVE-DOWN-ONE. Given a row on the grid it checks for
;; any numbers in the row (there will always be at least one). For every number
;; on the row, N, check the same column on the row below. If it's a #\. just put
;; N in that positon and continue. If it's a number add N to it and put the
;; result in the column below. If it's a #\^ replace it with an #\X and add N to
;; the columns to the left and right of #\^ - or if those columns contain a
;; #\. put N in those positions. Once the entire row has been processed, return
;; the modified array.
(sr:-> move-down-one (number array) (array))
(defun move-down-one (row grid)
  "given a starting row and a grid map, return the grid map amended with the
results of moving from the starting row to the next row on the map"
  (iter (for col below (array-dimension grid 1))
    (let ((curr (aref grid row col)))   ; current pos
      ;; only do something when we're on a beam
      (when (numberp curr)                    ; current beam count
        (let ((nxt (aref grid (1+ row) col))) ; what's immediately below it?
          (cond
            ;; meeting another beam? add current beam count, curr, to nxt
            ((numberp nxt)
             (setf (aref grid (1+ row) col) (+ nxt curr)))

            ;; emcurry space - replace it with curr
            ((char= nxt #\.)
             (setf (aref grid (1+ row) col) curr))

            ;; it's a splitter
            ((char= nxt #\^)
             (setf (aref grid (1+ row) col) #\X) ; mark it as visited then...

             ;; make sure left and right are on grid before trying to setf them
             (when (in-grid? (cons (1+ row) (1- col)) grid) ; left in grid?
               (if (numberp (aref grid (1+ row) (1- col)))  ; if it's a num..
                   (setf (aref grid (1+ row) (1- col))      ; sum the two nums
                         (+ curr (aref grid (1+ row) (1- col))))
                   (setf (aref grid (1+ row) (1- col)) curr))) ; else replace

             (when (in-grid? (cons (1+ row) (1+ col)) grid) ; right in grid?
               (if (numberp (aref grid (1+ row) (1+ col)))
                   (setf (aref grid (1+ row) (1+ col))
                         (+ curr (aref grid (1+ row) (1+ col))))
                   (setf (aref grid (1+ row) (1+ col)) curr))))

            (t (error "Unexpected grid item in MOVE-DOWN-ONE!")))))))

  grid)                                 ; return modified grid

(sr:-> day07 (list) (values number number))
(defun day07 (input)
  "move down the grid until there are no more moves left - return the number of
splitters touched, and the total number of possible tachyon paths through the
grid"
  (let ((grid (make-grid input))
        (splitters 0)
        (paths 0))

    ;; work the grid for paths and splitter counts
    (iter (for line below (1- (array-dimension grid 0))) ; all but last line
      (setf grid (move-down-one line grid)))

    ;; now count touched splitters (Xs)
    (setf splitters
          (iter (for row below (array-dimension grid 0))
            (summing
              (iter (for col below (array-dimension grid 1))
                (counting (equal #\X (aref grid row col)))))))

    (setf paths
          (let ((last-row (1- (array-dimension grid 0))))
            (iter (for col below (array-dimension grid 1))
              (when (numberp (aref grid last-row col))
                (summing (aref grid last-row col))))))

    (values splitters paths)))

(5a:test day07-test
  (multiple-value-bind  (splitters paths) (day07 *example*)
    (5a:is (= 21 splitters))
    (5a:is (= 40 paths))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle! Both parts in one pass.

(time
 (multiple-value-bind (splitters paths) (day07 (uiop:read-file-lines *data-file*))
   (format t "~&The answer to AOC 2025 Day 7 Part 1 is ~a~%" splitters)
   (format t "~&The answer to AOC 2025 Day 7 Part 2 is ~a~%" paths)))

;; ----------------------------------------------------------------------------
;; Timing with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 7 Part 1 is 1516
;; The answer to AOC 2025 Day 7 Part 2 is 1393669447690
;; Evaluation took:
;; 0.003 seconds of real time
;; 0.003148 seconds of total run time (0.003030 user, 0.000118 system)
;; 100.00% CPU
;; 314,880 bytes consed
