;;;; Day18.lisp
;;;; 2023 AOC Day 18 solution
;;;; Leo Laporte
;;;; 16 March 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day18
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day18)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info

(defparameter *data-file* "~/cl/AOC/2023/Day_18/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 18: Lavaduct Lagoon ---
--- Part One ---

LEO'S NOTES: I think I can use my algorithm from Day 10 to calculate
the inner area. Or a flood fill. But I'm worried about those
unused "hex color codes." Eric has a tendency to go
exponential. Fortunately, math to the rescue. Turns out I can use the
Shoelace algorithm to calculate the area. In this case the area only
includes the inside half of each perimeter point so I have to add in
the other half (and one more for the 1/4 in each corner).

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("R 6 (#70c710)"
    "D 5 (#0dc571)"
    "L 2 (#5713f0)"
    "D 2 (#d2c081)"
    "R 2 (#59c680)"
    "D 2 (#411b91)"
    "L 5 (#8ceee2)"
    "U 2 (#caa173)"
    "L 1 (#1b58a2)"
    "U 2 (#caa171)"
    "R 2 (#7807d2)"
    "U 3 (#a77fa3)"
    "L 2 (#015232)"
    "U 2 (#7a21e3)"))

(defparameter *test-data2*
  '("D 2 (#000021)"
    "R 2 (#000020)"
    "U 2 (#000023)"
    "R 2 (#000020)"
    "U 2 (#000023)"
    "L 2 (#000022)"
    "D 2 (#000021)"
    "L 2 (#000022)"))

(defparameter *dig-regex* (re:create-scanner "([RLUD]+).+(\\d+).+([0-9a-f]{6})")
  "a regular expression to parse each line in the digger instructions into three strings: dir len and color")

(defun row (pos)
  "help me remember that row is the car in a position cons"
  (car pos))

(defun col (pos)
  "help me remember that col is the cdr in a position cons"
  (cdr pos))

(defun add-pos (x y)
  "adds two positions, x y, and returns the result"
  (cons (+ (row x) (row y)) (+ (col x) (col y))))

(defun list-lagoon-vertices (instructions)
  "given a set of strings describing a digging plan return a list of the
vertices describing the dig site"
  (let ((vertices (list (cons 0 0))) ; a list of vertex points for the dig
        (perimeter-length 0))        ; the length of all the points

    (iter (for instruction in instructions)
      (re:register-groups-bind
          ;; use regex to get the three items from each line: dir len
          ;; and color, converting len and color from strings into
          ;; numbers
          (dir (#'parse-integer len))
          ;; (#'parse-integer color :radix 16)) ; ignore for now
          (*dig-regex* instruction)

        (push (add-pos (first vertices)
                       (tr:match dir
                         ("U" (cons (- len) 0))
                         ("D" (cons len 0))
                         ("L" (cons 0 (- len)))
                         ("R" (cons 0 len))))
              vertices)

        (incf perimeter-length len))
      (finally (return (values  (rest vertices) ;; drop doubled start
                                perimeter-length))))))

(5a:test list-lagoon-vertices-test
  (5a:is (equal (list-lagoon-vertices   '("R 2 (#70c710)"
                                          "D 2 (#0dc571)"
                                          "L 2 (#5713f0)"
                                          "U 2 (#d2c081)"))
                (list (cons 2 0) (cons 2 2) (cons 0 2) (cons 0 0)))))

(defun shoelace (vertices)
  "Using the Shoelace Algorithm: given a list of points describing a
lattice polygon, return the internal area of the polygon - the
internal area is not just the points inside the perimeter, but
includes half the perimeter as well."
  (abs ; so it doesn't matter if the points go clockwise or counter.
   (/
    (iter (for (a b) on vertices)
      (when (null b) (setf b (first vertices))) ; loop back to the beginning
      (sum (- (* (row a) (col b))
              (* (row b) (col a)))))
    2)))

(5a:test shoelace-test
  (5a:is (= 4 (shoelace (list (cons 0 2) (cons 2 2) (cons 2 0) (cons 0 0)))))
  (5a:is (= 4 (shoelace (list (cons 0 0) (cons 2 0) (cons 2 2) (cons 0 2)))))
  (5a:is (= 16 (shoelace (list (cons 0 0) (cons 4 0) (cons 4 4) (cons 0 4)))))
  (5a:is (= 4 (shoelace (list-lagoon-vertices '("D 2 (#70c710)"
                                                "R 2 (#0dc571)"
                                                "U 2 (#5713f0)"
                                                "L 2 (#d2c081)"))))))

(defun day18-1 (los)
  "given a list of digging instructions return the total volume dug"
  (multiple-value-bind (vertices length)
      (list-lagoon-vertices los)
    (+ (shoelace vertices) (/ length 2) 1))) ; fill out the rest of the rim

(5a:test Day18-1-test
  (5a:is (= 62 (day18-1 *test-data*)))
  (5a:is (= 17 (day18-1 *test-data2*)))
  (5a:is (= 9  (day18-1 '("D 2 (#70c710)"
                          "R 2 (#0dc571)"
                          "U 2 (#5713f0)"
                          "L 2 (#d2c081)"))))
  (5a:is (= 25  (day18-1 '("D 4 (#70c710)"
                           "R 4 (#0dc571)"
                           "U 4 (#5713f0)"
                           "L 4 (#d2c081)"))))
  (5a:is (= 30  (day18-1 '("D 4 (#70c710)"
                           "R 5 (#0dc571)"
                           "U 4 (#5713f0)"
                           "L 5 (#d2c081)")))))

#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 18 Part 1 is ~a"
              (day18-1 (uiop:read-file-lines *data-file*))))


;; (time (format t "The answer to AOC 2023 Day 18 Part 2 is ~a"
;;	      (day18-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
