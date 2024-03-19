;;;; Day18.lisp
;;;; 2023 AOC Day 18 solution
;;;; Leo Laporte
;;;; 16-19 March 2024, Cabo San Lucas

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

(defun row (pos)
  "help me remember that row is the car in a position cons"
  (car pos))

(defun col (pos)
  "help me remember that col is the cdr in a position cons"
  (cdr pos))

(defun add-pos (x y)
  "adds two positions, x y, and returns the result"
  (cons (+ (row x) (row y)) (+ (col x) (col y))))

(5a:test add-pos-test
  (5a:is (equal (add-pos (cons 0 0) (cons 5 0)) (cons 5 0)))
  (5a:is (equal (add-pos (cons 0 0) (cons -5 0)) (cons -5 0)))
  (5a:is (equal (add-pos (cons 0 0) (cons 5 5)) (cons 5 5)))
  (5a:is (equal (add-pos (cons 0 0) (cons -5 5)) (cons -5 5)))
  (5a:is (equal (add-pos (cons 0 0) (cons 5 -5)) (cons 5 -5))))

(defun list-lagoon-vertices (instructions)
  "given a set of strings describing a digging plan return a list of the
vertices describing the dig site"
  (let ((vertices (list (cons 0 0))) ; a list of vertex points for the dig
        (perimeter-length 0))        ; the length of all the points

    (iter (for instruction in instructions)
      (destructuring-bind (dir len color) (str:words instruction) ; sep words
        (declare (ignore color)) ; for now
        (setf len (parse-integer len))
        (incf perimeter-length len)
        (push (add-pos (first vertices)
                       (tr:match dir
                         ("U" (cons (- len) 0))
                         ("D" (cons len 0))
                         ("L" (cons 0 (- len)))
                         ("R" (cons 0 len))))
              vertices))
      (finally (return (values (reverse (rest vertices)) ;; drop doubled start
                               perimeter-length))))))

(5a:test list-lagoon-vertices-test
  (5a:is (equal (list-lagoon-vertices   '("R 2 (#70c710)"
                                          "D 2 (#0dc571)"
                                          "L 2 (#5713f0)"
                                          "U 2 (#d2c081)"))
                (reverse (list (cons 2 0) (cons 2 2) (cons 0 2) (cons 0 0)))))

  (5a:is (equal (list-lagoon-vertices   '("R 1 (#70c710)"
                                          "D 2 (#0dc571)"
                                          "L 1 (#5713f0)"
                                          "U 2 (#d2c081)"))

                (reverse (list (cons 2 0) (cons 2 1) (cons 0 1) (cons 0 0))))))

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
  (5a:is (= 62 (day18-1 *test-data*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

"Each hexadecimal code is six hexadecimal digits long. The first five hexadecimal digits encode the distance in meters as a five-digit hexadecimal number. The last hexadecimal digit encodes the direction to dig: 0 means R, 1 means D, 2 means L, and 3 means U."

---------------------------------------------------------------------------- |#

(defparameter *regex* (re:create-scanner "#(\\p{Hex_Digit}{5})(\\d)")
  "regex to split the hex color code into length and direction")

(defun list-fixed-lagoon-vertices (insts)
  "parse the given instructions, this time using the hex color codes
 for len and dir"
  (let ((vertices (list (cons 0 0)))
        (perimeter-length 0))

    (dolist (i insts)
      (re:register-groups-bind
          (len dir)              ; extract the len and dir
          (*regex* i)            ; from i using the regex above

        (setf len (parse-integer len :radix 16)) ; convert to decimal
        (incf perimeter-length len)  ; add up the lengths to get perimeter

        ;; create the next vertex using the length and dir parameters
        ;; then push it to the list
        (push
         (add-pos (first vertices)
                  (tr:match dir
                    ("0" (cons 0 len))     ; right
                    ("1" (cons len 0))     ; down
                    ("2" (cons 0 (- len))) ; left
                    ("3" (cons (- len) 0)) ; up
                    (otherwise (error "Can't parse direction"))))
         vertices)))

    (values (reverse (rest vertices)) ;; drop doubled start, return verts
            perimeter-length)))       ;; and perimeter

(defun day18-2 (los)
  "given a list of digging instructions return the total volume dug"
  (multiple-value-bind (vertices length)
      (list-fixed-lagoon-vertices los)
    (+ (shoelace vertices) (/ length 2) 1))) ; fill out the rest of the rim

(5a:test Day18-2-test
  (5a:is (= 952408144115 (day18-2 *test-data*))))


;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 18 Part 1 is ~a"
              (day18-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 18 Part 2 is ~a"
              (day18-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 18 Part 1 is 50465
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000500 seconds of total run time (0.000456 user, 0.000044 system)
;; 100.00% CPU
;; 393,152 bytes consed

;; The answer to AOC 2023 Day 18 Part 2 is 82712746433310
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001283 seconds of total run time (0.001271 user, 0.000012 system)
;; 100.00% CPU
;; 196,560 bytes consed
