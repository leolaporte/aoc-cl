;;;; Day_08.lisp
;;;; 2024 AOC Day 8 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 2025-06-20
;;;; Finished: 2025-06-21

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day08
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day08)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
                 --- Day 8: Resonant Collinearity ---
                           --- Part One ---

How many unique locations within the bounds of the map contain an
antinode?

LEO's NOTES:

No help from the AI here. That's no fun!

I'll use a sparse hash for the antenna map. Then it's just a matter of
identifying and counting the antinodes. The rules are a little
confusing, let's see if I can find a simpler rule.

Oh wait - it's actually very simple. I just want to duplicate the
relationship between a and b above a and below b. So I can flip the
sign and get the antinode (hence the name!)

So if I have two points (4 3) and (5 5) the antinodes are
(4 3) -> (cons (+ 4 (- 4 5)) (+ 3 (- 3 5))) or (3 . 1)
(5 5) -> (cons (+ 5 (- 5 4)) (+ 5 (- 5 3))) or (6 . 7)

For any two points (a b) the antinodes are
(cons (+ (xpos a) (- (xpos a) (xpos b)))
(+ (ypos b) (- (ypos b) (ypos a))))

Defun ANTINODE to return two values, the antinodes of a and b

For every value in the hash table calculate all the possible
antinodes. (there may be multiple points with compatible antennas so
we'll have to work all the possible pair combinations of the list of
points).

Comppile a list of antinodes, deduplicate, and remove any points off
the grid. The length of the remaining list is the answer.

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"))

(defun parse-antenna-map (los)
  "given a list of text strings representing antenna locations, return a
sparse hash with the key being the antenna name as a char (e.g.#/A)
and the value being a list of antenna locations as (list (cons x y))."
  (let ((map (make-hash-table :test 'equal))
        (width (length (first los)))
        (height (length los)))

    (iter (for y below height)
      (iter (for x below width)
        (let ((ch (char (elt los y) x)))
          (unless (equalp ch #\.)
            (setf (gethash ch map) (cons (cons x y) (gethash ch map)))))))

    ;; return the completed hash table
    map))

(defun xpos (pt)
  ;; just some mnemonics...
  (car pt))

(defun ypos (pt)
  ;; ...to make the code clearer
  (cdr pt))

(defun next-node (base mid)
  "given two points, the BASE and the MID points, returns the antinode that extends from BASE to MID to NODE"
  (cons (+ (xpos mid)
           (- (xpos mid) (xpos base)))
        (+ (ypos mid)
           (- (ypos mid) (ypos base)))))

(defun antinodes (a b)
  "given two points, return a list of their (immediate) antinodes"
  (list
   (next-node a b)    ; aim in one direction
   (next-node b a)))  ; then the other

(5a:test antinodes-test
  (5a:is (equal (antinodes (cons 4 3) (cons 5 5))
                (list (cons 6 7) (cons 3 1))))
  (5a:is (equal (antinodes (cons 8 4) (cons 5 5))
                (list (cons 2 6) (cons 11 3))))
  (5a:is (equal (antinodes (cons 6 3) (cons 4 3))  ; same row
                (list (cons 2 3) (cons 8 3))))
  (5a:is (equal (antinodes (cons 4 5) (cons 4 3))  ; same col
                (list (cons 4 1) (cons 4 7)))))

(defun all-pairs (lst)
  "given a list return a list of all possible pair combinations of items
in that list. e.g. (all-pairs '(1 2 3) -> (list '(1 2) '(1 3) '(2 3)"
  (cond ((null (rest lst)) nil)
        (t (append (mapcar (lambda (pt) (cons (first lst) (list pt)))
                           (rest lst))
                   (all-pairs (rest lst))))))

(5a:test all-pairs-test
  (5a:is (equal (all-pairs '(1 2 3))
                (list '(1 2) '(1 3) '(2 3)))))

(defun on-grid? (pt height width)
  "returns true if a given point is on a grid of given HEIGHT and WIDTH"
  (and (< -1 (xpos pt) width)
       (< -1 (ypos pt) height)))

(5a:test on-grid?-test
  (5a:is-true (on-grid? (cons 0 0) 10 10))
  (5a:is-false (on-grid? (cons -1 0) 10 10))
  (5a:is-false (on-grid? (cons 10 0) 10 10))
  (5a:is-false (on-grid? (cons 0 10) 10 10)))

(defun day08-1 (antenna-map)
  "given an antenna map, return the total number of unique points
 containing antinodes"
  (let ((height (length antenna-map))
        (width (length (first antenna-map)))
        (map (parse-antenna-map antenna-map)))

    ;; make a list of all possible antinodes on the map
    (let ((antinode-list
            (iter (for (antenna locations) in-hashtable map) ; walk the map
              (appending ; list of all antinodes for each antenna type
               (iter (for pair in (all-pairs locations))
                 (appending ; antinodes for a given antenna type
                  (antinodes (first pair) (second pair))))))))

      ;; now remove duplicates and off-grid points and count result
      (length
       (remove-if-not (lambda (pt) (on-grid? pt height width))
                      (remove-duplicates antinode-list :test 'equal))))))

(5a:test day08-1-test
  (5a:is (= (day08-1 *example*) 14)))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

"...It turns out that an antinode occurs at ANY grid position exactly in
line with at least two antennas of the same frequency,

Calculate the impact of the signal using this updated model. How many
unique locations within the bounds of the map contain an antinode?"

LEO'S NOTES:

So now the antinodes are _repeating_ points in line with the two
nodes. The essential algorithm is the same although I'll have to know
when to stop so I'll need the on-grid? test in the new ANTINODES
routine.

Also an antenna can be an antinode if it's in line with
another matching antenna - which I gather ALL antennas are unless
they're singletons. So I will add a check to see if there's only one
of any kind of antenna (actually this is unnecessary because ALL-PAIRS
returns NIL unless there's more than one antenna so the second
APPENDING clause just skips singletons. This new EXPANDED-ANTINODES is
more elaborate but all I need to write (well along with a modified
MAIN function).

One note that helped me with a little refactor is noticing the
symmetry of the operation. If I think of one point in an antenna pair
as the BASE and the other as the MID drawing a line to the ANTINODE
then flipping the MID and BASE pair aims in the other direction.

---------------------------------------------------------------------------- |#

(defun get-antinodes (a b height width)
  "given two points, generate a list of on grid antinodes extending from
a to b and beyond"
  (do* ((base a mid)
        (mid b antinode)
        (anodes nil)
        (antinode (next-node base mid) (next-node base mid)))

       ((not (on-grid? antinode height width)) anodes)

    (push antinode anodes)))

(defun expanded-antinodes (a b height width)
  (append (get-antinodes a b height width) ; in one direction
          (get-antinodes b a height width) ; in other direction
          (list a b)))                     ; include original points

(5a:test expanded-antinodes-test
  (5a:is (expanded-antinodes (cons 4 3) (cons 5 5) 10 10)
         (list  (cons 7  9) (cons 6  7) (cons 3  1) (cons 4  3) (cons 5  5))))

(defun day08-2 (antenna-map)
  "given an antenna map, return the total number of unique points
 containing antinodes"
  (let ((height (length antenna-map))
        (width (length (first antenna-map)))
        (map (parse-antenna-map antenna-map)))

    ;; make a list of all possible antinodes on the map
    (let ((antinode-list
            (iter (for (antenna locations) in-hashtable map) ; walk the map
              (appending ; list of all antinodes for each antenna type
               (iter (for pair in (all-pairs locations))
                 (appending ; antinodes for a given antenna type
                  (expanded-antinodes ; the new version for Part 2
                   (first pair) (second pair) height width)))))))

      ;; now remove duplicate points and count result
      (length (remove-duplicates antinode-list :test 'equal)))))

(5a:test day08-2-test
  (5a:is (= (day08-2 *example*) 34)))

#| -------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 08 Part 1 is ~a~%"
              (day08-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 08 Part 2 is ~a~%"
              (day08-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 08 Part 1 is 336
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000162 seconds of total run time (0.000121 user, 0.000041 system)
;; 100.00% CPU
;; 130,608 bytes consed

;; The answer to AOC 2023 Day 08 Part 2 is 1131
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000254 seconds of total run time (0.000234 user, 0.000020 system)
;; 100.00% CPU
;; 261,344 bytes consed
