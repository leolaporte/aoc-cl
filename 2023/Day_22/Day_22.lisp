;;;; Day22.lisp
;;;; 2023 AOC Day 22 solution
;;;; Leo Laporte
;;;; Started: 28 April 2024, Petaluma, CA

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day22
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)               ; misc utilities
   (:ax :alexandria)             ; ditto
   (:re :cl-ppcre)               ; regex
   (:tr :trivia)                 ; pattern matching
   (:tp :trivia.ppcre)           ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day22) ; synchronize package and dir with C-c ~

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

#| ----------------------------------------------------------------------------
--- Day 22: Sand Slabs ---
--- Part One ---

https://adventofcode.com/2023/day/22

LEO'S NOTES: Forget bricks, and falling, and even a 3D plane. What if
I get abstract and consider each brick to be a list of x,y,z points.

1,0,1~1,2,1 -> (list '(1 0 1) '(1 1 1) '(1 2 1))

for example?

Before disintegrating bricks, I have to make sure they're at the
lowest possible point. The functioon SETTLE settles all bricks,
starting with the lowest, using the following rule:

1. Falling increases the Z axis until any brick is touched or Z=1
...NB a brick can span multiple Zs, so use edge with the smallest Z
for touches.

Then I can REMOVE-BRICKS using the following rules:

2. Once all bricks have fallen the following bricks can be removed
...a. bricks not touching any other brick
...b. bricks above (higher Z) all bricks they're touching
...c. bricks below brick(s) that are also supported by other bricks

I need a fast way to test for collisions, sorry touches, this is not a
game. Or is it? A collision occurs when two bricks, or any brick and
the ground, have adjacent points. I need a function TOUCHING? that
takes two bricks and returns t if they touch at any point.

TOUCHING? starts by checking Z. If Z1 and Z2 are 1 apart, then check X
and Y. So, to put it another way, I can ignore all the bricks that
aren't one apart on the Z axis. So I think I need a hash BRICKS where
the key is Z and the values are all X Y Z points for each brick.

---------------------------------------------------------------------------- |#

(defparameter *data-file* "input.txt"
  "Downloaded from the AoC problem set")

(defparameter *example*
  '("1,0,1~1,2,1"
    "0,0,2~2,0,2"
    "0,2,3~2,2,3"
    "0,0,4~0,2,4"
    "2,0,5~2,2,5"
    "0,1,6~2,1,6"
    "1,1,8~1,1,9"))

(defun expand-points (a b)
  "given two points in the form (list x y z) defining a 3D rectangle,
return a list of all perimeter points of the rectangle"
  (let ((r1 (first a))
        (c1 (second a))
        (z1 (third a))
        (r2 (first b))
        (c2 (second b))
        (z2 (third b))
        (pts (list a b)))

    (when (> r2 r1)
      (iter (for x from r1 to r2) (push (list x c1 z1) pts)))

    (when (> c2 c1)
      (iter (for x from c1 to c2) (push (list r1 x z1) pts)))

    (when (> z2 z1)
      (iter (for x from z1 to z2) (push (list r1 c1 x) pts)))

    (remove-duplicates pts :test 'equal)))

(defun make-brick-map (los)
  "given a list of strings containing end coordinates for bricks,
 create a hash of bricks with the key being the Z coordinate and a
 value being a list of lists of all XY points occupied by all the
 bricks at that Z (more that one brick can occupy each Z slot)."
  (let ((bricks (make-hash-table :test 'equal :size (length los))))

    (dolist (line los)
      (let* ((ends (re:split "~" line))
             (start (mapcar #'parse-integer (re:split "," (first ends))))
             (end (mapcar #'parse-integer (re:split "," (second ends))))
             (brick (expand-points start end))
             (z (if (< (third start) (third end))    ; bottom height
                    (third start)
                    (third end))))

        (if (gethash z bricks)       ; already some entries here?
            (setf (gethash z bricks) ; update with new brick
                  (list brick (gethash z bricks)))
            ;; else
            (setf (gethash z bricks) brick)))) ; create entry

    bricks))

(defun settle-bricks (map)
  "given a map of bricks, settle the map until all bricks are either
 touching the ground or resting on a lower brick"
  )

(defun touching? (a b)
  "given two bricks, return true if they're adjacent"
  )

(defun remove-bricks (map)
  "given a map of settled bricks, returns a list of bricks that can be
 removed"
  )

(defun day22-1 (los)
  "given a text description of an array of bricks, return the number of bricks that can be safely disintegrated after settling"
  )

(5a:test day22-1-test
  (5a:is (= 5 (day22-1 *example*))))


#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 22 Part 1 is ~a"
;;	      (day22-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 22 Part 2 is ~a"
;;	      (day22-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
