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

1,0,1~1,2,1 -> (list '(1 0 1) '(1 1 1) '(1 2 1)) i.e a horizontal brick composed of three cubes.

for example?

Before disintegrating bricks, I have to make sure they're at the
lowest possible point. The function SETTLE settles all bricks,
starting with the lowest, using the following rule:

1. Falling increases the Z axis until any brick overlaps another brick
or the floor (Z=1) - two bricks cannot occupy the same point. (and by the way, each "point" is a cube 1,1,1~1,1,1 is a cube at 1,1,1

Then I can REMOVE-BRICKS using the following rules:

2. Once all bricks have fallen the following bricks can be removed
...a. bricks not touching any other brick
...b. bricks above (higher Z) all bricks they're touching
...c. bricks below brick(s) that are also supported by other bricks

I need a fast way to test for collisions, sorry overlaps, this is not a
game. Or is it? An overlap occurs when two bricks, or any brick and
the ground, have matching points. I need a function OVERLAP? that
takes two bricks and returns t if they overlap at
any point.

OVERLAP? starts by checking Z. If Z1 and Z2 are not equal, then check
X and Y. So, to put it another way, I can ignore all the bricks that
aren't one apart on the Z axis. So I think I need a hash BRICKS where
the key is Z - the lowest point of the brick - and the values are all
X Y Z points for each brick.

The most important question is how to represent the brick pile. Naively it's a list of bricks. Maybe sorted by lowest Z in each brick? Or hashed by lowest Z? A BRICK is a struct containing a list of points occupied by the brick, plus a boolean, SETTLED?, which is set T once that brick cannot move lower. Maybe each BRICK also keeps track of its top and bottom Z, TOP and BOTTOM.

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

(defstruct (brick)
  "I am a brick - this is everything you need to know about me"
  (top 0 :type integer)         ; the highest current Z
  (bottom 0 :type integer)      ; the lowest current Z
  (settled nil :type boolean)   ; fully settled yet?
  (pts '() :type list))         ; current points occupied by brick

;; some handy mnemonics
(defun xth (brick-point)
  "the x coordinate of a cube"
  (first brick-point))

(defun yth (brick-point)
  "the y coordinate of a cube"
  (second brick-point))

(defun zth (brick-point)
  "the z coordinate of a cube"
  (third brick-point))

(defun expand-points (a b)
  "given two points in the form (list x y z) defining a 3D rectangle,
return a list of all points in the rectangle"
  (let* ((x1 (xth a))
         (y1 (yth a))
         (z1 (zth a))
         (x2 (xth b))
         (y2 (yth b))
         (z2 (zth b)))

    ;; create a list of all points in brick
    (iter outer (for x from x1 to x2)
      (iter (for y from y1 to y2)
        (iter (for z from z1 to z2)
          (in outer (collect (list x y z))))))))

(defun top-edge (brick)
  "returns the highest Z coordinate in a set of points describing a brick"
  (iter (for pt in brick)
    (maximizing (zth pt))))

(defun bottom-edge (brick)
  "returns the lowest Z coordinate in a set of points describing a brick"
  (iter (for pt in brick)
    (minimizing (zth pt))))

(defun parse-bricks (los)
  "given a list of strings containing end coordinates for bricks, return
a list of BRICK structs"
  (let (bricks )

    (dolist (line los)            ; for every brick string in the list
      (let* ((ends (re:split "~" line))
             (start (mapcar #'parse-integer (re:split "," (first ends))))
             (end (mapcar #'parse-integer (re:split "," (second ends))))
             (brick-points (expand-points start end))
             (top (top-edge brick-points))
             (bottom (bottom-edge brick-points))
             (settled (= bottom 1)))    ; settled is true if at bottom
        (push (make-brick :top top
                          :bottom bottom
                          :settled settled
                          :pts brick-points)
              bricks)))
    bricks))

;; The Eric saieth: "Two bricks cannot occupy the same position"
(defun collision? (coords bricks)
  "returns true if a brick represented by the points COORDS overlaps any brick in a list of brick structs, BRICKS"
  (dolist (pts coords)
    (iter (for b in bricks)
      (never (intersection pts (brick-pts b) :test #'equal)))))

(5a:test collision?-test
  (let (brix (parse-bricks *example*))
    (5a:is-true (collision? (list '(0 1 6)) 6 tops))
    (5a:is-false (collision? (list '(0 0 8)) 8 tops))
    (5a:is-true (collision? (list '(0 1 9) '(1 1 9)) 9 tops))
    (5a:is-false (collision? (expand-points '(1 1 7) '(1 2 8)) 8 tops))))

(defun brick-exists? (brick bricks)
  "returns true if brick exists in a list of bricks"
  (iter (for b in bricks)
    (thereis (equalp b brick))))

(5a:test brick-exists?-test
  (5a:is-true (brick-exists? (expand-points '(0 0 0) '(0 0 1))
                             (list (list '(3 4 5) '(5 6 7) '(7 8 9))
                                   (list '(0 0 0) '(0 0 1))))))

(defun shift-brick (old-brick new-brick tops bottoms)
  "removes OLD-BRICK from and adds NEW-BRICK to TOPS and BOTTOMS hashes"
  (let* ((old-top (top-edge old-brick))
         (old-bottom (bottom-edge old-brick))
         (new-top (top-edge new-brick))
         (new-bottom (bottom-edge new-brick))
         (old-row-tops (gethash old-top tops))
         (old-row-bottoms (gethash old-bottom bottoms)))

    ;; remove old brick from tops and bottoms
    (setf (gethash old-top tops)
          (remove-if #'(lambda (b) (equalp old-brick b)) old-row-tops))
    (setf (gethash old-bottom bottoms)
          (remove-if #'(lambda (b) (equalp old-brick b)) old-row-bottoms))

    ;; add new brick to tops and bottoms
    (multiple-value-bind (val res) (gethash new-top tops)
      (if res
          ;; add on to list of bricks already here
          (setf (gethash new-top tops) (append (list new-brick) val))
          ;; else start list of bricks at this Z
          (setf (gethash new-top tops) (list new-brick))))

    ;; add to hash of BOTTOMS
    (multiple-value-bind (val res) (gethash new-bottom bottoms)
      (if res
          ;; add on to list of bricks already here
          (setf (gethash new-bottom bottoms) (append (list new-brick) val))
          ;; else start list of bricks at this Z
          (setf (gethash new-bottom bottoms) (list new-brick))))))

(defun move-brick (brick n)
  "moves entire brick n levels"
  (iter (for point in brick)
    (incf (nth 2 point) n)
    (collect point into points)
    (finally (return points))))

(5a:test move-brick-test
  (5a:is (equalp (move-brick (expand-points '(0 0 5) '(0 0 7)) -2)
                 (list '(0 0 3) '(0 0 4) '(0 0 5)))))

(defun settle-bricks (tops bottoms)
  "given a map of bricks, settle the map until all bricks are either
 touching the ground or resting on a lower brick"
  (let ((levels (sort (ax:hash-table-keys bottoms) #'<)))

    (dolist (level levels) ; start at the bottom and work our way up
      (dolist (brick (gethash level bottoms)) ; for each brick at level
        ;; this loop moves the brick down level by level until it hits
        ;; another brick, then returns the adjusted brick
        (multiple-value-bind (new-brick new-level)
            (do ((moves 0 (1- moves)) ; number of levels to sink
                 (b brick (move-brick b moves))) ; next legal brick
                ((or (= 1 (+ level moves)) ; bottom
                     ;; or collision with next lower level
                     (collision? (move-brick b moves) (+ moves level) tops))
                 (values b moves)))

          (when (not (equalp brick new-brick)) ; did we move?
            (shift-brick brick new-brick tops bottoms))))))

  (values tops bottoms))

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
