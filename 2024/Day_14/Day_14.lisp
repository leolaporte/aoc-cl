;;;; Day14.lisp
;;;; 2024 AOC Day 14 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 2025-06-22
;;;; Finished: 2025-06-23

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day14
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day14)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/Day_14/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 14: Restroom Redoubt ---
--- Part One ---

"When a robot would run into an edge of the space they're in,
they instead teleport to the other side.

To determine the safest area, count the number of robots in each
quadrant after 100 seconds. Robots that are exactly in the
middle (horizontally or vertically) don't count as being in any
quadrant. Multiply the four totals to get the safety factor.

What will the safety factor be after exactly 100 seconds have
elapsed?"

LEO's NOTES:

Start by parsing the data into a list of robot records with:
* POS (cons x y)
* XV (velocity on the X axis)
* YV (velocity on the Y axis)

Then process each record into a list FINAL-POSITIONS containing each
robots final position using MOVE-ROBOT (start, xv, yv, steps) (don't
forget to wrap around if needed)

Sort FINAL-POSITIONS into quadrants, count the robots in each of four
quadrants, multiply those numbers together to get the solution to part
one.

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("P=0,4 v=3,-3"
    "p=6,3 v=-1,-3"
    "p=10,3 v=-1,2"
    "p=2,0 v=2,-1"
    "p=0,0 v=1,3"
    "p=3,0 v=-2,-2"
    "p=7,6 v=-1,-3"
    "p=3,0 v=-1,-2"
    "p=9,3 v=2,3"
    "p=7,3 v=-1,2"
    "p=2,4 v=2,-3"
    "p=9,5 v=-3,-3"))

(defparameter *time* 100)
(defparameter *height* 103)
(defparameter *width* 101)

(defstruct robot
  (pos (cons 0 0) :type CONS) ; current position as (x . y)
  (xv 0 :type INTEGER)        ; horizontal velocity
  (yv 0 :type INTEGER))       ; vertical velocity

(defun parse-robot-rules (los)
  "given rules for robot positioning and movement, return a list of robot
records populated with respective locations and movement for each robot"
  (let ((robots nil))

    (dolist (robot los)
      (let ((digits (mapcar #'parse-integer
                            (re:all-matches-as-strings "-?\\d+" robot))))
        (push (make-robot :pos (cons (first digits) (second digits))
                          :xv (third digits)
                          :yv (fourth digits)) robots)))
    robots))

(defun move-robot (robot moves width height)
  "given a robot, move the number of moves, return the updated robot
record - use the provided height and width to wrap around"
  (let ((position (robot-pos robot))
        (xv (robot-xv robot))
        (yv (robot-yv robot)))

    ;; move it all the moves - ignoring borders
    (setf position
          (cons (+ (car position) (* xv moves))
                (+ (cdr position) (* yv moves))))

    ;; use mod to adjust for wraparounds, return new robot
    (make-robot :pos (cons (mod (car position) width)
                           (mod (cdr position) height))
                :xv xv
                :yv yv)))

(5a:test move-robot-test
  (5a:is (equalp (move-robot (make-robot :pos (cons 0 0) :xv 1 :yv 1) 1 11 7)
                 (make-robot  :pos (cons 1 1) :XV 1 :YV 1)))
  (5a:is (equalp (move-robot (make-robot :pos (cons 0 0) :xv -1 :yv -1) 1 11 7)
                 (make-robot  :POS(cons 10 6) :XV -1 :YV -1)))
  (5a:is (equalp (move-robot (make-robot :pos (cons 0 0) :xv 1 :yv 1) 11 11 7)
                 (make-robot  :POS(cons 0 4) :XV 1 :YV 1)))
  (5a:is (equalp (move-robot (make-robot :pos (cons 0 0) :xv 1 :yv 1) 100 11 7)
                 (make-robot  :POS(cons 1 2) :XV 1 :YV 1))))

(defun day14-1 (los moves width height)
  (let* ((robots-start (parse-robot-rules los))
         (robots-end (mapcar (lambda (r) (move-robot r moves width height))
                             robots-start))
         (h-divider (floor height 2))
         (v-divider (floor width 2))
         (q1 0) (q2 0) (q3 0) (q4 0))      ; the quadrant counts

    (dolist (r robots-end) ; sort the robots into quadrants
      (let ((x (car (robot-pos r)))
            (y (cdr (robot-pos r))))
        (cond ((and (< x v-divider) (< y h-divider))
               (incf q1)) ; first quadrant
              ((and (> x v-divider) (< y h-divider))
               (incf q2)) ; second quad
              ((and (< x v-divider) (> y h-divider))
               (incf q3)) ; third
              ((and (> x v-divider) (> y h-divider))
               (incf q4))))) ; fourth

    (* q1 q2 q3 q4)))

(5a:test Day14-1-test
  (5a:is (= 12 (day14-1 *example* *time* 11 7))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"very rarely, most of the robots should arrange themselves into a
picture of a Christmas tree. What is the fewest number of seconds that
must elapse for the robots to display the Easter egg?"

LEO'S NOTES

Well it will be easy to run this a step at a time, but what the heck
does a Christmas tree look like? Is there some way I can test for an
organized layout? If it's a drawing at least some (most?) of the
robots will be adjacent. As long as the christmas tree doesn't have
twinkling lights. The most bothers me. It could be a very tiny
Christmas tree with lots of random points around it. So I have to
assume that more than half of the robots will be organized into an
image. Maybe only 51%.

So - figure out how many robots there are, then run step by step until
say, 51%. are next to another robot? I suppose I'll also have to write
a routine to display the result - just for fun.

Better yet, why don't I just look for a row of robots? I don't know
how big the tree is but if there are more than a few in a line that
could be the bottom of the tree. It should be an odd number so the
tree is symmetric, so test for 5 or more?

I'll have to print it out each time to check to see if there's a
tree. Sheesh. That seems so manual. But there's no platonic ideal
Christmas Tree so...

Paul Holder made an interesting suggestion. In part one there are
often multiple robots in the same position. That's probably not true
in part two since Eric would have made the drawing then worked
backwards - so what if you just test to see if each robot is in a
unique position. That would work very fast. Let's try.

Wow! It's a big assumption but it worked! (My look for 5 in a row
would have worked, too.)

---------------------------------------------------------------------------- |#

(defun move-one (robots width height)
  "moves a list of robots once"
  (mapcar (lambda (r) (move-robot r 1 width height)) robots))

(defun print-robot-map (robots width height)
  "prints out the map of robots"
  (let ((positions (mapcar (lambda (r) (robot-pos r)) robots)))
    (iter (for row below height)
      (format t "~&")
      (iter (for col below width)
        (if (member (cons col row) positions :test #'equal)
            (format t "*")
            (format t "."))))))

(defun day14-2 (los)
  "given a list of robots and their velocities, return the number of
 moves it will take for them to form the image of a christmas tree"
  (let* ((robots (parse-robot-rules los))
         (unique-robots (length los)) ; actual number of robots
         (moves 0))

    (loop
      ;; now move all the robots one step and check for uniqueness
      (incf moves)
      (setf robots (move-one robots *width* *height*))

      (when (= unique-robots ;; the original number of robots
               (length (remove-duplicates (iter (for r in robots)
                                            (collecting (robot-pos r)))
                                          :test #'equal))) ; # unique posns
        ;; (print-robot-map robots *width* *height*) ; you can print it to see
        (return-from day14-2 moves)))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 14 Part 1 is ~a"
              (day14-1
               (uiop:read-file-lines *data-file*) *time* *width* *height*)))

(time (format t "The answer to AOC 2024 Day 14 Part 2 is ~a"
              (day14-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2024 Day 14 Part 1 is 226236192
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000438 seconds of total run time (0.000403 user, 0.000035 system)
;; 100.00% CPU
;; 131,040 bytes consed

;; The answer to AOC 2024 Day 14 Part 2 is 8168
;; Evaluation took:
;; 0.279 seconds of real time
;; 0.280537 seconds of total run time (0.275609 user, 0.004928 system)
;; [ Real times consist of 0.012 seconds GC time, and 0.267 seconds non-GC time. ]
;; [ Run times consist of 0.010 seconds GC time, and 0.271 seconds non-GC time. ]
;; 100.72% CPU
;; 639,741,680 bytes consed
