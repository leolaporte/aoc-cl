;;;; Day17.lisp
;;;; 2023 AOC Day 17 solution
;;;; Leo Laporte
;;;; 5 March 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-heap :iterate))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day17
  (:use  #:cl :iterate)
  (:local-nicknames
   (:he :cl-heap)        ; for priority queue
   (:5a :fiveam)))       ; testing framework

(in-package :day17)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info

(defparameter *data-file* "~/cl/AOC/2023/Day_17/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 17: Clumsy Crucible ---
--- Part One ---

LEO'S NOTES: Finally pathfinding! And this is a Dijkstra with a twist:
no more than three blocks in a single direction. Can I use the
VISITED list in the Dijkstra to eliminate moves that would be the
fourth in a direction? No. VISITED is just a list of points we've
checked - it's a superset of the shortest path. Hmm.

After spending a few days noodling around I think I've come up with a
solution. Instead of just collecting vertices and costs I have to have
a more elaborate EDGE state: grid position, direction, number of moves
in the same direction. e.g.

(list (cons row col) axis-of-movement, number-of-moves along that axis)

And for every EDGE I'll store it's calculated heat loss and whether
it's been visited yet. I can do that in a global *STATE-HASH* Since
every edge is a key in the hash I'll be tracking many more points. (as
it turns out 157,146!) To reduce this number I'll only add an entry to
*STATE-HASH* when I refer to it. (get-dist) (set-dist) (set-visited)
and (visited?) create new *STATE-HASH* entries if one doesn't already
exist. Just in time hashing!

I can simplify the four directions into just two: horizontal and
vertical because I can't backtrack. That will also save me space and
time.

The most important takeaway for future is that Dijkstra can be
generalized for a variety of situations; modelling the states on the
graph as needed. And, with it a function for next states and their
costs, and a test for the target state. (For a generic A* I can
include a heuristic function, as well. I might do that if this gets
too slow.)

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("2413432311323"
    "3215453535623"
    "3255245654254"
    "3446585845452"
    "4546657867536"
    "1438598798454"
    "4457876987766"
    "3637877979653"
    "4654967986887"
    "4564679986453"
    "1224686865563"
    "2546548887735"
    "4322674655533")
  "provided example in the problem")

(defstruct (edge-state)
  "saved state of an edge in the graph - an edge is (list (cons row col)
direction move-count."
  (dist most-positive-fixnum :type integer) ; starts "infinitely" distant
  (visited nil :type boolean))              ; not visited yet

(defparameter *state-hash* (Make-hash-table :test 'equal)
  "GLOBAL: hash table  EDGE => EDGE-STATE")

(defun make-heat-loss-map (los)
  "given a list of strings defining a heat loss map return a 2D array with each
 position holding an integer from 1-9 representing the heat loss
 experienced when entering that position"
  (let* ((width (length (first los)))
         (height (length los))
         (map (make-array (list width height) :element-type 'integer)))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref map row col)
              (digit-char-p (elt (nth row los) col))))
      (finally (return map)))))

(defun pht (hash)
  "utility to print a hash to terminal"
  (iter (for (key value) in-hashtable hash)
    (format t "~% ~A => ~A" key value)))

;; Some mnemonic functions to help me remember how things work. An
;; edge is (list (cons row col) dir moves) - direction is the
;; direction from the vertex, moves is the count of moves so far in
;; that direction

(defun row (pos)
  "help me remember that row is the car in a position cons"
  (car pos))

(defun col (pos)
  "help me remember that col is the cdr in a position cons"
  (cdr pos))

(defun origin (edge)
  "given an edge (list origin dir moves) return origin"
  (first edge))

(defun dir (edge)
  "given an edge (list origin dir moves) return dir"
  (second edge))

(defun moves (edge)
  "given an edge (list origin dir moves) return moves"
  (third edge))

;; utility functions for getting and setting the values in
;; the EDGE-STATE struct.

(defun get-value (edge map)
  "returns the value of the originating vertex"
  (let ((pos (origin edge)))
    (aref map (row pos) (col pos))))

(defun get-dist (edge)
  "returns the distance associated with EDGE in *STATE-HASH*"
  (when (null (gethash edge *state-hash*))                ; doesn't exist yet
    (setf (gethash edge *state-hash*) (make-edge-state))) ; so create it
  (edge-state-dist (gethash edge *state-hash*)))

(defun set-dist (edge distance)
  "sets the distance of the edge in *state-hash*"
  (when (null (gethash edge *state-hash*))                ; doesn't exist yet
    (setf (gethash edge *state-hash*) (make-edge-state))) ; so create it
  (let ((es (gethash edge *state-hash*)))
    (setf (edge-state-dist es) distance)
    (setf (gethash edge *state-hash*) es)))

(defun visited? (edge)
  "returns true if this pos has been visited"
  (when (null (gethash edge *state-hash*))                ; doesn't exist yet
    (setf (gethash edge *state-hash*) (make-edge-state))) ; so create it
  (edge-state-visited (gethash edge *state-hash*)))

(defun set-visited (edge)
  "marks pos visited - returns updated edge-state "
  (when (null (gethash edge *state-hash*))                ; doesn't exist yet
    (setf (gethash edge *state-hash*) (make-edge-state))) ; so create it

  (let ((es (gethash edge *state-hash*)))
    (setf (edge-state-visited es) t)
    (setf (gethash edge *state-hash*) es)))

(defun list-surrounds (curr map max-moves)
  "The Gatekeeper. Given a point on a grid return a list of valid
surrounding points - eliminates already visited points and choices
that would represent max-moves in the same direction"
  (let* ((r (row (origin curr)))
         (c (col (origin curr)))
         (height (array-dimension map 0))
         (width (array-dimension map 1))
         (surrounds (list
                     (list (cons r (1- c)) 'H)
                     (list (cons r (1+ c)) 'H)
                     (list (cons (1- r) c) 'V)
                     (list (cons (1+ r) c) 'V))))

    ;; remove if off grid
    (setf surrounds
          (remove-if-not                ; on grid
           #'(lambda (s) (and (< -1 (row (origin s)) height)
                              (< -1 (col (origin s)) width)))
           surrounds))

    ;; remove if previously visited
    ;; (creates a hash entry if one doesn't exist)
    (setf surrounds
          (remove-if                    ; been here already
           #'(lambda (s) (visited? s))
           surrounds))

    ;; adjust moves
    (setf surrounds
          (mapcar #'(lambda (s)
                      (if (equal (dir curr) (dir s))  ; moving in same dir?
                          (append s (list (1+ (moves curr)))) ; increment moves
                          (append s (list 1))))  ; otherwise reset it to 1
                  surrounds))

    ;; remove if position would exceed max-moves in the same dir
    (setf surrounds
          (remove-if   ; exceeds max-moves in same direction
           #'(lambda (s) (> (moves s) max-moves))
           surrounds))

    ;; return filtered list
    surrounds))

(defun find-shortest-path (start end map max-in-a-row)
  "uses Dijkstra's algorithm to find the smallest heat loss - hereinafter
referred to as DISTANCE - from START to END on the map, avoiding
moving in the same direction more than max times in a row, returns the
lowest total distance possible"
  (let ((curr nil)           ; current edge so far
        (dirs (list 'H 'V))  ; possible axes of movement
        (distance 0)         ; distance traveled along best route
        (q (make-instance 'he:priority-queue))) ; sorted by least dist

    (clrhash *state-hash*)

    (iter (for d in dirs)
      ;; zero out starting EDGES
      (set-dist (list start d 0) 0)
      ;; and prime the queue
      (he:enqueue q (list start d 0) 0))

    ;; main loop - travel the map until end or empty queue
    (iter (while (he:peep-at-queue q)) ; still something in the queue

      ;; get the nearest EDGE on the queue
      (setf curr (he:dequeue q))       ; next EDGE with lowest distance
      (setf distance (get-dist curr))  ; update distance
      (set-visited curr)               ; mark it visited

      (when (equal (origin curr) end)  ; at end? return
        (return-from find-shortest-path distance))

      ;; check all valid surrounding positions then enqueue any
      ;; improved edges
      (iter (for surr in (list-surrounds curr map max-in-a-row))
        (let ((new-dist (+ distance (get-value surr map))))
          (when (< new-dist (get-dist surr)) ; improvement?
            ;; save new, better, distance
            (set-dist surr new-dist)
            ;; enqueue edge
            (he:enqueue q surr new-dist))))

      ;; queue is empty - end loop. Shouldn't get here.
      (finally (error "Queue is empty")))))

(defun Day17-1 (los max)
  (find-shortest-path (cons 0 0)                       ; start
                      (cons (1- (length los))
                            (1- (length (first los)))) ; end
                      (make-heat-loss-map los)         ; map
                      max))                            ; max in a row

(5a:test Day17-1-test
  (5a:is (= 102 (Day17-1 *test-data* 3))))


#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 17 Part 1 is ~a"
              (day17-1 (uiop:read-file-lines *data-file*) 3)))

;; (time (format t "The answer to AOC 2023 Day 17 Part 2 is ~a"
;;	      (day17-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
