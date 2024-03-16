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

And for every EDGE I'll store its calculated heat loss and whether
it's been visited yet. I can do that in a global *STATE-HASH* Since
every edge is a key in the hash I'll be tracking many more keys. (as
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

(defun make-heat-loss-grid (los)
  "given a list of strings defining a heat loss grid return a 2D array with each
 position holding an integer from 1-9 representing the heat loss
 experienced when entering that position"
  (let* ((width (length (first los)))
         (height (length los))
         (grid (make-array (list height width) :element-type 'integer)))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref grid row col)
              (digit-char-p (elt (nth row los) col))))
      (finally (return grid)))))

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

(defun get-value (edge grid)
  "returns the value of the originating vertex"
  (let ((pos (origin edge)))
    (aref grid (row pos) (col pos))))

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

(defun manhattan-distance (a b)
  "computes the manhattan distance between two points on a grid - A*
heuristic for cl-heap"
  (+ (abs (- (row a) (row b))) (abs (- (col a) (col b)))))

(defun list-surrounds (curr grid max)
  "The Gatekeeper. Given a point on a grid return a list of valid
surrounding points - eliminates already visited points and points off
the grid."
  (let* ((r (row (origin curr)))
         (c (col (origin curr)))
         (height (array-dimension grid 0))
         (width (array-dimension grid 1))
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
           #'(lambda (s) (> (moves s) max))
           surrounds))

    ;; return filtered list
    surrounds))

(defun find-shortest-path (start end grid max)
  "uses Dijkstra's algorithm to find the smallest heat loss - hereinafter
referred to as DISTANCE - from START to END on the grid, avoiding
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

    ;; main loop - travel the grid until end or empty queue
    (iter (while (he:peep-at-queue q)) ; still something in the queue

      ;; get the nearest EDGE on the queue
      (setf curr (he:dequeue q))       ; next EDGE with lowest distance
      (setf distance (get-dist curr))  ; update distance
      (set-visited curr)               ; mark it visited

      (when (equal (origin curr) end)  ; at end? return
        (return-from find-shortest-path distance))

      ;; check all valid surrounding positions then enqueue any
      ;; improved edges
      (iter (for surr in (list-surrounds curr grid max))
        (let ((new-dist (+ distance (get-value surr grid))))
          (when (< new-dist (get-dist surr)) ; improvement?
            ;; save new, better, distance
            (set-dist surr new-dist)
            ;; enqueue edge
            (he:enqueue q surr new-dist))))

      ;; queue is empty - end loop. Shouldn't get here.
      (finally (error "Queue is empty")))))

(defun Day17-1 (los max)
  (find-shortest-path
   (cons 0 0)                       ; start
   (cons (1- (length los))
         (1- (length (first los)))) ; end
   (make-heat-loss-grid los)         ; grid
   max))                            ; max in a row

(5a:test Day17-1-test
  (5a:is (= 102 (Day17-1 *test-data* 3))))

#| ----------------------------------------------------------------------------

--- Part Two ---

"Once an ultra crucible starts moving in a direction, it needs to move
a minimum of four blocks in that direction before it can turn (or even
before it can stop at the end). However, it will eventually start to
get wobbly: an ultra crucible can move a maximum of ten consecutive
blocks without turning."

LEO'S NOTES: Looks like I can isolate this to the LIST-SURROUNDS
function. The logic is different now. I will only offer surrounds that
are between four and ten moves on an axis. But I have to make sure
that I account for the heat loss of every block I pass through.

Let's think this through. Surrounds is never going to get a point with
moves < 4. So if we're going in a straight line still add 1 up to a
max of 10. After 10 moves it will have to turn and move 4 moves on
that axis.

Mean while I have to have a new function to calculate the heat loss
since I get loss from every block I pass through. So my new surrounds
function will have to return both a new edge and the total heat loss
up to now.

Looks like I have to update both the get-shortest-path and
list-surrounds. I'll create two new functions: get-ultra-shortest-path
and list-ultra-surrounds. My ultra-surrounds will return a cons of
edges and costs. (cons edge cost) where cost is the additional cost to
get to this point. (NB: Homework for another day: from now on my
generic Dijkstra sould return both edges and costs from the
list-surrounds func.)

----------------------------------------------------------------------------|#

(defparameter *test-data2*
  '("111111111111"
    "999999999991"
    "999999999991"
    "999999999991"
    "999999999991"))

(defparameter *test-data3*
  '("1111119999"
    "9111111111"
    "9199919991"
    "9199919991"
    "9199919991"
    "9111119991"
    "9999999991"
    "9999999991"
    "9999999991"
    "9999999991"))

(defparameter *test-data4*
  '("1111199999999999"
    "9999199999999999"
    "9999199999999999"
    "9999199999999999"
    "9999111111111111"))

(defparameter *test-data5*
  '("19999"
    "19999"
    "19999"
    "19999"
    "11111"))

(defun heat-loss (start end grid)
  "given a starting point and an ending point return the total heat loss
from touching each point from start + 1 to end"
  (let ((height (array-dimension grid 0))
        (width (array-dimension grid 1))
        (r1 (row start))
        (c1 (col start))
        (r2 (row end))
        (c2 (col end)))

    ;; first make sure we're on the grid
    (when (not (and (< -1 c1 width)
                    (< -1 c2 width)
                    (< -1 r1 height)
                    (< -1 r2 height)))
      (return-from heat-loss nil))

    (cond ((= c1 c2)      ; moving vertically
           (if (< r1 r2)
               (iter (for r from (1+ r1) to r2)  ; moving D
                 (sum (aref grid r c1)))
               (iter (for r from r2 below r1)    ; moving U
                 (sum (aref grid r c1)))))

          ((= r1 r2)    ; horizontally
           (if (< c1 c2)
               (iter (for c from (1+ c1) to c2)  ; moving R
                 (sum (aref grid r1 c)))
               (iter (for c from c2 below c1)    ; moving L
                 (sum (aref grid r1 c)))))

          (t (error "heat loss error ~A ~A" start end)))))

(5a:test heat-loss-test
  (let ((grid (make-heat-loss-grid *test-data*)))
    (5a:is-false (heat-loss (cons 0 0) (cons 0 -1) grid))
    (5a:is (= 12 (heat-loss (cons 0 0) (cons 0 4) grid)))
    (5a:is (= 14 (heat-loss (cons 0 0) (cons 5 0) grid)))))

(defun list-ultra-surrounds (edge grid mini maxi)
  "The Gatekeeper. Given the current state (an edge) return the next states to examine and the cost each adds as a list of (cons (list edge dir moves) cost)"
  (let* ((curr-pos (origin edge)) ; starting position
         (r (row curr-pos))       ; starting row
         (c (col curr-pos))       ; starting column
         (cnt (moves edge))       ; number of moves in this dir so far
         (height (array-dimension grid 0))
         (width (array-dimension grid 1))
         (surrounds '()))  ; the list of next states to examine

    ;; ok now lets figure out where the next states and costs are
    ;; creating a list of surrounds in the form:
    ;; (cons (list edge dir moves) heat-loss)

    ;; for each number of moves so far there are 3 possibilities:
    ;; 1. we've moved 10 in this direction: turn and move 4 immediately
    ;; 2. in between 4 and 11 moves: go 1 in current direction or 4 in
    ;; new dir
    ;; 3. It's the first move on the grid (cnt = 0) so move 4
    (cond
      ((or (>= cnt maxi)     ;; moved 10 already so turn and go four
           (= cnt 0))        ;; first move!
       (when (equal (dir edge) 'V) ; we have been moving vertically
         (setf surrounds       ; so turn ...
               (list
                (let ((next-pos (cons r (- c mini)))) ; left 4
                  (cons (list next-pos 'H mini)
                        (heat-loss curr-pos next-pos grid)))
                (let ((next-pos (cons r (+ c mini)))) ; right 4
                  (cons (list next-pos 'H mini)
                        (heat-loss curr-pos next-pos grid))))))

       (when (equal (dir edge) 'H) ; we have been moving horizontally
         (setf surrounds       ; so turn ..
               (list
                (let ((next-pos (cons (- r mini) c))) ; up 4
                  (cons (list next-pos 'V mini)
                        (heat-loss curr-pos next-pos grid)))
                (let ((next-pos (cons (+ r mini) c))) ; down 4
                  (cons (list next-pos 'V mini)
                        (heat-loss curr-pos next-pos grid)))))))

      ;; in between 4 and 11 moves in this direction, move one in each dir
      ;; unless there's a turn in which case move four
      ((< (1- mini) cnt maxi)
       (when (equal (dir edge) 'H)
         (setf surrounds
               (list
                (let ((next-pos (cons r (1- c)))) ; left
                  (cons (list next-pos 'H (1+ cnt))
                        (heat-loss curr-pos next-pos grid)))

                (Let ((next-pos (cons r (1+ c)))) ; right
                  (cons (list next-pos 'H (1+ cnt))
                        (heat-loss curr-pos next-pos grid)))

                (let ((next-pos (cons (- r mini) c))) ; turn up
                  (cons (list next-pos 'V mini)
                        (heat-loss curr-pos next-pos grid)))

                (let ((next-pos (cons (+ r mini) c))) ; turn down
                  (cons (list next-pos 'V mini)
                        (heat-loss curr-pos next-pos grid))))))

       (when (equal (dir edge) 'V)
         (setf surrounds
               (list
                (let ((next-pos (cons (1- r) c))) ; up
                  (cons (list next-pos 'V (1+ cnt))
                        (heat-loss curr-pos next-pos grid)))

                (let ((next-pos (cons (1+ r) c))) ; down
                  (cons (list next-pos 'V (1+ cnt))
                        (heat-loss curr-pos next-pos grid)))

                (let ((next-pos (cons r (- c mini)))) ; turn left
                  (cons (list next-pos 'H mini)
                        (heat-loss curr-pos next-pos grid)))

                (let ((next-pos (cons r (+ c mini)))) ; turn right
                  (cons (list next-pos 'H mini)
                        (heat-loss curr-pos next-pos grid)))))))

      (t (error "Count is out of bounds ~A" cnt)))

    ;; ok we've generated the surrounds so now lets filter out the flops
    ;; remember SURROUNDS is (list (cons edge heat-loss))
    ;; remove if off grid
    (setf surrounds
          (remove-if-not                ; on grid
           #'(lambda (s) (and (< -1 (row (origin (car s))) height)
                              (< -1 (col (origin (car s))) width)))
           surrounds))

    ;; remove if previously visited
    ;; (creates a hash entry if one doesn't exist)
    (setf surrounds
          (remove-if      ; been to this edge already
           #'(lambda (s) (visited? (car s)))
           surrounds))

    ;; return filtered list
    surrounds))

(defun find-ultra-shortest-path (start end grid mini maxi)
  "uses Dijkstra's algorithm to find the smallest heat loss - hereinafter
referred to as DISTANCE - from START to END on the grid, avoiding
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

    ;; main loop - travel the grid until end or empty queue
    (iter (while (he:peep-at-queue q)) ; still something in the queue

      ;; get the nearest EDGE on the queue
      (setf curr (he:dequeue q))       ; next EDGE with lowest distance
      (setf distance (get-dist curr))  ; update distance
      (set-visited curr)               ; mark it visited

      (when (equal (origin curr) end)  ; at the end point
        (return-from find-ultra-shortest-path distance))

      ;; check all valid surrounding positions then enqueue any
      ;; improved edges - ultra surround returns a cons of edge and cost
      (iter (for surr-cost in (list-ultra-surrounds curr grid mini maxi))
        (let ((surr (car surr-cost))
              (new-dist (+ distance (cdr surr-cost))))
          (when (< new-dist (get-dist surr)) ; improvement?
            ;; save new, better, distance
            (set-dist surr new-dist)
            ;; enqueue edge
            (he:enqueue q surr new-dist))))

      ;; queue is empty - end loop. Shouldn't get here.
      (finally (error "Queue is empty")))))

(defun Day17-2 (los mini maxi)
  (find-ultra-shortest-path
   (cons 0 0)                       ; start
   (cons (1- (length los))
         (1- (length (first los)))) ; end
   (make-heat-loss-grid los)        ; grid
   mini maxi))                      ; min & max in a row

(5a:test Day17-2-test
  (5a:is (= 94 (Day17-2 *test-data* 4 10)))
  (5a:is (= 71 (Day17-2 *test-data2* 4 10)))
  (5a:is (= 34 (Day17-2 *test-data3* 4 10))) ; from reddit
  (5a:is (= 51 (Day17-2 *test-data4* 4 10))) ;    "
  (5a:is (= 8 (Day17-2 *test-data5* 4 10)))) ;    "

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 17 Part 1 is ~a"
              (day17-1 (uiop:read-file-lines *data-file*) 3)))

(time (format t "The answer to AOC 2023 Day 17 Part 2 is ~a"
              (day17-2 (uiop:read-file-lines *data-file*) 4 10)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
