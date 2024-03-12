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

"Each city block is marked by a single digit that represents the amount
of heat loss if the crucible enters that block. The starting point,
the lava pool, is the top-left city block; the destination, the
machine parts factory, is the bottom-right city block. (Because you
already start in the top-left block, you don't incur that block's heat
loss unless you leave that block and then return to it.)

Because it is difficult to keep the top-heavy crucible going in a
straight line for very long, it can move at most three blocks in a
single direction before it must turn 90 degrees left or right. The
crucible also can't reverse direction; after entering each city block,
it may only turn left, continue straight, or turn right.

Directing the crucible from the lava pool to the machine parts
factory, but not moving more than three consecutive blocks in the same
direction, what is the least heat loss it can incur?"

LEO'S NOTES: Finally pathfinding! And this is a simple Dijkstra except
for the twist: no more than three blocks in a single direction. Can I
use the VISITED list in the Dijkstra to eliminate moves that would be
the fourth in a direction? No. VISITED is just a list of points we've
checked - it's a superset of the shortest path. Hmm.

After spending a few days noodling around I think I've come up with a
solution. Instead of a DISTANCES hash, I'm going to make a STATES hash
that will maintain several conditions for every position on the map:
DIST, PATH, VISITED.

This will actually be faster than a visited list and I think I have to
do this to satisfy the no more than three in a row rule. PATH will
show the moves in a line that it took to get to a point. 'U 'D 'L 'R.

I'll update the STATES hash whenever I check out a new point. Distance
will be updated to the new, best distance, VISITED can be set to true
when I pop that point off the priority queue. PATH is a little
tricky. Each time I move from one CELL to another I'll note the move
direction, up, down, left, right. When the move is the same as the
move to get to the previous cell, I'll push it to the PATH. If I'm
making a turn I'll clear the path and start with the new direction. I
think this works. The path in the originating cell shows the current
best path. If we try another path it will be replaced.

(After further thought I realized that since I can't backtrack i only
need to track the direction of moves: horizontal or vertical. I use 'H
and 'V.)

When there are three identical moves in the path, I can't
move again in the same direction so that won't be offered as a
potential next cell by LIST-SURROUNDS. LIST-SURROUNDS is the
gatekeeper - it doesn't let VISITED cells or cells that would require
a fourth move in the same direction through.

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
    "4322674655533"))

(defstruct (cell)
  "saved state of a cell in the map"
  (dist most-positive-fixnum :type integer) ; starts "infinitely" distant
  (path '() :type list)                     ; straight line moves 'H or 'V
  (prev (cons nil nil) :type cons)          ; track previous pos for path
  (visited nil :type boolean))              ; visited yet?

(defparameter *state* (make-hash-table :test 'equal)
  "GLOBAL: hash table key: POS => value: CELL")

(defun parse-map (los)
  "given a list of strings defining a map return a 2D array with each
 cell holding an integer from 1-9"
  (let* ((width (length (first los)))
         (height (length los))
         (map (make-array (list width height) :element-type 'integer)))

    (iter (for row below height)
      (iter (for col below width)
        (setf (aref map row col)
              (digit-char-p (elt (nth row los) col))))
      (finally (return map)))))

(defun row (pos)
  "help me remember that row is the car in a position cons"
  (car pos))

(defun col (pos)
  "help me remember that col is the cdr in a position cons"
  (cdr pos))

(defun pht (hash)
  (iter (for (key value) in-hashtable hash)
    (format t "~% ~A => ~A" key value)))

(defun get-value (pos map)
  "returns the value at a point"
  (aref map (row pos) (col pos)))

;; several utility functiions for getting and setting the values in
;; the cell struct.

(defun get-dist (pos)
  "returns the distance associated with pos in *state*"
  (cell-dist (gethash pos *state*)))

(defun set-dist (pos distance)
  "sets the distance of the position in *state*"
  (let ((cell (gethash pos *state*)))
    (setf (cell-dist cell) distance)
    (setf (gethash pos *state*) cell)))

(defun get-path (pos)
  "returns the path list followed to get to pos"
  (cell-path (gethash pos *state*)))

(defun set-path (pos path)
  "sets the path of the position in *state*"
  (let ((cell (gethash pos *state*)))
    (setf (cell-path cell) path)
    (setf (gethash pos *state*) cell)))

(defun get-prev (pos)
  "gets the previous position from a cell"
  (let ((cell (gethash pos *state*)))
    (if cell (cell-prev cell) nil)))

(defun set-prev (pos prev)
  "sets the previous position"
  (let ((cell (gethash pos *state*)))
    (setf (cell-prev cell) prev)
    (setf (gethash pos *state*) cell)))

(defun visited? (pos)
  "returns true if this pos has been visited"
  (cell-visited (gethash pos *state*)))

(defun set-visited (pos)
  "marks pos visited - returns updated cell"
  (let ((cell (gethash pos *state*)))
    (setf (cell-visited cell) t)
    (setf (gethash pos *state*) cell)))

;; now some stuff to keep track of the heading directions
(defun get-move-axis (curr next)
  "given two adjacent points, return the new heading of the move from
curr to next as 'H or 'V (for horiz or vert move)"
  (if (equal (row curr) (row next)) 'H 'V))

(defun new-path (curr next)
  "if (first path) of current position is the same as the next move
 then we're still going in the same direction so push the new move,
otherwise we've turned so replace path with the new move, returns
updated path"
  (let ((move (get-move-axis curr next)) ; what's the next move direction
        (path (get-path curr)))          ; previous moves in a line

    (if (equal move (first path))        ; still moving in same direction?
        (setf path (push move path))     ; yes add this move to the path
        (setf path (list move)))         ; else start new sequence

    path))

(defun list-surrounds (curr map max-moves)
  "The Gatekeeper. Given a point on a grid return a list of valid
surrounding points in DOWN RIGHT LEFT UP order - eliminates already
visited points and choices that would represent max-moves in the same
direction"
  (let* ((r (row curr))
         (c (col curr))
         (height (array-dimension map 0))
         (width (array-dimension map 1))
         (surrounds (list
                     (cons (1- r) c)     ; up
                     (cons (1+ r) c)     ; down
                     (cons r (1- c))     ; left
                     (cons r (1+ c)))))  ; right

    ;; remove if off grid
    (setf surrounds
          (remove-if-not                ; on grid
           #'(lambda (p) (and (< -1 (row p) height)
                              (< -1 (col p) width)))
           surrounds))

    ;; remove if previously visited
    (setf surrounds
          (remove-if                    ; been here already
           #'(lambda (p) (visited? p))
           surrounds))

    ;; remove if position would exceed max-moves in the same dir
    (setf surrounds
          (remove-if   ; exceeds max-moves in same direction
           #'(lambda (p)
               (> (length (new-path curr p)) max-moves))
           surrounds))

    ;; return filtered list
    surrounds))


(declaim (inline find-shortest-path)) ; required by enqueue and dequeue

(defun find-shortest-path (start end map max-in-a-row)
  "uses Dijkstra's algorithm to find the smallest heat loss (hereinafter
referred to as DISTANCE) from START to END on the map, avoiding moving
in the same direction more than max times in a row, returns the lowest
total distance possible"
  (let ((height (array-dimension map 0))
        (width (array-dimension map 1))
        (curr start)   ; where we are so far
        (distance 0)   ; distance traveled along best route
        (q (make-instance 'he:priority-queue))) ; sorted by least dist

    ;; set each position in states hash to cell defaults:
    ;; "infinite" distance, empty path, empty prev, unvisited
    (clrhash *state*)
    (iter (for row below height)
      (iter (for col below width)
        (setf (gethash (cons row col) *state*) (make-cell))))

    ;; zero out starting point
    (set-dist start 0)
    ;; and prime the queue with it
    (he:enqueue q start 0)

    ;; main loop - travel the map until end or empty queue
    (iter (while (he:peep-at-queue q)) ; still something in the queue

      ;; get the nearest point on the queue
      (setf curr (he:dequeue q))       ; next pos with lowest distance
      (setf distance (get-dist curr))  ; update distance
      (set-visited curr)               ; mark it visited

      (when (equal curr end)           ; at end? return
        (return-from find-shortest-path distance))

      ;; check all surrounding positions we haven't visited and aren't
      ;; more than max in the same direction (LIST-SURROUNDS ensures
      ;; this) then enqueue any improved routes
      (iter (for surr in (list-surrounds curr map max-in-a-row))
        (let ((new-dist (+ distance (get-value surr map))))
          (when (< new-dist (get-dist surr))                ; improvement?
            ;; save new, better, distance
            (set-dist surr new-dist)
            ;; and how we got here (path)
            (set-path surr (new-path curr surr))
            ;; save previous pos for path reconstruction
            (set-prev surr curr)
            (he:enqueue q surr new-dist)))) ; enqueue it

      ;; end while loop
      (finally (return distance)))))

(declaim (notinline find-shortest-path))

(defun Day17-1 (los max)
  (find-shortest-path (cons 0 0)                       ; start
                      (cons (1- (length los))
                            (1- (length (first los)))) ; end
                      (parse-map los)                  ; map
                      max))                            ; max in a row

(5a:test Day17-1-test
  (5a:is (= 102 (Day17-1 *test-data* 3))))


(defun reconstruct-path (start end)
  "returns the path from start to end along with associated cells"
  (let ((route (list end))
        (prev nil)
        (cells (list (cons end (gethash end *state*)))))

    (iter (while (not (equal (setf prev (get-prev (first route)))
                             start)))
      (push prev route)
      (push (cons prev (gethash prev *state*)) cells)
      (finally (return cells)))))

#|
0123456789012

2>>34^>>>1323 0
32v>>>35v5623 1
32552456v>>54 2
3446585845v52 3
4546657867v>6 4
14385987984v4 5
44578769877v6 6
36378779796v> 7
465496798688v 8
456467998645v 9
12246868655<v 0
25465488877v5 1
43226746555v> 2

2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533

2>>3^>>311323
32v>>5v>>>623
325524565v254
344658584v452
454665786v536
143859879v>>>
445787698776v
363787797965v
46549679868<v
45646799864v>
122468686556v
254654888773v
432267465553v
|#
#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2023 Day 17 Part 1 is ~a"
;;              (day17-1 (uiop:read-file-lines *data-file*) 3)))

;; (time (format t "The answer to AOC 2023 Day 17 Part 2 is ~a"
;;	      (day17-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
