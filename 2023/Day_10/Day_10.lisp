;;;; Day10.lisp
;;;; 2023 AOC Day 10 solution
;;;; Leo Laporte
;;;; 7- Jan 2024

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :iterate))

(defpackage :day10
  (:use #:cl #:iterate)                 ; use ITERATE instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)                      ; regex
   (:5a :fiveam)))                     ; testing

(in-package :day10)
(setf 5a:*run-test-when-defined* t)         ; test as we go
(declaim (optimize (debug 3) (safety 3)))   ; max debugging info
;; (declaim (optimize (speed 3))            ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_10/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 10: Pipe Maze ---
--- Part One ---

You make a quick sketch of all of the surface pipes you can see (your
puzzle input).

The pipes are arranged in a two-dimensional grid of tiles:

| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.

S is the starting position of the animal; there is a pipe on this
tile, but your sketch doesn't show what shape the pipe has.

Find the single giant loop starting at S. How many steps along the
loop does it take to get from the starting position to the point
farthest from the starting position?

LEO'S NOTES: I'm going to stop using LOOP and start using the more
lispy ITERATE library. We'll see how it goes.

At first I thought I might be doing some pathfinding here, but it
seems to be much simpler. Apparently there's only one path from S to
S. So no need to find the optimal path. I just need to look around and
go whenever I can. For now I'll assume there's only one adjacent
square I can move through at each step along the way.

So I'll need three utility functions: a test to see if we're on S, a
function to find S on a map, and a function that moves to the next
possible location. Locations are stored as (cons row col). This
assumes that there is only one way to move on any square on the path.

In fact, it should be able to proceed all the way from S to S in a
single loop.

1. start at S
2. look N S E W to find a pipe we can traverse
3. flow to first place we can move to
4. continue to flow as directed by char at pos (counting moves)
5. goto 4 until back at S
6. return (/ moves 2)

----------------------------------- Parsation ------------------------------- |#

(defparameter *loop1*
  '("-L|F7"
    "7S-7|"
    "L|7||"
    "-L-J|"
    "L|-JF"))

(defparameter *loop2*
  '("7-F7-"
    ".FJ|7"
    "SJLL7"
    "|F--J"
    "LJ.LJ"))

(defun parse-pipe-map (los)
  "turns the list of strings describing a pipe map, return a 2D array
indexed in (row col) order"
  (let* ((width (length (first los)))
         (height (length los))
         (pipes (make-array (list height width))))

    (iter
      (for row below height)
      (iter (for col below width)
        (setf (aref pipes row col) (elt (nth row los) col))))
    pipes))

#| -------------------------------- Workness ------------------------------- |#

(defun row (loc)
  "given a location as (cons row col) return row"
  (car loc))

(defun col (loc)
  "given a location as (cons row col) return col"
  (cdr loc))

(defun char-at (map loc)
  "returns character at a given location on map"
  (aref map (row loc) (col loc)))

(defun s? (loc map)
  "returns true if the current location on the map is S (the start)"
  (char= (aref map (row loc) (col loc)) #\S))

(5a:test s?-test
  (5a:is-true (s? (cons 2 0) (parse-pipe-map *loop2*)))
  (5a:is-false (s? (cons 0 0) (parse-pipe-map *loop2*)))
  (5a:is-true (s? (cons 1 1) (parse-pipe-map *loop1*))))

(defun find-start (pipes)
  "given a pipe map, return the (row col) location of #\S"
  (iter
    (for row below (array-dimension pipes 0))
    (iter
      (for col below (array-dimension pipes 1))
      (when (s? (cons row col) pipes)
        (return-from find-start (cons row col))))))

(5a:test find-start-test
  (5a:is (equal (find-start (parse-pipe-map *loop1*)) (cons 1 1)))
  (5a:is (equal (find-start (parse-pipe-map *loop2*)) (cons 2 0))))

;; calculate the next location in any direction - if a move goes off
;; the map, return the original position unchanged
(defun move (dir map pos)
  (let ((r (row pos))
        (c (col pos)))

    (case dir
      (N (setf r (1- r)))
      (S (setf r (1+ r)))
      (E (setf c (1+ c)))
      (W (setf c (1- c)))
      (otherwise (error "What location is that? ~a ~a" dir pos)))

    (if (or (< r 0) (< c 0)
            (>= r (array-dimension map 0))
            (>= c (array-dimension map 1)))
        pos                                        ; out-of range
        (cons r c))))

(5a:test move-test
  (let ((map (parse-pipe-map *loop1*)))
    (5a:is (equal (move 'N map (cons 4 4)) (cons 3 4)))
    (5a:is (equal (move 'S map (cons 4 4)) (cons 4 4)))
    (5a:is (equal (move 'W map (cons 0 0)) (cons 0 0)))
    (5a:is (equal (move 'E map (cons 0 4)) (cons 0 4)))
    (5a:is (equal (move 'W map (cons 3 3)) (cons 3 2)))))

(defun flow (map pos heading moves)
  "given a location on a pipe map and a heading (N E S W) return the
 number of moves it will take to get back to the start (S) - begin to
 flow from first move after S! Function recurses until S"
  (let ((next (char-at map pos)))       ; next move

    ;; which way do I flow next?
    (case next
      (#\S (return-from flow moves))    ; we're at the end!

      ;; straight pipe - maintain heading
      ((or #\| #\-) nil)

      ;; bent pipe - make a turn
      (#\L
       (if (equal heading 'S)
           (setf heading 'E)            ; turn right
           (setf heading 'N)))          ; turn left

      (#\J
       (if (equal heading 'S)
           (setf heading 'W)            ; turn right
           (setf heading 'N)))          ; turn left

      (#\7
       (if (equal heading 'N)           ; were we going north?
           (setf heading 'W)            ; then make a left
           (setf heading 'S)))          ; otherwise a right

      (#\F
       (if (equal heading 'N)
           (setf heading 'E)            ; turn right
           (setf heading 'S)))          ; turn left

      (otherwise (error "I'm lost! ~a" heading)))

    ;; not at end, so keep going along (new?) heading
    (flow map (move heading map pos) heading (incf moves))))

(5a:test flow-test
  (5a:is (flow (parse-pipe-map *loop1*) (cons 1 2) 'W 1) 8)
  (5a:is (flow (parse-pipe-map *loop2*) (cons 2 1) 'E 1) 16))

(defun Day10-1 (los)
  "given a list of strings representing a map of pipes, find the starting
point and return the most distant point on the path (we do this by
flowing through the entire path and returning 1/2 that distance)"
  (let* ((map (parse-pipe-map los)) ; our pipe map 2D array (from input)
         (start (find-start map))   ; the location of the S in the map

         ;; points at the four cardinal directions
         (np (move 'N map start))   ; position north of start
         (ep (move 'E map start))   ; east
         (sp (move 'S map start))   ; south
         (wp (move 'W map start))   ; west of start

         ;; characters at those points
         (nc (char-at map np))      ; character north of start
         (ec (char-at map ep))      ; character east of start
         (sc (char-at map sp))      ; south
         (wc (char-at map wp)))     ; west

    (/
     ;; figure out where the route begins - pick first place we can
     ;; flow to, going clockwise from north
     (cond ((or (char= nc #\|) (char= nc #\7) (char= nc #\F))
            (flow map np 'N 1))    ; and flow in that direction

           ((or (char= ec #\-) (char= ec #\7) (char= nc #\J))
            (flow map ep 'E 1))

           ((or (char= sc #\|) (char= sc #\J) (char= sc #\L))
            (flow map sp 'S 1))

           ((or (char= wc #\-) (char= wc #\F) (char= wc #\L))
            (flow map wp 'W 1))

           (t (error "Lost AGAIN!")))
     2)))                               ; halfway point is most distant

(5a:test Day10-1-test
  (5a:is (= (day10-1 *loop1*) 4))
  (5a:is (= (day10-1 *loop2*) 8)))

#| -----------------------------------------------------------------------------
--- Part Two

"Figure out whether you have time to search for the nest by
calculating the area within the loop. How many tiles are enclosed by
the loop?"

LEO'S NOTES: I'm told there is a trick that simplifies this. Hmmm.

------------------------------------------------------------------------------|#

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 10 Part 1 is ~a"
              (day10-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 10 Part 2 is ~a"
;;	      (day10-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------
