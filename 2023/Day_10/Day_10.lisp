;;;; Day10.lisp
;;;; 2023 AOC Day 10 solution
;;;; Leo Laporte
;;;; 7- Jan 2024

;; --------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; --------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :iterate))

(defpackage :day10
  (:use #:cl #:iterate)   ; use ITERATE instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))        ; testing

(in-package :day10)
(setf 5a:*run-test-when-defined* t)    ; test as we go
(declaim (optimize (debug 3)))         ; max debugging info
;; (declaim (optimize (speed 3))       ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_10/input.txt"
  "Downloaded from the AoC problem set")

#| --------------------------------------------------------------------------
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

----------------------------------- Parsation -------------------------- |#

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

(defparameter map1 (parse-pipe-map *loop1*))
(defparameter map2 (parse-pipe-map *loop2*))
(defparameter map3 (parse-pipe-map (uiop:read-file-lines *data-file*)))

#| -------------------------------- Workness ----------------------------- |#

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

(defun find-start (map)
  "given a pipe map, return the (row col) location of #\S"
  (iter
    (for row below (array-dimension map 0))
    (iter
      (for col below (array-dimension map 1))
      (when (s? (cons row col) map)
        (return-from find-start (cons row col))))))

(5a:test find-start-test
  (5a:is (equal (find-start (parse-pipe-map *loop1*)) (cons 1 1)))
  (5a:is (equal (find-start (parse-pipe-map *loop2*)) (cons 2 0))))

;; calculate the next location in any direction - if a move goes off
;; the map, return the original position unchanged
(defun move (dir map loc)
  (let ((r (row loc))
        (c (col loc)))

    (case dir
      (N (setf r (1- r)))
      (S (setf r (1+ r)))
      (E (setf c (1+ c)))
      (W (setf c (1- c)))
      (otherwise (error "What location is that? ~a ~a" dir loc)))

    (if (or (< r 0) (< c 0)
            (>= r (array-dimension map 0))
            (>= c (array-dimension map 1)))
        loc                             ; out-of range
        (cons r c))))

(5a:test move-test
  (let ((map (parse-pipe-map *loop1*)))
    (5a:is (equal (move 'S map (cons 4 4)) (cons 4 4))) ; out of range
    (5a:is (equal (move 'W map (cons 0 0)) (cons 0 0))) ; out of range
    (5a:is (equal (move 'E map (cons 0 4)) (cons 0 4))) ; out of range
    (5a:is (equal (move 'N map (cons 2 2)) (cons 1 2)))
    (5a:is (equal (move 'S map (cons 2 2)) (cons 3 2)))
    (5a:is (equal (move 'E map (cons 2 2)) (cons 2 3)))
    (5a:is (equal (move 'W map (cons 2 2)) (cons 2 1)))))

(defun find-first-move (map)
  "given a pipe map, find the first move from the start, return two
values: the location and its cardinal direction ('N 'E 'W' 'S)"
  (let* ((start (find-start map))    ; the location of the S in the map

         ;; points at the four cardinal directions
         (np (move 'N map start))        ; position north of start
         (ep (move 'E map start))        ; east
         (sp (move 'S map start))        ; south
         (wp (move 'W map start))        ; west of start

         ;; characters at those points
         (nc (char-at map np))           ; character north of start
         (ec (char-at map ep))           ; east of start
         (sc (char-at map sp))           ; south
         (wc (char-at map wp)))          ; west

    ;; figure out where the route begins - pick first place we can
    ;; flow to, going clockwise from north
    (cond ((or (char= nc #\|) (char= nc #\7) (char= nc #\F))
           (values np 'N))              ; start here

          ((or (char= ec #\-) (char= ec #\7) (char= nc #\J))
           (values ep 'E))

          ((or (char= sc #\|) (char= sc #\J) (char= sc #\L))
           (values sp 'S))

          ((or (char= wc #\-) (char= wc #\F) (char= wc #\L))
           (values wp 'W))

          (t (error "Lost AGAIN!")))))

(defun get-flow-path (map)
  "given a pipe map follow the map to return a list of points that flow
back to the start (S). Iterative version."

  (multiple-value-bind (loc heading) (find-first-move map) ; get first move
    (do* ((p loc (move heading map p))        ; next point on the map
          (d (char-at map p) (char-at map p)) ; pipe at that point
          (path (list p) (push p path)))      ; path so far

         ((char= d #\S) path) ; return the path of flow

      ;; loop body: look for turns and change heading if we find one
      (case d
        (#\L
         (if (equal heading 'S)        ; turn depends on heading
             (setf heading 'E)         ; coming from north? turn right
             (setf heading 'N)))       ; coming from east? turn left

        (#\J
         (if (equal heading 'S)
             (setf heading 'W)          ; turn right
             (setf heading 'N)))        ; turn left

        (#\7
         (if (equal heading 'N)
             (setf heading 'W)          ; left
             (setf heading 'S)))        ; right

        (#\F
         (if (equal heading 'N)
             (setf heading 'E)          ; right
             (setf heading 'S)))))))

(5a:test flow-test
  (5a:is (length (flow (parse-pipe-map *loop1*))) 8)
  (5a:is (length (flow (parse-pipe-map *loop2*))) 16))

(defun Day10-1 (los)
  "given a list of strings representing a map of pipes return the most
distant point on the path (we do this by flowing through the entire
path and returning 1/2 that distance)"
  (let ((map (parse-pipe-map los))) ; our pipe map 2D array (from input)

    (/
     (length (get-flow-path map))   ; get length of flow path
     2)))     ; divide length of path by 2 to find most distant pt

(5a:test Day10-1-test
  (5a:is (= (day10-1 *loop1*) 4))
  (5a:is (= (day10-1 *loop2*) 8)))

#| -----------------------------------------------------------------------
--- Part Two

"Figure out whether you have time to search for the nest by
calculating the area within the loop. How many tiles are enclosed by
the loop?"

LEO'S NOTES:

In the simplest case, if the path were a square the inner part of the
loop would be preceded by a | and continue until the next |. We can
ignore horizontal lines. It gets more complicated as the path winds
around but the basic idea should hold, right? For example, when I see
| I'm now inside the loop until the next |. Let's play with that and
see. Oh and maybe J and L are also counted as | and F and 7 are -. Is
that right? This is going to require some experimentation.

1. modify the flow function in part 1 to keep track of route

2. Figure out which pipe type is under the S and replace the S with
the pipe.

3. replace - F and 7 with >, replace | J and L with ^ (this is where I
might have to experiment)

4. for aesthetic purposes replace ALL other pipes with "." so I can
verify my method with a visual inspection of the pipe map (cut down
the noise)

5. Go row by row left to right from top, replacing . with 0 whenever
inside an odd number of ^

6. Count the 0s for our answer.

-------------------------------------------------------------------------|#

;; New examples
(defparameter *loop3*
  '("..........."
    ".S-------7."
    ".|F-----7|."
    ".||.....||."
    ".||.....||."
    ".|L-7.F-J|."
    ".|..|.|..|."
    ".L--J.L--J."
    "..........."))

(defparameter *loop4*
  '(".F----7F7F7F7F-7...."
    ".|F--7||||||||FJ...."
    ".||.FJ||||||||L7...."
    "FJL7L7LJLJ||LJ.L-7.."
    "L--J.L7...LJS7F-7L7."
    "....F-J..F7FJ|L7L7L7"
    "....L7.F7||L7|.L7L7|"
    ".....|FJLJ|FJ|F7|.LJ"
    "....FJL-7.||.||||..."
    "....L---J.LJ.LJLJ..."))

(defparameter *loop5*
  '("FF7FSF7F7F7F7F7F---7"
    "L|LJ||||||||||||F--J"
    "FL-7LJLJ||||||LJL-77"
    "F--JF--7||LJLJ7F7FJ-"
    "L---JF-JLJ.||-FJLJJ7"
    "|F|F-JF---7F7-L7L|7|"
    "|FFJF7L7F-JF7|JL---7"
    "7-L-JL7||F7|L7F-7F7|"
    "L.L7LFJ|||||FJL7||LJ"
    "L7JLJL-JLJLJL--JLJ.L"))

(defparameter map3 (parse-pipe-map *loop3*))
(defparameter map4 (parse-pipe-map *loop4*))
(defparameter map5 (parse-pipe-map *loop5*))

(defun start-pipe (map)
  "given a pipe map, return the pipe hidden under S"
  (let* ((start (find-start map))

         ;; points at the four cardinal directions
         (np (move 'N map start))       ; position north of start
         (ep (move 'E map start))       ; east
         (sp (move 'S map start))       ; south
         (wp (move 'W map start))       ; west of start

         ;; characters at those points
         (nc (char-at map np))          ; character north of start
         (ec (char-at map ep))          ; east of start
         (sc (char-at map sp))          ; south
         (wc (char-at map wp)))         ; west

    ;; what's under the S?
    (cond
      ;; pipe connects N...
      ((or (char= nc #\|) (char= nc #\F) (char= nc #\7))
       (cond
         ;; ...to E
         ((or (char= ec #\-)
              (char= ec #\7)
              (char= ec #\J)) #\L)
         ;; ...to S
         ((or (char= sc #\|)
              (char= sc #\J)
              (char= sc #\L)) #\|)
         ;; ...to W
         ((or (char= wc #\-)
              (char= wc #\F)
              (char= wc #\L)) #\J)))

      ;; pipe connects E
      ((or (char= ec #\-) (char= ec #\J) (char= ec #\7))
       (cond
         ;; to S
         ((or (char= sc #\|)
              (char= sc #\J)
              (char= sc #\L)) #\F)

         ;; to W
         ((or (char= wc #\-)
              (char= wc #\L)
              (char= wc #\F)) #\-)))

      ;; pipe connects S
      ((or (char= sc #\|) (char= sc #\L) (char= sc #\J))
       ;; to W (that's all that's left)
       #\7))))

(5a:test start-pipe-test
  (5a:is (equal (start-pipe map1) #\F))
  (5a:is (equal (start-pipe map2) #\F))
  (5a:is (equal (start-pipe map3) #\F))
  (5a:is (equal (start-pipe map4) #\F))
  (5a:is (equal (start-pipe map5) #\7)))

(defun prettify-map (map)
  "given a pipe map, replace the flow path with > and ^ characters,
including the starting square, then make all the other positions a ."
  (let ((path (get-flow-path map))     ; the path of the flow in map
        (start-loc (find-start map)))

    ;; replace S with pipe type
    (setf (aref map (row start-loc) (col start-loc)) (start-pipe map))

    ;; replace path piping with flow characters > and ^
    (dolist (loc path)
      (let ((c (char-at map loc)))
        (if (or (char= c #\-) (char= c #\7) (char= c #\F)) ; if it's - 7 or F
            (setf (aref map (row loc) (col loc)) #\>) ; replace with >
            (setf (aref map (row loc) (col loc)) #\^)))) ; otherwise with ^

    ;; replace every non-path location with .
    (iter (for row below (array-dimension map 0))
      (iter (for col below (array-dimension map 1))
        (let ((c (aref map row col)))
          (when (and (not (char= c #\^)) (not (char= c #\>))) ; not a flow char
            (setf (aref map row col) #\.)))))  ; so make it a .

    map))

(defun mark-inside (map)
  "given a prettified pipe-flow-map, replace all the points inside the
path with #\0, return modified map"
  (let ((wall-count 0))

    (iter (for row below (array-dimension map 0))
      (setf wall-count 0)    ; reset wall count for each row

      (iter (for col below (array-dimension map 1))  ; walk down the row
        (let ((c (aref map row col)))                ; looking at each pipe type

          ;; check three conditions
          (cond ((char= c #\^) (incf wall-count))  ; wall char, increment count

                ((and (char= c #\.) (oddp wall-count)) ; inside loop!
                 (setf (aref map row col) #\0))

                (t nil)))))                            ; otherwise do nothing
    map))

(defun Day10-2 (los)
  "given a list of strings representing a pipe map, return the number
 of points surrounded entirely by the flow path"
  (let* ((pretty-map (mark-inside (prettify-map (parse-pipe-map los))))
         (inside-count 0))

    (iter (for col below (array-dimension pretty-map 0))
      (iter (for row below (array-dimension pretty-map 1))
        (when (char= (char-at pretty-map (cons row col)) #\0)
          (incf inside-count))))

    inside-count))

(5a:test Day10-2-test
  (5a:is (= (Day10-2 *loop3*) 4))
  (5a:is (= (Day10-2 *loop4*) 8))
  (5a:is (= (Day10-2 *loop5*) 10)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 10 Part 1 is ~a"
              (day10-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 10 Part 2 is ~a"
;;	      (day10-2 (uiop:read-file-lines *data-file*))))

;; ------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ------------------------------------------------------------------------

;; The answer to AOC 2023 Day 10 Part 1 is 7030
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.002301 seconds of total run time (0.002190 user, 0.000111 system)
;; 100.00% CPU
;; 487,472 bytes consed
