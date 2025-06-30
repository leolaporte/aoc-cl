;;;; Day15.lisp
;;;; 2024 AOC Day 15 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 2025-06-24
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day15
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day15)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/Day_15/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 15: Warehouse Woes ---
--- Part One ---

"The GPS coordinate of a box is equal to 100 times its distance from
the top edge of the map plus its distance from the left edge of the
map. (This process does not stop at wall tiles; measure all the way to
the edges of the map.)

what is the sum of all boxes' GPS coordinates?"

LEO'S NOTES:

Job one is to move all the boxes around.

Let me think about how I want to process the input.  The map is a
simple 2D grid. But it looks like there might be a lot of moving
objects around, so instead of using a 2D array I'll represent the map
as a one-dimensional vector for speed.

The moves are just a series of characters. Keep them as a list of char.

So that's parsing done. (well not exactly - I should have heeded
Eric's warning about the input file. The examples all had the move
instructions in a single line. The input did not. So all my examples
worked, while the input did not because I was not executing all the
moves. After tearing my hair out for a bit I asked Claude Code and it
pinpointed the problem instantly. The fix was simple as outlined
below.)

I'll have to return multiple values, including the "width" of the grid
so I can convert x y positions into vector indices. (+ x (* y width))

I'll need a move routine that takes the vector map, robot position,
and a move command, then returns the modified vector map and new robot
position. It will have to encapsulate the rules about pushing rocks
and walls. Pushing rocks will also result in an update of the map
vector.

Can I make move generic enough to work for both the robots and the
rocks? If there's a rock in the way I have to move it in the same
direction. If there's a wall there, neither the robot or the rock
move, if there's a space there the robot and the rock(s) move one move
in that direction. If there's a rock in the next space, however, then
I repeat the rock move.

OK so basically I'm looking for the first open space in the direction
of the move. If there is one, move the robot (and any intevening
rocks) one move in that direction. If there isn't, don't move.

I'll do this with three functions:

1. NEXT-POS a simple utility function which returns the next index
into the array for a move in a given direction. (handling up, down,
left, and right)

2. NEXT-POSITIONS which will return a list of points including the
robot, intevening rocks, and the empty space. If the robot can't move
the list will just be one point: its current position. If the robot is
next to an empty space the list will be two points, the current
position and the empty spot. A move will consist of swapping those
points. If there are intervening rocks, the list will include the
positions of the robot, rocks, and empty space.

3. SLIDE which takes the list of positions from NEXT-POSITIONS and
adjusts the map and robot position accordingly. It will take the last
item in the list, the empty space, put it in front, then slide the
other items into the gap. i.e. '(@ O O .) -> '(. @ O O) or '(@ .) ->
'(. @)

After SLIDEing for every move instruction I'll have the final map and
can do the simple GPS calculations.

----------------------------------------------------------------------------|#

(defparameter *example*
  '("########"
    "#..O.O.#"
    "##@.O..#"
    "#...O..#"
    "#.#.O..#"
    "#...O..#"
    "#......#"
    "########"
    ""
    "<^^>>>vv<v>>v<<"))

(defparameter *big-example*
  '("##########"
    "#..O..O.O#"
    "#......O.#"
    "#.OO..O.O#"
    "#..O@..O.#"
    "#O#..O...#"
    "#O..O..O.#"
    "#.OO.O.OO#"
    "#....O...#"
    "##########"
    ""
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<>
<<v<<<v^vv^v>^vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v
<<<<v<^v>^<^^>>>^<v<v><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^
^>v^<^v>v<>>v^v^<v>v^^<^^vv<<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^
<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^^><^><>>><>^^<<^^v>>><^<v>^<
vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><^>><>^v<><^vvv<^^<><v
<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^>^>>^v>vv>^<<^
v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^<><^^>^
^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>
^<><<v>v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v
><v^<vv<>v^<<^"))

(defparameter *phaul*
  '("######"
    "#....#"
    "#.OO@#"
    "#.OO.#"
    "#....#"
    "######"
    ""
    "<<^<v")
  "donated by Paul Holder for testing")

(defparameter *sokoban*
  '("....#####............."
    "....#...#............."
    "....#O..#............."
    "..###..O###..........."
    "..#..O..O.#..........."
    "###.#.###.#.....######"
    "#...#.###.#######....#"
    "#.O..O...............#"
    "#####.####.#@####....#"
    "....#......###..######"
    "....########.........."
    ""
    "^<<<^^^<<^<<v<<vvv>>>>>>>>>>>>>>>>^>v<<<<<<<<<<<<<<<<<^<<v>>>>>>>
>>>>>>>>>>v>^<^>v<<<<<<<<<<<<<<<<^^^>>vv^^<<vvv>>>>>>>>>>>>>>>><<<<<<<<<<^^^
<<<^<vvv^^<<vvv>>>>>>>>>>>>>>>v>^<<<<<<<<<<<<<<^^^^>^^<vvvvv^^<<vvv>>>>>>>>>
>>>>>^>v<v>^<<<<<<<<<^^^<<<^^^>vv<<vvvv>>>>^^^<<<^<vvv^^<<vvv>>>>>>>>>>>>>>>")
  "an actual sokoban puzzle from Reddit")

(defparameter *wall* #\#)
(defparameter *box* #\O)
(defparameter *empty* #\.)
(defparameter *robot* #\@)
(defparameter *up* #\^)
(defparameter *down* #\v)
(defparameter *left* #\<)
(defparameter *right* #\>)

(defun parse-warehouse (los)
  "given a list of text strings containing a map of box positions and a
long string with move instructions <>^v return a 1D vector for the
map, the width of the 2D grid (for x y to index conversions), the
starting position of the robot, and the move instructions as a list of
chars"
  (let* ((map-strings (iter (for l in los)   ; get all the map strings first
                        (until (equal l "")) ; up to the blank line
                        (collecting l)))
         (height (length map-strings))
         (width (length (first map-strings)))
         (map (make-array (* height width)
                          :element-type 'base-char
                          :fill-pointer 0)) ; map vector
         (robot-posn 0) ; the position of the robot

         ;; The move instructions are the remaining lines of the
         ;; input. Turn them into a list of moves as #\^ #\v etc.
         ;; also remove any stray newlines (this was a bug in my first
         ;; try, all the examples had the moves in a single line,
         ;; while the input did not. At first I was only getting part
         ;; of the move instructions. Claude Code found the problem
         ;; and let me know I was missing instructions. Sweet.)
         (move-instructions
           (remove #\Newline ; strip those stray CR LF etc.
                   (coerce (apply #'concatenate 'string
                                  ;; gotta get 'em ALL
                                  (nthcdr (1+ (length map-strings)) los))
                           'list))))

    ;; Make a vector of the map
    (iter (for row below height)
      (iter (for col below width)
        (let ((ch (char (nth row map-strings) col)))
          (if (char= ch *robot*)
              (progn
                (setf robot-posn (+ col (* row width))) ; save the robot's posn
                (vector-push *empty* map))              ; then replace it
              (vector-push ch map)))))

    (values map width robot-posn move-instructions)))

(defun next-pos (dir pos width)
  "returns the next position in the vector based on the current POS and DIR - does no range checking"
  (ecase dir
    (#\^  (- pos width))
    (#\v  (+ pos width))
    (#\>  (1+ pos))
    (#\<  (1- pos))))

(5a:test next-pos-t
  (5a:is (= 1 (next-pos *right* 0 10)))
  (5a:is (= 11 (next-pos *down* 1 10)))
  (5a:is (= 1 (next-pos *up* 11 10)))
  (5a:is (= 4 (next-pos *left* 5 10))))

(defun next-positions (map dir pos width)
  "given a MAP vector and a DIR move along the MAP until we get to an
 empty space, return a list of all positions touched, including start"
  (let ((points (list pos)) ; start with the current robot position
        (start pos))        ; keep track of it in case we can't move
    (loop
      (let ((next (next-pos dir pos width)))
        (ecase (aref map next) ; only three things we can hit
          (#\# (return-from next-positions (list start))) ; hit a wall, no move
          (#\. (return-from next-positions ; found a hole, move everything
                 (reverse (push next points))))
          (#\O (setf pos next))) ; keep seeking wall or open point
        (push next points)))))

(5a:test next-positions-test
  (multiple-value-bind (map width pos move-instructions)
      (parse-warehouse *big-example*)
    (declare (ignore move-instructions))
    (5a:is (equal '(44 34)  (next-positions map *up*  pos width)))
    (5a:is (equal '(44 45) (next-positions map *right*  pos width)))
    (5a:is (equal '(44 43 42) (next-positions map *left*  pos width)))
    (5a:is (equal '(44 54) (next-positions map *down*  pos width)))))

(defun slide (posns map)
  "given a list of positions POSNS on a map, with the first position
being the robot and the last an open space, move the characters at
each POSNS to the next position, putting the last position in
front. Return the new robot position and resulting map.... e.g. '(a b
c d) becomes (d a b c)"
  (let ((len (length posns))
        (new-map (copy-seq map))) ; make a copy to modify
    ;; no move
    (when (= 1 len) (return-from slide (values (first posns) new-map)))

    ;; move last posn to front
    (let ((new-posns (cons (nth (1- len) posns) (butlast posns))))
      ;; shift map
      (iter (for p in posns) (for np in new-posns)
        (setf (aref new-map p) (aref map np))) ; modify map

      ;; return new robot posn and modified map
      (values (second posns) new-map))))

(5a:test slide-test
  (5a:is (equalp (slide '(1) #(a b c d)) (values 1 #(a b c d)))) ;no move
  (5a:is (equalp (slide '(0 1) #(a b c d)) (values 1 #(b a c d)))) ;1 right
  (5a:is (equalp (slide '(0 1 2 3) #(a b c d)) (values 1 #(d a b c))))
  (5a:is (equalp (slide '(0 1 2) #(a b c d)) (values 1 #(c a b d))))
  (5a:is (equalp (slide '(6 2) #(a b c d e f g h))
                 (values 2 #(a b g d e f c h))))
  (5a:is (equalp (slide '(10 6 2) #(a b c d
                                    e f g h
                                    i j k l))
                 (values 6 #(a b g d
                             e f k h
                             i j c l))))
  (5a:is (equalp (slide '(1 5 9) #(a b c d
                                   e f g h
                                   i j k l))
                 (values 5 #(a j c d
                             e b g h
                             i f k l)))))

(defun calculate-gps (map width)
  "given a vector and the row width of the 2D grid it represents,
 return the sum of all the GPS coordinates of *BOX* in the map"
  ;; collect the vector positions of all the boxes
  (let ((boxes (iter (for index below (length map))
                 (when (char= (aref map index) *box*)
                   (collect index)))))

    ;; The GPS coordinate of a box is equal to 100 times its
    ;; row plus the col
    (iter (for box in boxes)
      (multiple-value-bind (row col) (floor box width)
        (summing (+ (* 100 row) col))))))

(defun print-map (pos dir map width)
  "print the move and resulting map (for debugging)"
  (format t "~%~a~%" dir)
  (iter (for ch below (length map))
    (when (= 0 (mod ch width))
      (format t "~&"))
    (if (= ch pos)
        (format t "@")
        (format t "~a" (aref map ch)))))

(defun day15-1 (los)
  "given a warehouse map and a list of moves return the GPS positions of
all the *BOX*es after executing all the moves"
  (multiple-value-bind (map width pos moves)
      (parse-warehouse los)

    (dolist (dir moves) ; go through moves one by one
      (multiple-value-bind (new-pos new-map)
          (slide (next-positions map dir pos width) map) ; move
        (setf map new-map)    ; update map
        (setf pos new-pos)))  ; update robot posn

    (calculate-gps map width)))

(5a:test day15-1-test
  (5a:is (= 2028 (day15-1 *example*)))
  (5a:is (= 10092 (day15-1 *big-example*)))
  (5a:is (= 1208 (day15-1 *phaul*)))
  (5a:is (= 4317 (day15-1 *sokoban*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

Hoo boy.

---------------------------------------------------------------------------- |#

(defun day15-2 (los)
  los
  )

(5a:test day15-2-test
  (5a:is (= 9021 (day15-2 *big-example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 15 Part 1 is ~a"
              (day15-1 (uiop:read-file-lines *data-file*))))


;; (time (format t "The answer to AOC 2024 Day 15 Part 2 is ~a"
;;	      (day15-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
