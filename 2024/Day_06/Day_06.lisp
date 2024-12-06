;;;; Day###.lisp
;;;; 2024 AOC Day ### solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 6 Dec 2024 0900 PST
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day06
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day06)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/Day_06/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 6: Guard Gallivant ---
--- Part One ---

"If there is something directly in front of you, turn right 90 degrees.
Otherwise, take a step forward.

How many distinct positions will the guard visit before leaving the
mapped area?" (Including starting position)

LEO'S NOTES:

This seems fairly simple (especially if you've done AoC before). I'll
make a sparse hash of the floor map then process the moves until the
guard is no longer on the map (hash returns NIL). Oops - that means it
can't be a sparse hashmap because I need to get a nil when off the
map. So I'll map every point.

Does the guard always start facing N? She does in the example, and,
checking input, yes she does in the problem set.

One more thing... I have to count DISTINCT positions - so I'm not
counting moves, just unique positions.

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("....#....."
    ".........#"
    ".........."
    "..#......."
    ".......#.."
    ".........."
    ".#..^....."
    "........#."
    "#........."
    "......#..."))

(defstruct (guard)
  (posn (cons 0 0) :type list)
  (heading 'N :type symbol))

(defun parse-map (los)
  "given a list of strings turn it into a hash-map, return the
 hash-map plus the guard in her starting position and heading"
  (let ((labmap (make-hash-table :test 'equal))
        (theguard (make-guard))
        (width (length (first los)))
        (height (length los)))

    (iter (for y below height)
      (iter (for x below width)
        (let ((spot (elt (nth y los) x)))
          (cond ((equal spot #\^) ; it's the guard
                 (setf (guard-posn theguard) (cons x y))
                 (setf (guard-heading theguard) 'N)
                 (setf (gethash (cons x y) labmap) #\.)) ; just an empty pos

                (t (setf (gethash (cons x y) labmap) spot))))))

    (values theguard labmap)))

(defun ph (hash)
  "a little utility to display a hash table"
  (maphash (lambda (key value)
             (format t "~a => ~a~%" key value)) hash))

(defun move (posn heading)
  "given a point and a heading, returns the position after moving
 one square in that heading"
  (let* ((headings (list
                    (cons 'N '(0 -1))
                    (cons 'E '(1 0))
                    (cons 'S '(0 1))
                    (cons 'W '(-1 0))))
         (dir (cdr (assoc heading headings))))
    (cons (+ (car posn) (car dir)) (+ (cdr posn) (cadr dir)))))

(defun move-guard (grd labmap)
  "given a guard and a map of the lab move the guard one move, return
 the new guard struct or NIL if off the map"
  (let* ((heading (guard-heading grd))
         (next-posn (move (guard-posn grd) heading)) ; next position
         (next-char (gethash next-posn labmap)))     ; char @ posn
    (cond ((null next-char) nil)  ; off map, done

          ((equal next-char #\.) ; no obstacle, next posn same heading
           (setf (guard-posn grd) next-posn)
           grd)

          ((equal next-char #\#)                       ; obstacle
           (setf (guard-heading grd)              ; turn right
                 (cond ((equal heading 'N) 'E)
                       ((equal heading 'E) 'S)
                       ((equal heading 'S) 'W)
                       ((equal heading 'W) 'N)))
           (move-guard grd labmap)))))           ; try moving again

(5a:test move-guard-test
  (multiple-value-bind (g m) (parse-map *example*)
    (5a:is (equalp (move-guard g m) (make-guard :posn (cons 4 5)
                                                :heading 'N)))
    (5a:is (equalp (move-guard (make-guard :posn (cons 4 1)) m)
                   (make-guard :posn (cons 5 1)
                               :heading 'E)))
    (5a:is (equalp (move-guard (make-guard :posn (cons 8 1) :heading 'E) m)
                   (make-guard :posn (cons 8 2)
                               :heading 'S)))
    (5a:is (equalp (move-guard (make-guard :posn (cons 2 6) :heading 'W) m)
                   (make-guard :posn (cons 2 5)
                               :heading 'N)))
    (5a:is-false  (move-guard (make-guard :posn (cons 3 0) :heading 'N) m))))

(defun Day_06-1 (los)
  (multiple-value-bind (g m) (parse-map los)
    (do ((grd g (move-guard grd m))          ; moves every time through loop
         (positions (list (guard-posn g))
                    (push (guard-posn grd) positions))) ; add seen positions

        ((null grd) ; moved off grid
         (length (remove-duplicates positions :test 'equalp)))))) ; return

(5a:test Day_06-1-test
  (5a:is (= 41 (Day_06-1 *example*))))


#| ----------------------------------------------------------------------------
--- Part Two ---

"You need to get the guard stuck in a loop by adding a single new
obstruction. How many different positions could you choose for this
obstruction?"

LEO'S NOTES: OK now we're getting interesting.

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 06 Part 1 is ~a"
              (day_06-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2024 Day 06 Part 2 is ~a"
;;	      (day_06-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
