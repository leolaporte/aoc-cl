;;;; Day15.lisp
;;;; 2022 AOC Day 15 solution
;;;; Leo Laporte, 15 Dec 2022

;; ---------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ---------------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day15
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day15)

(setf fiveam:*run-test-when-defined* t) ; run tests when compiled for iteration speed
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day15/input.txt")  ; supplied data from AoC

#| ---------------------------- Day 15: Beacon Exclusion Zone ----------------------------

--- Part One ---

"Sensors and beacons always exist at integer coordinates. Each sensor knows its own
position and can determine the position of a beacon precisely; however, sensors can only
lock on to the one beacon closest to the sensor as measured by the Manhattan distance.
(There is never a tie where two beacons are the same distance to a sensor.)

None of the detected beacons seem to be producing the distress signal, so you'll need to
work out where the distress beacon is by working out where it isn't. For now, keep things
simple by counting the positions where a beacon cannot possibly be along just a single row.

Consult the report from the sensors you just deployed. In the row where y=2000000, how
many positions cannot contain a beacon?"

NOTES: So I'm guessing the size of this grid is going to be massive so I don't want to
calculate more than I have to. But it turns out the problem is way simpler than it seems.

For each scanner I know its range (the dist to the associated beacon). I can calculate
how much it sees of the provided y coordinate (I'll call it the line of interest (loi).
From there I can generate a list of visible points (no need to do points, even, just
x-coordinates) and count them.

What's the geometry? If the distance to the loi is exactly equal to the scanner's
range that scanner can see exactly one point on the loi: (scanner x loi-y), for every
row closer to the loi, add a point left and right of the origin.

Those are the visible points. Each scanner will have a set of visible points.
Union the sets then count the resulting set to get the answer. Done with part 1.

Again I'll represent points as (cons x y) and today it's in col row order.

UPDATE: So unfortunately according the the example data if a beacon is already
in the loi that point does not count. I've been ignoring beacons, but I guess
I can't. I need to count beacons in the loi and subtract that number from
the total points I can see. Erg.
--------------------------------------------------------------------------------------- |#

(defparameter *sample-data*
  '("Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3")
  "provided AoC example")

(defparameter *sample-loi* 10
  "line of interest for the sample scans")

(defparameter *digits* (re:create-scanner "(\\d)+")
  "the regex to find digits in a string")

(defstruct scnr
  "a scanner, with location, range, distance to line of interest and
points on loi that it can see"
  (loc (cons 0 0) :type cons)      ; x y position stored as (cons x y)
  (beacon (cons 0 0) :type cons)   ; x y position of beacon
  (range 0 :type integer)          ; the scanner's range
  (dist-to-loi 0 :type integer)    ; the scanner's distance to the line of interest
  (visible '() :type list))        ; x positions visible on the loi

(defun col (p)
  "a mnemonic for the column data in a point stored as (cons x y)"
  (car p))

(defun row (p)
  "a mnemonic for the row data in a point stored as (cons x y)"
  (cdr p))

(defun dist (x y)
  "return the manhattan distance between two points, x y"
  (+ (abs (- (col x) (col y))) (abs (- (row x) (row y)))))

(defun make-scanner-list (los)
  "given a list of strings, each describing a scanner and beacon, return a list
of scanner structures, each populated with scanner and beacon locations and range"
  (flet ((parse-scanner-string (str)
	   (let* ((s (make-scnr))          ; create an empty scanner
		  (digits (re:all-matches-as-strings *digits* str))   ; extract the digits
		  (s-pos (cons (parse-integer (first digits))         ; first two are
			       (parse-integer (second digits))))      ; scanner position
		  (b-pos (cons (parse-integer (third digits))         ; second pair are
			       (parse-integer (fourth digits)))))     ; beacon position

	     (setf (scnr-loc s) s-pos)     ; set the scanner's location
	     (setf (scnr-beacon s) b-pos)  ; the beacon location
	     (setf (scnr-range s)          ; set the scanner range to the
		   (dist s-pos b-pos))     ; Manhattan distance from scanner to beacon
	     s)))

    (mapcar #'parse-scanner-string los)))

;; OK I've got a list of scanner structures. Now I need to know what each scanner
;; can see along the line of interest. Start by calculating each scanner's range.
(defun set-scanners-dist (scanners loi)
  "given a list of scanners and a line of interest, populate each scanner with
its distance to loi."
  (flet ((set-dist-to-loi (s)
	   (setf (scnr-dist-to-loi s)
		 (dist (scnr-loc s)
		       (cons (col (scnr-loc s)) loi)))   ; manhattan distance to loi
	   s))
    (mapcar #'set-dist-to-loi scanners)))

;; No need to keep track of scanners that can't see that far. So prune 'em.
(defun prune-scanners (scanners)
  "removes any scanners that can't see the line of interest"
  (remove-if #'(lambda (s) (> (scnr-dist-to-loi s) (scnr-range s))) scanners))

;; Now make a list of all the points on the line that each scanner can see.
(defun set-visible-points (scanner)
  "returns a scanner with the visible field populated with a list of the points on the
loi it can see"
  (let ((num (- (scnr-range scanner)
		(scnr-dist-to-loi scanner)))            ; how many visible on each side
	(start (col (scnr-loc scanner))))               ; column of first visible point
    (setf (scnr-visible scanner)                        ; set visible points
	  (loop for x from (- start num) upto (+ start num)
		collect x))                             ; working from left to right
    scanner))                                           ; return updated scanner

(defun set-scanners-visible (scanners)
  "given a list of scanners, sets the list of visible points for each and returns
the new list of scanners"
  (mapcar #'set-visible-points scanners))

;; Oh but the problem has thrown me a curve. I ALSO have to eliminate any points
;; which already have a beacon on it. D'oh!
(defun get-beacon-points (scanners loi)
  "return a list of the x-coordinates of beacons in the scanner list that are on
the line of interest"
  (loop for s in scanners collecting
			  (when (= (row (scnr-beacon s)) loi)
			    (col (scnr-beacon s)))))

(defun day15-1 (los loi)
  "given a list of strings describing a number of scanner-beacon pairs, and the
y-coordinate of a line of interest, return the number of points covered on the loi
by all the scanners"
  (let* ((scanners
	   (set-scanners-visible   ; add list of visible points (col only)
	    (prune-scanners        ; only scanners that can see the line
	     (set-scanners-dist    ; distance to line
	      (make-scanner-list los) loi))))

	 (beacon-points (get-beacon-points scanners loi)) ; x-coords of beacons

	 (visible-points           ; list of x-coords for visible points on line
	   (remove-duplicates (mapcan #'(lambda (s) (scnr-visible s)) scanners))))

    ;;  remove beacon-points from visible-points and count the remainder
    (length (set-difference visible-points beacon-points))))

(5a:test day15-1-test
  (5a:is (= 26 (day15-1 *sample-data* *sample-loi*))))

#| ---------------------------------------------------------------------------------------
--- Part Two ---

The distress beacon is not detected by any sensor, but the distress beacon must have x
and y coordinates each no lower than 0 and no larger than 4000000.

To isolate the distress beacon's signal, you need to determine its tuning frequency,
which can be found by multiplying its x coordinate by 4000000 and then adding its y
coordinate.

NOTES: I could just do part one 4 million times. Hmm. That's pretty slow. The folks
in the Club TWiT #coding Discord suggest looking at points 1 unit outside the perimeter
described by each scanner. OK. Gotta be faster than scanning 4 million lines! One other
thing - and this might help with speed - since I now know the dimensions of the grid
I can convert x y coordinates into a single integer.
--------------------------------------------------------------------------------------- |#

(defparameter *width* 4000001)    ; "no larger than 4,000,000"
(defparameter *height* *width*)

(defstruct scnr2
  "a scanner, with location, range, and a set of points just out of view"
  (loc 0 :type integer)      ; x y position stored as (+ x (* y width))
  (range 0 :type integer)    ; the scanner's range
  (blind '() :type list))    ; list of points where the blind spot begins

(defstruct grid
  "a two dimensional grid of points of height h and width w"
  (h 0 :type integer)
  (w 0 :type integer))

(defparameter *sample-grid* (make-grid :h 20 :w 20)
  "sample data's grid for testing")

(defun xy-to-pt (p g)
  "convert x y coordinates on grid g into an integer"
  (+ (col p) (* (row p) (grid-w g))))

(defun pt-to-xy (p g)
  "convert an integer point into x y coordinates as (cons x y)"
  (cons (rem p (grid-w g)) (floor p (grid-w g))))

(defun freq (xy g)
  "determine the tuning frequency of a point"
  (+ (* (col xy) 4000000) (row xy)))

;; Time to build the scanner list
(defun make-scanner2-list (los g)
  "given a list of strings, each describing a scanner and beacon, return a list
of scanner structures, each populated with scanner locations and range capability"
  (flet ((parse-scanner-string (str)
	   (let* ((s (make-scnr2))          ; create an empty scanner
		  (digits (re:all-matches-as-strings *digits* str))   ; extract the digits
		  (s-pos (cons (parse-integer (first digits))         ; first two are
			       (parse-integer (second digits))))      ; scanner position
		  (b-pos (cons (parse-integer (third digits))         ; second pair are
			       (parse-integer (fourth digits)))))     ; beacon position

	     (setf (scnr2-loc s) (xy-to-pt s-pos g))   ; set the scanner's location
	     (setf (scnr2-range s)                     ; set the scanner range to the
		   (dist s-pos b-pos))                 ; distance from scanner to beacon
	     s)))

    (mapcar #'parse-scanner-string los)))

(defparameter *sample-scanners* (make-scanner2-list *sample-data* *sample-grid*)
  "the scanner list from the provided sample data - for testing")

;; now we need a function to calculate the blind spot perimeter of a scanner
;; start by creating a function that gives us the list of points representing
;; a diagonal line between two x y coordinates
(defun diagonal-points (x y)  ; points are (cons col row)
  (cond ((and (< (col x) (col y)) (< (row x) (row y))) ; going down and right
	 (loop
	   for i from (col x) upto (col y)
	   for j from (row x) upto (row y)
	   collect (cons i j)))

	((and (> (col x) (col y)) (< (row x) (row y))) ; going down and left
	 (loop
	   for i from (col x) downto (col y)
	   for j from (row x) upto (row y)
	   collect (cons i j)))))

(5a:test diagonal-points-test
  (5a:is (equal (diagonal-points (cons 0 0) (cons 3 3))      ; down right
		(list (cons 0 0) (cons 1 1) (cons 2 2) (cons 3 3))))
  (5a:is (equal (diagonal-points (cons 0 0) (cons -3 3))    ; down left
		(list (cons 0 0) (cons -1 1) (cons -2 2) (cons -3 3)))))

(defun set-blinds-scnr2 (s g)
  "given a scanner and a grid, create a list of points (as a single digit) that
represent the perimeter just beyond the scanners view"
  (let* ((loc (pt-to-xy (scnr2-loc s) g))                  ; position of scanner in x y coords
	 (blind (1+ (scnr2-range s)))                      ; the scanner's range + 1
	 (top (cons (col loc) (- (row loc) blind)))        ; the corners of the blind spot
	 (right (cons (+ (col loc) blind) (row loc)))
	 (bottom (cons (col loc) (+ (row loc) blind)))
	 (left  (cons (- (col loc) blind) (row loc))))

    (setf (scnr2-blind s)
	  (remove-duplicates                        ; corner points are duplicated
	   (mapcar #'(lambda (xy) (xy-to-pt xy g))  ; convert (cons row col) back to a digit
		   (append
		    (diagonal-points top right)      ; get points on diagonal working down
		    (diagonal-points right bottom)
		    (diagonal-points top left)
		    (diagonal-points left bottom)))))
    s))

(defun set-scanners-blinds (scanners g)
  "given a list of scanners on a grid, return the list with the populated blind spots
for each scanner"
  (mapcar #'(lambda (s) (set-blinds-scnr2 s g)) scanners))

(defun day15-2 (los g)
  (let ((scanners (set-scanners-blinds (make-scanner2-list los g) g)))
    (freq                                    ; get the frequency
     (reduce #'set-difference                ; of the single point left after differencing
	     (loop for s in scanners collect (scnr2-blind s))) g)))  ; all the blind spots

(5a:test day15-2-test
  (5a:is (= 56000011 (day15-2 *sample-data* *sample-grid*))))

;; now solve the puzzle!
;; (time (format t "The answer to AOC 2022 Day 15 Part 1 is ~a"
;;	      (day15-1 (uiop:read-file-lines *data-file*) 2000000)))

;; (time (format t "The answer to AOC 2022 Day 15 Part 2 is ~a"
;;	      (day15-2 (uiop:read-file-lines *data-file*))))

;; ---------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ---------------------------------------------------------------------------------------
