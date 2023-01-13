;;;; Day15.lisp
;;;; 2022 AOC Day 15 solution
;;;; Leo Laporte, 15 Dec 2022

;; -------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day15
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day15)

(setf fiveam:*run-test-when-defined* t) ; test as we go
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day15/input.txt")  ; AoC input

#| ------------------ Day 15: Beacon Exclusion Zone ------------------

--- Part One ---

"Sensors and beacons always exist at integer coordinates. Each sensor
knows its own position and can determine the position of a beacon
precisely; however, sensors can only lock on to the one beacon closest
to the sensor as measured by the Manhattan distance.  (There is never
a tie where two beacons are the same distance to a sensor.)

None of the detected beacons seem to be producing the distress signal,
so you'll need to work out where the distress beacon is by working out
where it isn't. For now, keep things simple by counting the positions
where a beacon cannot possibly be along just a single row.

Consult the report from the sensors you just deployed. In the row
where y=2000000, how many positions cannot contain a beacon?"

NOTES: So I'm guessing the size of this grid is going to be massive so
I don't want to calculate more than I have to. But it turns out the
problem is way simpler than it seems.

For each scanner I know its range (the dist to the associated
beacon). I can calculate how much it sees of the provided y coordinate
(I'll call it the line of interest (loi).  From there I can generate a
list of visible points (no need to do points, even, just
x-coordinates) and count them.

What's the geometry? If the distance to the loi is exactly equal to
the scanner's range that scanner can see exactly one point on the loi:
(scanner x loi-y), for every row closer to the loi, add a point left
and right of the origin.

Those are the visible points. Each scanner will have a set of visible
points.  Union the sets then count the resulting set to get the
answer. Done with part 1.

Again I'll represent points as (cons x y) and today it's in col row
order.

UPDATE: So unfortunately according the the example data if a beacon is
already in the loi that point does not count. I've been ignoring
beacons, but I guess I can't. I need to count beacons in the loi and
subtract that number from the total points I can see. Erg.
----------------------------------------------------------------------- |#

(defparameter *sample-data*
  '("Sensor at x=2, y=18: closest beacon is at x=-2, y=15"  ; note negative!
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

(defparameter *input-loi* 2000000
  "line of interest for the AoC input")

(defparameter *digits* (re:create-scanner "(-?\\d)+")
  "the regex to find digits in a string")

(defstruct scnr
  "a scanner, with location, range, distance to line of interest and
points on loi that it can see"
  (loc (cons 0 0) :type cons)      ; x y position stored as (cons x y)
  (beacon (cons 0 0) :type cons)   ; x y position of beacon
  (range 0 :type fixnum)           ; range
  (dist-to-loi 0 :type fixnum)     ; distance to the line of interest
  (visible '() :type list))        ; x posns visible on the loi

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
  "given a list of strings, each describing a scanner and beacon,
return a list of scanner structures, each populated with scanner and
beacon locations and range"
  (flet ((parse-scanner-string (str)
	   (let* ((s (make-scnr))   ; create an empty scanner
		  ;; extract the digits
		  (digits (re:all-matches-as-strings *digits* str))
		  ;; first two are scanner position
		  (s-pos (cons (parse-integer (first digits))
			       (parse-integer (second digits))))
		  ;; next two are beacon position
		  (b-pos (cons (parse-integer (third digits))
			       (parse-integer (fourth digits)))))

	     (setf (scnr-loc s) s-pos)     ; scanner location
	     (setf (scnr-beacon s) b-pos)  ; beacon location
	     (setf (scnr-range s)          ; scanner range is....
		   (dist s-pos b-pos))     ; ...dist from it to beacon
	     s)))

    (mapcar #'parse-scanner-string los)))

(defparameter *scanners* (make-scanner-list *sample-data*))

;; OK I've got a list of scanner structures. Now I need to know what
;; each scanner can see along the line of interest. Start by
;; calculating each scanner's range
(defun set-scanners-dist (scanners loi)
  "given a list of scanners and a line of interest, populate each
scanner with its distance to loi."
  (flet ((set-dist-to-loi (s)
	   (setf (scnr-dist-to-loi s)
		 (dist (scnr-loc s)
		       (cons (col (scnr-loc s)) loi))) ; dist to loi
	   s))
    (mapcar #'set-dist-to-loi scanners)))

;; No need to keep track of scanners that can't see that far. So prune
;; 'em.
(defun prune-scanners (scanners)
  "removes any scanners that can't see the line of interest"
  (remove-if
   #'(lambda (s) (> (scnr-dist-to-loi s) (scnr-range s)))
   scanners))

;; Now make a list of all the points on the line that each scanner can
;; see.
(defun set-visible-points (scanner)
  "returns a scanner with the visible field populated with a list of the
points on the loi it can see"
  (let ((num (- (scnr-range scanner)
		(scnr-dist-to-loi scanner))) ; how many visible pts
	(start (col (scnr-loc scanner))))    ; col of first visible point
    (setf (scnr-visible scanner)             ; set visible points
	  (loop for x from (- start num) upto (+ start num)
		collect x))                  ; working from left to right
    scanner))                                ; return updated scanner

(defun set-scanners-visible (scanners)
  "given a list of scanners, sets the list of visible points for each
and returns the new list of scanners"
  (mapcar #'set-visible-points scanners))

;; Oh but the problem has thrown me a curve. I ALSO have to eliminate
;; any points which already have a beacon on it. D'oh!
(defun get-beacon-points (scanners loi)
  "return a list of the x-coordinates of beacons in the scanner list
that are on the line of interest"
  (loop for s across scanners
	collecting
	(when (= (row (scnr-beacon s)) loi)
	  (col (scnr-beacon s)))))

(defun day15-1 (los loi)
  "given a list of strings describing a number of scanner-beacon pairs,
and the y-coordinate of a line of interest, return the number of
points covered on the loi by all the scanners"
  (let* ((scanners
	   (set-scanners-visible   ; add list of visible points (col only)
	    (prune-scanners        ; only scanners that can see the line
	     (set-scanners-dist    ; distance to line
	      (make-scanner-list los) loi))))

	 (beacon-points (get-beacon-points scanners loi)) ; beacon col

	 (visible-points           ; cols for visible points on line
	   (remove-duplicates
	    (mapcan #'(lambda (s) (scnr-visible s)) scanners))))

    ;; remove beacon-points from visible-points and count the remainder
    (length (set-difference visible-points beacon-points))))

(5a:test day15-1-test
  (5a:is (= 26 (day15-1 *sample-data* *sample-loi*))))

#| -------------------------------------------------------------------
--- Part Two ---

"The distress beacon is not detected by any sensor, but the distress
beacon must have x and y coordinates each no lower than 0 and no
larger than 4000000.

To isolate the distress beacon's signal, you need to determine its
tuning frequency, which can be found by multiplying its x coordinate
by 4000000 and then adding its y coordinate."

NOTES: I could just do part one 4 million times. Hmm. That's pretty
slow. Or I guess I could go through all 16,000,008,000,001 points to
see which one is invisible. That might take a while.

Can I reduce the set of points I need to look at? Well since there is
exactly one point in the grid that's invisible, I know it can't be
more than one point outside the range of all scanners. (If it were
farther out there'd be more than one invisible point and we're told
there's exactly one.) In other words, it has to be in the set of
points represented by the perimeters 1 point outside each scanner's
range - the invisible perimeter.

Furthermore since it's the only invisible point in the whole grid it
must occur in the set of all perimeter points at least four times. So
if I reduce the set of possibles by finding points in the blind spots
of four or more scanners I can then (quickly?) see which of those
points is invisible to all the scanners.

The only flaw in this logic is that a point in the corners of the grid
might only be bordered by one scanner. Points on the edge can be
bounded by as few as two scanners. So for completeness, I'll check for
these "edge cases" as well.
------------------------------------------------------------------- |#

(defparameter *width* 4000001)    ; "no larger than 4,000,000"
(defparameter *height* *width*)
(defparameter *sample-width* 21)
(defparameter *sample-height* 21)

(defun freq (x)
  "determine the tuning frequency of a point"
  (+ (* (col x) 4000000) (row x)))

(5a:test freq-test
  (5a:is (= 56000011 (freq (cons 14 11)))))

;; now we need a function to calculate the blind spot perimeter of a
;; scanner. Start by creating a function that gives us the list of
;; points representing a diagonal line between two x y coordinates
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

(defun set-scanner-invisibles (s)
  "given a scanner and a grid, create a list of points that represent
the perimeter just beyond the scanners view"
  (let* ((loc (scnr-loc s))                               ; position of scanner
	 (invisible (1+ (scnr-range s)))                  ; the scanner's range + 1
	 (top (cons (col loc) (- (row loc) invisible)))   ; the four corners
	 (right (cons (+ (col loc) invisible) (row loc)))
	 (bottom (cons (col loc) (+ (row loc) invisible)))
	 (left  (cons (- (col loc) invisible) (row loc))))

    (setf (scnr-visible s)
	  (append
	   (diagonal-points top right)
	   (rest (diagonal-points right bottom))
	   (rest (diagonal-points top left))      ; trim top
	   (rest (diagonal-points left bottom)))) ; trim left
    s))

(defun set-all-scanners-invisibles (scanners)
  "given a list of scanners, return the list populated with the blind
spots for each scanner"
  (mapcar #'set-scanner-invisibles scanners))

(defun see-it? (p scanner)
  "given a point and a scanner, return true if the scanner can see the
point"
  ;; is the point within the range of the scanner?
  (<= (dist p (scnr-loc scanner)) (scnr-range scanner)))

(5a:test see-it?-test
  (let ((ss (set-all-scanners-invisibles (make-scanner-list *sample-data*))))
    (5a:is-true (see-it? (cons 2 18) (first ss)))
    (5a:is-false (see-it? (cons -2 26) (first ss)))
    (5a:is-false (see-it? (cons -9 18) (first ss)))
    (5a:is-true (see-it? (cons 2 21) (first ss)))))

(defun invisible? (p scanners)
  "returns true if p is invisible to all scanners"
  (dolist (s scanners)
    (when (see-it? p s)               ; saw it!
      (return-from invisible? nil)))  ; so false
  t)                                  ; no scanner saw it, so true

(5a:test invisible?-test
  (let ((ss (set-all-scanners-invisibles (make-scanner-list *sample-data*))))
    (5a:is-false (invisible? (cons 2 18) ss))
    (5a:is-true (invisible? (cons 14 11) ss))))

(defun four-or-more (lst)
  "given a list of points return a list of points that appear four or
more times in the list"
  (cond ((null lst) nil)
	((= (count (first lst) lst :test #'equal) 4)
	 (cons (first lst) (four-or-more (rest lst))))
	(t (four-or-more (rest lst)))))

(5a:test four-or-more-test
  (5a:is (equal (four-or-more '(1 1 1 1)) '(1)))
  (5a:is (equal (four-or-more '(1 2 3 4 1 3 1 3 1)) '(1)))
  (5a:is (equal (four-or-more '(1 1 2 2 3 3 4 4 1 2 2 3 3 4)) '(2 3)))
  (5a:is (equal (four-or-more '(1 1 2 2 3 3 1 1 1 2 2 3 3 4)) '(1 2 3)))
  (5a:is (equal (four-or-more '(1 2 3 4)) '())))

(defun outside-grid? (pt h w)
  "Just in case, eliminate any perimeter points that are outside the
specified grid (21x21 in the example, 4000001x4000001 in the problem set)"
  (or (> 0 (col pt))
      (> (col pt) w)
      (> 0 (row pt))
      (> (row pt) h)))

(5a:test outside-grid?-test
  (5a:is-true (outside-grid? (cons -1 -1) *sample-height* *sample-width*))
  (5a:is-false (outside-grid? (cons 1 1) *sample-height* *sample-width*))
  (5a:is-true (outside-grid? (cons 22 22) *sample-height* *sample-width*))
  (5a:is-false (outside-grid? (cons 12 12) *sample-height* *sample-width*)))

(defun edge-case? (pt h w)
  "returns true if the point is in the corner of the grid or along its
edge"
  (or (= (col pt) 0)
      (= (col pt) (1- w))
      (= (row pt) 0)
      (= (row pt) (1- h))))

(5a:test edge-case?-test
  (5a:is-true (edge-case? (cons 0 1) 21 21))
  (5a:is-true (edge-case? (cons 20 1) 21 21))
  (5a:is-true (edge-case? (cons 11 0) 21 21))
  (5a:is-true (edge-case? (cons 0 20) 21 21)))

(defun day15-2 (los h w)
  (let* (;; build the scanner list with invisibles set
	 (scanners (set-all-scanners-invisibles (make-scanner-list los)))

	 ;; make a flat list of all the permimeter invisibles inside the grid
	 (visibles (remove-if #'(lambda (pt) (outside-grid? pt h w)) ;
			      (reduce #'append
				      (loop for s in scanners
					    collect (scnr-visible s)))))

	 ;; now reduce the list to edge cases and points appearing four or more times
	 (candidates (append (remove-if-not ; add in the edge cases
			      #'(lambda (pt) (edge-case? pt h w)) visibles)
			     (four-or-more visibles))))

    ;; go through list of points that appear 4 or more times in the list
    ;; of invisibles and look for a point invisible to all scanners
    (dolist (pt candidates)
      (format t ".") ; progress bar
      (when (invisible? pt scanners)
	(return-from day15-2 (freq pt))))
    (error "Could not find the invisible point!")))

(5a:test day15-2-test
  (5a:is (= 56000011 (day15-2 *sample-data* *sample-height* *sample-width*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 15 Part 1 is ~a"
	      (day15-1 (uiop:read-file-lines *data-file*) *input-loi*)))

(time (format t "The answer to AOC 2022 Day 15 Part 2 is ~a"
	      (day15-2 (uiop:read-file-lines *data-file*) *height* *width*)))

;; ---------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ---------------------------------------------------------------------------------------
