;;;; Day15.lisp
;;;; 2022 AOC Day 15 solution
;;;; Leo Laporte, 16 Jan 2022

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day15
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day15)

(setf fiveam:*run-test-when-defined* nil) ; test as we go
;; (declaim (optimize (debug 3)))          ; max debugging info
(declaim (optimize (speed 3) (debug 0))) ; max speed

(defconstant +data-file+ "~/cl/AOC/2022/day15/input.txt")  ; AoC input

#| ----------------------- Day 15: Beacon Exclusion Zone -----------------------

--- Part One ---

"Sensors and beacons always exist at integer coordinates. Each sensor knows its
own position and can determine the position of a beacon precisely; however,
sensors can only lock on to the one beacon closest to the sensor as measured by
the Manhattan distance.  (There is never a tie where two beacons are the same
distance to a sensor.)

None of the detected beacons seem to be producing the distress signal, so you'll
need to work out where the distress beacon is by working out where it isn't. For
now, keep things simple by counting the positions where a beacon cannot possibly
be along just a single row.

Consult the report from the sensors you just deployed. In the row where
y=2000000, how many positions cannot contain a beacon?"

NOTES:
We know that each scanner can see a swath of any given line - perhaps as little
as zero, perhaps encompassing the entire line. The width of the view is directly
related to the range of the scanner and its distance from the line.

View width = (1+ (* 2 (- md r))) where md is manhattan distance and r is the
scanner's range.

The view is centered on the scanner's x-axis. So the range is represented as
(- x (- md r), y .. (+ x (- md r)), y

For part one, just collect all the x points seen on the line of interest,
subtract any points occupied by beacons and return the number of remaining
points.
----------------------------------------------------------------------------- |#

(defparameter *example*
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

(defconstant +example-loi+ 10
  "line of interest for the sample scans")

(defconstant +input-loi+ 2000000
  "line of interest for the AoC input")

(defparameter *digits* (re:create-scanner "(-?\\d)+")
  "the regex to find digits in a string")

(defun col (p)
  "a mnemonic for the column data in a point stored as (cons x y)"
  (car p))

(defun row (p)
  "a mnemonic for the row data in a point stored as (cons x y)"
  (cdr p))

(defun dist (x y)
  "return the manhattan distance between two points, x y - can this be optimized?"
  (+ (abs (- (col x) (col y))) (abs (- (row x) (row y)))))

(defun pht (hash)
  "little utility for printing the contents of a hash"
  (loop for k being the hash-keys in hash using (hash-value v)
	do (format t "~A => ~A~&" k v)))

;; process provided data
(defun parse-lines (los)
  "returns two structures: a hash of scanners and their ranges, plus a list of
beacon points (for part 1)."
  (labels
      ((parse-line (str) ; process a single string
	 (let* ((digits (re:all-matches-as-strings *digits* str)) ; get digits
		(scanner (cons (parse-integer (first digits))     ; scanner
			       (parse-integer (second digits))))
		(beacon (cons (parse-integer (third digits))      ; beacon
			      (parse-integer (fourth digits))))
		(range (dist scanner beacon)))                    ; range
	   (values scanner beacon range))))                 ; return all 3

    ;; set up data structures for return values
    (let ((scanners        ; scanner=>range hash table
	    (make-hash-table :test 'equal :size (length los)))
	  (beacons '()))   ; list of beacon points

      (dolist (str los)
	(multiple-value-bind (s b r) (parse-line str)  ; parse line at a time
	  (setf (gethash s scanners) r)                ; build scanner hash
	  (push b beacons)))                           ; build beacon list

      (values scanners (remove-duplicates beacons :test #'equal))))) ; return both

(defun day15-1 (los loi)
  "given a list of strings describing a number of scanner and beacon locations,
 and the y-coordinate of a line of intrest, return the number of points visible
 on the loi by all the scanners but not occupied by beacons"
  (let ((points '()))  ; start with an empty list of visible points on loi

    ;; parse strings into a hash of scanner=>range and beacon points
    (multiple-value-bind (scanner-hash beacons) (parse-lines los)

      ;; then walk that hash
      (maphash
       #'(lambda (s r)
	   (let ((md (abs (- loi (row s))))) ; manhattan distance to line
	     (when (>= r md)                 ; if line is in range of scanner
	       (loop for pt                  ; build a list of visible pts
		       from (- (col s) (- r md)) upto (+ (col s) (- r md))
		     do (push pt points))))) ; save them to our list
       scanner-hash)

      (- (length (remove-duplicates points))  ; remove overlap and count
	 (length (remove-if-not #'(lambda (b) (= (row b) loi)) beacons))))))

(5a:test day15-1-test
  (5a:is (= 26 (day15-1 *example* +example-loi+))))

#| -------------------------------- Part Two -----------------------------------

"The distress beacon is not detected by any sensor, but the distress beacon must
have x and y coordinates each no lower than 0 and no larger than 4000000.

To isolate the distress beacon's signal, you need to determine its tuning
frequency, which can be found by multiplying its x coordinate by 4000000 and
then adding its y coordinate."

NOTES:

So, there's one beacon location that no scanner can see. I could test each point
to see if it's invisible to all scanners, but that would require looking at 16
quadrillion points (4 million x 4 million). Clearly we're going to have to
reduce the number of candidate points.

Because only one point in the entire grid is invisible (otherwise there would be
multiple correct answers) it must be the case that it lies just outside the
range of at least one scanner (if it's more than one point beuond all the
scanners range there would have to be multiple visible points). So we only have
to look at the set of points exactly one point outside the scanner range. I
first did this with a list but it was uselessly slow. Using a vector to store
the candidate points made a big difference. We should also make the visible?
check and manhattan distance calculation as fast as possible. They're called A
LOT.

This ended up being a geometry problem, but for me, primarily a problem in
optimizing for speed and memory usage.
----------------------------------------------------------------------------- |#

(defconstant +example-width+ 21)
(defconstant +example-height+ +example-width+)
(defconstant +input-width+ 4000001)
(defconstant +input-height+ +input-width+)

(defparameter *example-scanners* (parse-lines *example*)
  "hash of scanner=>range for example data")

(defun freq (x)
  "determine the tuning frequency of a point"
  (+ (* (col x) 4000000) (row x)))

(5a:test freq-test
  (5a:is (= 56000011 (freq (cons 14 11)))))

(defun outside-grid? (pt h w)
  "eliminate any perimeter points that are outside the
specified grid (21x21 in the example, 4000001x4000001 in the problem set)"
  (or (> 0 (col pt))    ; negative x
      (> (col pt) w)    ; x higher than width
      (> 0 (row pt))    ; negative y
      (> (row pt) h)))  ; y higher than height

(5a:test outside-grid?-test
  (5a:is-true (outside-grid? (cons -1 -1) +example-height+ +example-width+))
  (5a:is-false (outside-grid? (cons 1 1) +example-height+ +example-width+))
  (5a:is-true (outside-grid? (cons 22 22) +example-height+ +example-width+))
  (5a:is-false (outside-grid? (cons 12 12) +example-height+ +example-width+)))

(defun invisible? (pt scanner-hash)
  "returns true if p is invisible to all scanners"
  (loop for s being the hash-keys in scanner-hash using (hash-value r)
	do (when (<= (dist pt s) r)         ; if the point is in range of the scanner
	     (return-from invisible? nil))) ; return false, otherwise keep going
  t)  ; never returned false so pt must be invisible

(5a:test invisible?-test
  (5a:is-false (invisible? (cons 2 18) *example-scanners*))
  (5a:is-true (invisible? (cons 14 11) *example-scanners*)))

(defun perimeter-points (pt range)
  "given a scanner return a vector of points that represents the perimeter just
beyond the scanners view"
  (let* ((invisible (1+ range))    ; just out of range
	 (points (make-array (* 4 invisible) :adjustable t :fill-pointer 0))
	 (top (cons (col pt) (- (row pt) invisible)))    ; corner points
	 (right (cons (+ (col pt) invisible) (row pt)))  ; of the
	 (bottom (cons (col pt) (+ (row pt) invisible))) ; invisible
	 (left (cons (- (col pt) invisible) (row pt))))  ; perimeter

    (loop ; top to right
	  for c from (col top) below (col right)
	  for r from (row top) below (row right)
	  do (vector-push (cons c r) points))

    (loop ; right to bottom
	  for c from (col right) above (col bottom)
	  for r from (row right) below (row bottom)
	  do (vector-push (cons c r) points))

    (loop ; bottom to left
	  for c from (col bottom) above (col left)
	  for r from (row bottom) above (row left)
	  do (vector-push (cons c r) points))

    (loop ; left to top
	  for c from (col left) below (col top)
	  for r from (row left) above (row top)
	  do (vector-push (cons c r) points))

    points))

(5a:test perimeter-points-test
  (5a:is (equalp (perimeter-points (cons 5 5) 3)
		 #((5 . 1) (6 . 2) (7 . 3) (8 . 4)
		   (9 . 5) (8 . 6) (7 . 7) (6 . 8)
		   (5 . 9) (4 . 8) (3 . 7) (2 . 6)
		   (1 . 5) (2 . 4) (3 . 3) (4 . 2)))))

(defun day15-2 (los h w)
  "given a list of strings representing the locations of a number of scanners and
their closest beacons, find the single point that cannot be seen by any of the
scanners within a grid of height h and width w"
  (let ((scanner-hash (parse-lines los)))  ; build the scanner=>range hash

    ;; for each scanner, generate the perimeter and check for invisibility
    (maphash
     #'(lambda (scanner range)
	 ;; map should be set to return nil since we don't care about
	 ;; the result but in SBCL this overflows the heap for some
	 ;; reason, so I'm using 'list
	 (map 'list ; throw out the result - we just need the side effect
	      #'(lambda (pt)                     ; for every perimeter point
		  (unless (outside-grid? pt h w) ; outside grid? don't bother
		    (when (invisible? pt scanner-hash)   ; is it invisible?
		      (return-from day15-2 (freq pt))))) ; done!
	      (perimeter-points scanner range))) ; generate perimeter points
     scanner-hash)))

(5a:test day15-2-test
  (5a:is (= 56000011
	    (day15-2 *example* +example-height+ +example-width+))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 15 Part 1 is ~a"
	      (day15-1 (uiop:read-file-lines +data-file+) +input-loi+)))

  (time (format t "The answer to AOC 2022 Day 15 Part 2 is ~a"
		(day15-2 (uiop:read-file-lines +data-file+)
			 +input-height+ +input-width+)))

  ;; -----------------------------------------------------------------------------
  ;; Timings with SBCL on M2 MacBook Air with 24GB RAM
  ;; -----------------------------------------------------------------------------

  ;; The answer to AOC 2022 Day 15 Part 1 is 5838453
  ;; Evaluation took:
  ;; 0.928 seconds of real time
  ;; 0.928290 seconds of total run time (0.745965 user, 0.182325 system)
  ;; [ Run times consist of 0.505 seconds GC time, and 0.424 seconds non-GC time. ]
  ;; 100.00% CPU
  ;; 660,595,648 bytes consed

  ;; The answer to AOC 2022 Day 15 Part 2 is 12413999391794
  ;; Evaluation took:
  ;; 6.717 seconds of real time
  ;; 6.713806 seconds of total run time (6.442755 user, 0.271051 system)
  ;; [ Run times consist of 0.656 seconds GC time, and 6.058 seconds non-GC time. ]
  ;; 99.96% CPU
  ;; 790,144,752 bytes consed
