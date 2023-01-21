;;;; Day16.lisp
;;;; 2022 AOC Day 16 solution
;;;; Leo Laporte, 16 Jan 2022

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day16
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day16)

(setf fiveam:*run-test-when-defined* t) ; test as we go
(declaim (optimize (debug 3)))          ; max debugging info
;; (declaim (optimize (debug 0) (speed 3) (safety 0))) ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2022/day16/input.txt")  ;  data from AoC

#| --------------------- Day 16: Proboscidea Volcanium -------------------------

You scan the cave for other options and discover a network of pipes and
pressure-release valves. You aren't sure how such a system got into a volcano,
but you don't have time to complain; your device produces a report (your puzzle
input) of each valve's flow rate if it were opened (in pressure per minute) and
the tunnels you could use to move between the valves.

You estimate it will take you one minute to open a single valve and one minute
to follow any tunnel from one valve to another. What is the most pressure you
could release?

NOTES:

This is really interesting. I'm not going to think about part two at
all. Focusing on the problem at hand... The valves are a symmetric directed
graph in which every path can go either way so this is a pathfinding
problem. But we can only make 30 moves to get the most pressure so it's a
longest path problem, not shortest path. It's kind of a hybrid.

Additionally, the problem input has 66 valves, but only around 16 are
unbroken. So I think that means we can think of the 0 flow rooms as just part of
a longer path.

----------------------------------------------------------------------------- |#

(defparameter *example-data*
  '("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
    "Valve HH has flow rate=22; tunnel leads to valve GG"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
    "Valve JJ has flow rate=21; tunnel leads to valve II")
  "Example data from the problem")

(defstruct valve
  name      ; name of the valve (double caps)
  rate      ; rate of flow
  tunnels)  ; list of tunnels leading away

(defparameter *infinity* 99999)

(defparameter *valve-regex*
  (re:create-scanner
   "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.*)")
  "a regular expression to separate out the content from the cruft")

(defun parse-valves (los)
  (labels ((parse-valve (s)
	     (re:register-groups-bind
		 (name  (#'parse-integer rate) tunnels)
		 (*valve-regex* s)

	       (make-valve
		:name name
		:rate rate
		:tunnels (re:split ", " tunnels)))))

    (mapcar #'parse-valve los)))

(defparameter *example* (parse-valves *example-data*))
(defparameter *data* (parse-valves (uiop:read-file-lines *data-file*)))

;; some really useful tips (spoilers?) here:
;; https://www.reddit.com/r/adventofcode/comments/zo21au/comment/j0nz8df/

;; first we need to calculate the distances (in time units) between all nodes
;; using Floyd-Warshall algo (runs on dataset in 0.03 seconds)
(defun floyd (valves)
  "given a list of valve structures, return a hash with the keys being a cons of any two valves and the value being the time it takes to travel between those points"
  (let* ((num-valves (length valves))
	 (dists (make-hash-table :test 'equal :size (* num-valves num-valves))))

    ;; set up hash with to and from valves and "infinite" distances
    (dolist (v1 valves)
      (dolist (v2 valves)
	(setf
	 (gethash (cons (valve-name v1) (valve-name v2)) dists)
	 *infinity*)))

    ;; Now populate all the one move distances
    (dolist (v1 valves)
      (dolist (v2 (valve-tunnels v1))
	(setf (gethash (cons (valve-name v1) v2) dists) 1)))

    ;; now do the floyd-warshall walk through the hash to find the shortest
    ;; distances
    (dolist (i valves)
      (dolist (j valves)
	(dolist (k valves)
	  (let ((new-dist (+ (gethash (cons (valve-name i) (valve-name k))
				      dists)
			     (gethash (cons (valve-name k) (valve-name j))
				      dists))))
	    (when (< new-dist
		     (gethash (cons (valve-name i) (valve-name j)) dists))
	      (setf (gethash (cons (valve-name i) (valve-name j)) dists)
		    new-dist))))))

    ;; clean up points
    (loop for valves being the hash-keys in dists using (hash-value dist)
	  do (when (or (= dist *infinity*)                 ; infinite dist
		       (equalp (car valves) (cdr valves))) ; to = from
	       (remhash valves dists)))                    ; remove this entry

    dists))

(5a:test floyd-test
  (5a:is (= 1 (gethash (cons "AA" "DD") (floyd *example*))))
  (5a:is (= 5 (gethash (cons "BB" "GG") (floyd *example*))))
  (5a:is (= 3 (gethash (cons "EE" "HH") (floyd *example*)))))


(defun pht (hash)
  "little utility for printing the contents of a hash"
  (loop for k being the hash-keys in hash using (hash-value v)
	do (format t "~A => ~A~&" k v)))

;; Make a hash of rooms, costs, and flow-rates

;; DFS of rooms

;; track: valves that have already been opened (i.e. seen rooms)
;; time remaining (or if no more valves can be opened)





(5a:test day16-1-test
  (5a:is (= 1651 (day16-1 *example*))))

#| ---------------------------------- Part Two ---------------------------------


------------------------------------------------------------------------------|#

  ;; now solve the puzzle!
  ;; (time (format t "The answer to AOC 2022 Day 16 Part 1 is ~a"
  ;;	      (day16-1 (uiop:read-file-lines *data-file*))))

  ;; (time (format t "The answer to AOC 2022 Day 16 Part 2 is ~a"
  ;;	      (day16-2 (uiop:read-file-lines *data-file*))))

  ;; -----------------------------------------------------------------------------
  ;; Timings with SBCL on M2 MacBook Air with 24GB RAM
  ;; -----------------------------------------------------------------------------
