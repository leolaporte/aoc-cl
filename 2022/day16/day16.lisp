;;;; Day16.lisp
;;;; 2022 AOC Day 16 solution
;;;; Leo Laporte, 16 Jan 2022

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

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

#| --------------------- Day 16: Proboscidea Volcanium ----------------------

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
highest pressure problem, not shortest path. It's kind of a hybrid.

Additionally, the problem input has 66 valves, but only around 16 are
unbroken. So I think that means we can think of the 0 flow rooms as just
part of a longer path.

I think I can use Dijkstra for this but I need to build a table of valves
and the shortest travel time between them ignoring any valves that don't
work (flow=0) e.g. (DD . II) => 2. I'll do this using the Floyd-Warshall
algorithm.

The set of working valves is small enough that I probably could brute force
the optimal path with a depth first search but why not use Dijkstra to make
it faster? This has been fun because to use Dijkstra I have to modify it to
track time, additionally I'm looking for a maximum flow, not a minimum
distance.

Fundamentally Dijkstra works by looking at all the neighbors of the starting
point. It calculates the distance to each then puts them in a priority-queue,
sorted by the thing we're optimizing for (normally distance but in this case we
want to maximize total flow). Then we move to the optimal next point (the top
not yet visited value on the queue but, and this is important, not necessarily a
neighbor of the current point - we might have to backtrack if there's a better
route) then repeat the process. The algorithm concludes when it reaches the end
point - or in this case when either time runs out or all the valves have been
turned on.

So this is going to be a bit of a weird Dijkstra. Not only am I looking for a
maximum, I also have to track the time. I'm going to store the best flow
achieved by a valve and the time left after turning on the valve in the
structure itself. Let's see how it works.

It's just a tad low. And I notice that it's not choosing the same optimal path
that the example is. Hmm. I'm thinking that the issue is the time factor - I
don't know that a route isn't optimal until I run out of time. In other words,
Dijkstra's no better than testing all the paths. So it's a depth first search
for me. I think there are few enough nodes that this will be doable.

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
  ;; basic unchanging valve data provided by the problem
  (name "" :type string)             ; name of the valve (double caps)
  (rate 0 :type fixnum)              ; rate of flow
  (tunnels '() :type list))     ; list of tunnels leading away

(defconstant +infinity+ 99999 "an arbitrarily large value") ; for Floyd-Warshall

(defparameter *valve-regex*
  (re:create-scanner
   "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? (.*)")
  "a regular expression to separate out the content from the cruft in the
problem set")

(defun pht (hash)
  "little utility for printing the contents of a hash"
  (loop for k being the hash-keys in hash using (hash-value v)
        do (format t "~A => ~A~&" k v)))

(defun parse-valves (los)
  "converts a list of strings describing valves in a cave into a list of valve structures"
  (labels ((parse-valve (s)
             (re:register-groups-bind
                 (name (#'parse-integer rate) tunnels)
                 (*valve-regex* s)

               (make-valve
                :name name
                :rate rate
                :tunnels (re:split ", " tunnels)))))

    (mapcar #'parse-valve los)))

;; set these up for tests, experimentation
(defparameter *example* (parse-valves *example-data*))
(defparameter *data* (parse-valves (uiop:read-file-lines *data-file*)))

(defun get-valve (name valves)
  "given a valve name and list of valves return the valve record or nil if it
doesn't exist"
  (dolist (v valves)
    (when (equalp name (valve-name v))
      (return-from get-valve v)))
  nil)

(5a:test get-valve-test
         (5a:is (equalp (get-valve "AA" *example*)
                        (make-valve :name "AA" :tunnels '("DD" "II" "BB"))))
         (5a:is (equalp (get-valve "ZZ" *example*) nil)))

;; calculate the minimum travel time between all nodes using Floyd-Warshall algo
;; (it's slow: O(n^3) but runs on this dataset in 0.03 seconds) - only save
;; distances to valves that work
(defun build-dist-table (valves)
  "given a list of valve structures, return a hash with the keys being a cons of
any two valves and the value being the shortest time it takes to travel between
those points - ignore non-working valves"
  (let* ((num-valves (length valves))
         (dists (make-hash-table :test 'equal
                                 :size (* num-valves num-valves))))

    ;; set up hash with to and from valves and "infinite" distances
    (dolist (v1 valves)
      (dolist (v2 valves)
        (setf
         (gethash (cons (valve-name v1) (valve-name v2)) dists)
         +infinity+)))

    ;; populate all the one move distances
    (dolist (v1 valves)
      (dolist (v2 (valve-tunnels v1))
        (setf (gethash (cons (valve-name v1) v2) dists) 1)))

    ;; do the floyd-warshall walk through the hash to find the shortest
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

    ;; remove destinations we don't care about
    (loop for vs being the hash-keys in dists using (hash-value dist)
          do (when (or (equalp (car vs) (cdr vs))   ; to = from
                       (zerop                       ; dest valve broken
                        (valve-rate (get-valve (cdr vs) valves))))
               (remhash vs dists)))                 ; remove this entry

    dists)) ; return the final hash

(defparameter *example-dists* (build-dist-table *example*))

(5a:test build-dist-table-test
  (5a:is (= 1 (gethash (cons "AA" "DD") *example-dists*)))
  (5a:is (null (gethash (cons "BB" "GG") *example-dists*)))
  (5a:is (= 3 (gethash (cons "EE" "HH") *example-dists*)))
  (5a:is (null (gethash (cons "EE" "FF") *example-dists*))))

(defun get-neighbors (valve dists)
  "return list of valves that neighbor valve"
  (let ((dests '()))
    (loop for from-to being the hash-keys in dists
          do (when (equalp (car from-to) valve)
               (push (cdr from-to) dests)))
    dests))

(5a:test get-neighbors-test
  (5a:is (equalp (get-neighbors "CC" *example-dists*)
                 (list "JJ" "HH" "EE" "DD" "BB"))))

;; Prep done; it's depth first search time. The termination condition is 30
;; moves. (There are others but let's go full brute first).  So I have a map of
;; the cave, all the valves, and the inter-valve distances I should be able to
;; recurse and try all the 30-move variations, saving the largest so far.
;; For each recursion I need to track the total flow, total time so far,
;; Now the DFS using mutually recursive functions open-valve and next-valves
(defun next-valves (current neighbors time-left total-flow unopened valves distances)
  "returns a list of total flows for every possible path in graph - recurses with
open-valve, returns nil when out of time or no more valves to turn on"
  (cond ((null unopened) nil)           ; all valves open
        ((null neighbors) nil)          ; dead end
        ((< time-left 0) nil)           ; out of time

        (t (cons (open-valve current (first neighbors) time-left total-flow unopened valves distances)
                 (next-valves current (rest neighbors) time-left total-flow unopened valves distances)))))

(defun open-valve (prev next time-left total-flow unopened valves distances)
  "opens the next valve and updates time-left, total-flow, and unopened - mututal
recursion with next-valves"
  (let* ((time-after-visit (- time-left (gethash (cons prev next) distances)))
         (time-after-valve-on (- time-after-visit 1))) ; takes 1 minute to turn on valve

    (cond ((< time-after-valve-on 0) total-flow) ; out of time so return total flow

          ((not (member next unopened))   ; already opened
           (next-valves                   ; so go to neighboring valves
            next (get-neighbors next distances) time-after-visit total-flow unopened valves distances))

          (t (next-valves  ; otherwise open the valve and update time and flow
              next
              (get-neighbors next distances)
              time-after-valve-on
              (+ total-flow (* (valve-rate (get-valve next valves)) time-after-valve-on))
              (remove next unopened)
              valves
              distances)))))

(defun get-flows (los start time)
  "perform a depth-first-search on the valve graph, return the highest flow"
  (let* ((valves (parse-valves los))
         (distances (build-dist-table valves))   ; discards non-working valves
         (unopened (loop for v in valves         ; list of names of unopened valves
                         when (> (valve-rate v) 0)
                           collect (valve-name v))))

    (next-valves start (get-neighbors start distances) time 0 unopened valves distances)))

(defun day16-1 (los)
  (apply #'max (get-flows los "AA" 30)))

(5a:test day16-1-test
  (5a:is (= 1651 (day16-1 *example-data*))))

;;  "AA" "DD" "BB" "JJ" "HH" "EE" "CC"

#| ---------------------------------- Part Two ---------------------------------


------------------------------------------------------------------------------|#

;; now solve the puzzle!
;;(time (format t "The answer to AOC 2022 Day 16 Part 1 is ~a"
;;            (day16-1 (uiop:read-file-lines +data-file+))))

;; (time (format t "The answer to AOC 2022 Day 16 Part 2 is ~a"
;;            (day16-2 (uiop:read-file-lines +data-file+))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; -----------------------------------------------------------------------------
