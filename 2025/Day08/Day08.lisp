;;;; Day08.lisp
;;;; 2025 AOC Day 8 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: Thu Dec 11 15:51:26 2025
;;;; Finished: Fri 19 Dec 2025 08:55:40 PM PST

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day08
  (:use :cl :alexandria :iterate)       ; no prefix for these libraries
  (:local-nicknames                     ; short prefixes for these
   (:re :cl-ppcre)                      ; regex
   (:5a :fiveam)                        ; test framework
   (:sr :serapeum)                      ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day08)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day08/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;                        --- Day 8: Playground  ---
;;                              --- Part One ---
;;
;; ----------------------------------------------------------------------------
;;
;; These numbers represent "junction boxes" with X Y Z coordinates in a 3D
;; space. We (well the elves) want to connect the junction boxes that are
;; closest together in a straight line (i.e. Euclidean distance). So (dist p q)
;; = (sqrt (+ (expt (- xp xq) 2) (expt (- yp yq) 2) (expt (- zp zq) 2)
;;
;; Once we connect the two closest junction boxes to form a circuit, we look for
;; the next two. If any are already in a circuit the new junction points are
;; added to that circuit. And continue until we've connected all the points up
;; to the limit (10 for the example, 1000 for the input).
;;
;; Then multiply the largest 3 circuits (by the number of junction boxes in the
;; circuit) to get the answer. In the example it's (5x4x2) or 40.
;;
;; The problem asks us to "connect together the 1000 pairs of junction boxes
;; which are closest together. Afterward, what do you get if you multiply
;; together the sizes of the three largest circuits?"
;;
;; -----------------------------------------------------------------------------

(defparameter *example* (list "162,817,812"
                              "57,618,57"
                              "906,360,560"
                              "592,479,940"
                              "352,342,300"
                              "466,668,158"
                              "542,29,236"
                              "431,825,988"
                              "739,650,466"
                              "52,470,668"
                              "216,146,977"
                              "819,987,18"
                              "117,168,530"
                              "805,96,715"
                              "346,949,466"
                              "970,615,88"
                              "941,993,340"
                              "862,61,35"
                              "984,92,344"
                              "425,690,689"))

;; structure to represent the junction box coordinates, x y and z
(defstruct (junction
            (:conc-name j-)             ; short name is j-x, j-y. j-z
            (:constructor make-junc (x y z))) ; short constructor too
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(sr:-> parse-input (list) vector)
(defun parse-input (input)
  "given a list of strings representing junction box coordinates, x, y, and z,
return a vector containing JUNCTION structs for each"
  (iter (for box in input)
    (collect
        (apply #'make-junc (mapcar #'parse-integer (sr:words box)))
      result-type vector)))

;; I'll need a distance calculation. The problem says it's 'straight line
;; distance' and provides a link to a calculation for Euclidean distance, but I
;; don't need the precise distance so I'll round the result to an integer using
;; ISQRT instead of SQRT - it's much faster""
(sr:-> dist (junction junction) fixnum)
(defun dist (j1 j2)
  "given two JUNCTIONS j1 and j2 return the euclidean distance rounded to the
nearest integer between the two"
  (isqrt (+ (expt (- (j-x j1) (j-x j2)) 2)
            (expt (- (j-y j1) (j-y j2)) 2)
            (expt (- (j-z j1) (j-z j2)) 2))))

(5a:test dist-test
  (5a:is (= (dist (make-junc 1 2 3) (make-junc 4 5 6)) 5)))

;; Originally I stored the JUNCTION pairs and their distances as a hash table,
;; but I'm going to use a tuple instead of (dist x y z) - it's simpler and
;; easier to sort.
(sr:-> get-all-edges (vector) list)
(defun get-all-edges (junctions)
  "return a list of (distance j1 j2) for all pairs, sorted by distance"
  (let* ((len (length junctions))
         (edges nil))
    (iter (for i below len)
      (for j1 = (aref junctions i))
      (iter (for j from (1+ i) below len)
        (for j2 = (aref junctions j))
        (push (list (dist j1 j2) j1 j2) edges)))
    (sort edges #'< :key #'first)))

;; Now this is the part that took me the longest. I tried two different (naive)
;; methods of combining JUNCTION pairs into circuits, and they worked for part 1
;; but failed miserably for part 2 because I wasn't going in the proper
;; order. Part 2 wants the points in the circuit connection that, finally,
;; completes the circuit. I couldn't get that working even though I could
;; connect all the circuits. Hmmm.
;;
;; Then after checking Reddit I realized this is a "minimum spanning tree" or
;; MST problem: find the shortest path through the JUNCTION boxes (aka nodes) to
;; minimize the amount of light strings. Turns out there's a lot of computer
;; science behind these problems. And the key is the UNION-FIND data structure.
;;
;; Let me paraphrase the instructions. This helps me understand what I need to
;; do. For part 1 I need to start with the two junction boxes that are closest
;; together. Put them in a circuit. Check the next closest pair. If the pair
;; shares a junction box with the first circuit, add it to the circuit,
;; otherwise create a new circuit. Take the next closest pair. Check to see if
;; any of its points fit into an existing circuit without creating a loop. (It's
;; a loop if both points in the junction box pair already exist in the circuit.)
;; Only add a pair to an existing circuit if exactly one of the two points
;; exists in the circuit. Proceed until all the junction boxes are in a circuit,
;; even if it's only a circuit of one. Multiply the sizes of the three largest
;; circuits together to get the answer.
;;
;; To make it clear we're using a graph data structure, I'll call a junction box
;; a *node* and the distance between two nodes an *edge*. The circuit I'll call
;; a *path*. A path is really just a *set* of nodes, so I can add a node to a
;; path using UF-UNION? to check if a node is already in a circuit avoiding
;; loops.
;;
;; This is the fundamental process for the whole problem. It turns out that this
;; is an example of Kruskal's Algorithm (or Prim's algorithm which is
;; similar). It delivers the "minimum spanning tree of an undirected edge weight
;; graph." In other words, the shortest string of lights that can connect all
;; the junction boxes. Eric basically handed this to us - if I had studied
;; computer science I'm sure I would have sussed that straight off. As it is I
;; needed the hint from Reddit. And Chapters 7 and 8 of Steven S. Skiena's
;; excellent "Algorithm Design Manual - Third Edition" Springer 2020 -
;; understanding graphs probably solves 80% of AoC problems.
;;
;; In this case the weights are the distances between points, and the minimum
;; tree is the shortest length of Christmas lights. So this describes the
;; problem exactly.
;;
;; We already have the first two steps done (from Wikipedia):
;;
;; 1. Create a forest (a set of trees) initially consisting of a separate
;; single-vertex tree for each vertex in the input graph. (DONE)
;;
;; 2. Sort the graph edges by weight. (DONE)
;;
;; Now...
;;
;; 3. Loop through the edges of the graph, in ascending sorted order by their
;; weight.
;;
;; 3.1 For each edge:
;;  - Test whether adding the edge to the current forest would create a cycle.
;;  -If not, add the edge to the forest, combining two trees into a single tree.
;;
;; At the termination of the algorithm, the forest forms a minimum spanning
;; forest of the graph. If the graph is connected, the forest has a single
;; component and forms a minimum spanning tree. Ta da!
;;
;; I asked Claude to work up a UNION-FIND with Kruskal and then re-worked it for
;; the problem. Mostly today for me was devoted to understanding this new data
;; structure and algorithm.
;;
;; Unfortunately Claude relied on the fact that hash-tables in Common Lisp are
;; passed by reference, and some of the functions below mutate UF as a side
;; effect instead of explicitly passing the structure around. I don't like doing
;; it this way - and I'll probably rework it when I get a chance. But until
;; then, consider yourself warned.

(sr:-> make-union-find () cons)
(defun make-union-find ()
  "create a new union-find structure, a CONS of two hash-tables with the key =
parent -> value =rank"
  (cons (make-hash-table :test 'equalp) ; parent
        (make-hash-table :test 'equalp))) ; rank

(sr:-> find-root (cons atom) atom)
(defun find-root (uf node)
  "find the root of NODE with path compression. Recurses up the tree until it finds
the root - WARNING: UF is passed by reference and so is modified as a side-effect"
  (let ((parent (car uf)))
    ;; Initialize node if not seen
    (unless (gethash node parent)
      (setf (gethash node parent) node)) ; mutates UF!
    ;; Find root with path compression
    (if (equalp (gethash node parent) node)
        node
        (setf (gethash node parent)     ; mutates UF!
              (find-root uf (gethash node parent))))))

(sr:-> uf-union? (cons atom atom) boolean)
(defun uf-union? (uf node1 node2)
  "union the sets containing NODE1 and NODE2 - if they can be combined without
creating a cycle update UF and return t, otherwise return nil - WARNING: UF is
passed by reference and so is modified as a side-effect"
  (let* ((parent (car uf))
         (rank (cdr uf))
         (root1 (find-root uf node1))
         (root2 (find-root uf node2)))
    (unless (gethash root1 rank) (setf (gethash root1 rank) 0))
    (unless (gethash root2 rank) (setf (gethash root2 rank) 0))
    (cond
      ((equalp root1 root2) nil)        ; already in same set
      ((< (gethash root1 rank) (gethash root2 rank))
       (setf (gethash root1 parent) root2) ; mutates UF
       t)
      ((> (gethash root1 rank) (gethash root2 rank))
       (setf (gethash root2 parent) root1) ; mutates UF
       t)
      (t
       (setf (gethash root2 parent) root1) ; mutates UF
       (incf (gethash root1 rank))
       t))))

(sr:-> uf-component-sizes (cons list) list)
(defun uf-component-sizes (uf nodes)
  "return a list of component sizes for all NODES in union-find UF - we need this
to solve part 1"
  (let ((size-map (make-hash-table :test 'equalp)))
    (iter (for node in nodes)
      (let ((root (find-root uf node)))
        (incf (gethash root size-map 0))))
    (hash-table-values size-map)))

(5a:test union-find-test
  (let ((uf (make-union-find)))
    ;; Initially separate
    (5a:is-true (uf-union uf 'a 'b))    ; conected
    (5a:is-false (uf-union uf 'a 'b))   ; loop
    (5a:is-true (uf-union uf 'c 'd))    ; new junction
    (5a:is-true (uf-union uf 'b 'c))    ; connects {a,b} with {c,d}
    (5a:is-false (uf-union uf 'a 'd))   ; a and d are already in UF
    (5a:is-true (equal (list 4) (uf-component-sizes uf '(a b c d))))
    (sr:pretty-print-hash-table (car uf))
    (sr:pretty-print-hash-table (cdr uf))))

;;  OK now I have all the pieces in place. KRUSKAL does all the work of solving
;;  part one - using the algorithm described above.
(sr:-> kruskal (list fixnum) list)
(defun kruskal (edges max-edges)
  "process up to MAX-EDGES edges using Kruskal's algorithm
   returns (component-sizes last-j1 last-j2)"
  (let ((uf (make-union-find))
        (nodes nil)
        (last-j1 nil)
        (last-j2 nil)
        (edges-processed 0))
    (iter (for edge in edges)
      (let ((j1 (second edge))
            (j2 (third edge)))

        ;; only do this for the MAX-EDGES
        (while (< edges-processed max-edges))
        ;; Track all nodes we've seen
        (pushnew j1 nodes :test 'equalp)
        (pushnew j2 nodes :test 'equalp)
        ;; Union the nodes
        (uf-union uf j1 j2)             ; modifies UF in place
        (setf last-j1 j1 last-j2 j2)
        (incf edges-processed)))
    (values (uf-component-sizes uf nodes) last-j1 last-j2)))

(sr:-> day08-1 (list fixnum) fixnum)
(defun day08-1 (input max-edges)
  "given a list of junction boxes, INPUT, return the result of multiplying
 together the three largest possible circuits that can be built by connecting
 the MAX-EDGES closest pairs. Uses Kruskal's algorithm with Union-Find."
  (let* ((junctions (parse-input input))
         (edges (get-all-edges junctions))
         (sizes (kruskal edges max-edges)))

    (sr:~> sizes
           (sort _ #'>)
           (subseq _ 0 (min 3 (length _)))
           (apply #'* _))))

(5a:test day08-1-test
  (5a:is (= 40 (day08-1 *example* 10))))

;; ----------------------------------------------------------------------------
;;                           -- Part Two --
;;
;; Part 2 threw me for a loop at first. Now we want to connect all the JUNCTION
;; pairs and, when connected, multiply the X coordinates of the pair that
;; completes the circuit.
;;
;; My first instinct was wrong. I thought "just keep connecting until everything
;; is connected and remember the last pair." But my old BUILD-PATHS function was
;; merging circuits in a haphazard way - it wasn't properly tracking which edge
;; actually caused the final merge.
;;
;; Once I understood that I'm building a *Minimum Spanning Tree* and found the
;; UNION-FIMD data structure I was able to solve part two economically (and
;; correctly!).
;;
;; To connect n nodes into a single tree, we need exactly n-1 edges. Each edge
;; we add (that actually merges two components) reduces our component count by
;; one. We start with n components (each junction box alone) and we need to get
;; down to 1 component (everything connected).

;; So the algorithm is:
;; 1. Start with n components (one per junction box)
;; 2. Process edges shortest-to-longest (same as Part 1)
;; 3. Each time UF-UNION returns T, we successfully merged two components -
;; decrement our component counter and remember this edge
;; 4. Stop when components = 1 (everything is connected)
;; 5. The last edge that returned T is our answer!
;;
;; ----------------------------------------------------------------------------

(sr:-> day08-2 (list) fixnum)
(defun day08-2 (input)
  "find the pair that connects all junction boxes into a single circuit.
   Return the product of the X coordinates of that pair."
  (let* ((junctions (parse-input input))
         (edges (get-all-edges junctions))
         (uf (make-union-find))
         (n (length junctions))
         (components n)                 ; start with n separate components
         (last-j1 nil)
         (last-j2 nil))

    ;; Initialize all nodes in union-find
    (iter (for j in-vector junctions)
      (find-root uf j))

    ;; Process edges until we have 1 component (all connected)
    (iter (for edge in edges)
      (let ((j1 (second edge))
            (j2 (third edge)))
        (while (> components 1))
        (when (uf-union uf j1 j2)
          ;; edge merged j1 j2 - uf updated in place
          (decf components)
          (setf last-j1 j1 last-j2 j2))))

    (* (j-x last-j1) (j-x last-j2))))

(5a:test day08-2-test
  (5a:is (= 25272 (day08-2 *example*))))

;; ----------------------------------------------------------------------------
;; Now solve the puzzle...
;; ----------------------------------------------------------------------------

(time (format t "The answer to AOC 2025 Day 8 Part 1 is ~a"
              (day08-1 (uiop:read-file-lines *data-file*) 1000)))

(time (format t "The answer to AOC 2025 Day 8 Part 2 is ~a"
              (day08-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timing with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 8 Part 1 is 105952
;; Evaluation took:
;; 0.200 seconds of real time
;; 0.200346 seconds of total run time (0.198009 user, 0.002337 system)
;; [ Real times consist of 0.011 seconds GC time, and 0.189 seconds non-GC time. ]
;; [ Run times consist of 0.011 seconds GC time, and 0.190 seconds non-GC time. ]
;; 100.00% CPU
;; 32,675,616 bytes consed

;; The answer to AOC 2025 Day 8 Part 2 is 975931446
;; Evaluation took:
;; 0.170 seconds of real time
;; 0.170823 seconds of total run time (0.170458 user, 0.000365 system)
;; 100.59% CPU
;; 32,548,464 bytes consed
