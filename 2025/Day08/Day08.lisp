;; [[file:Day08.org::setup][setup]]
;;;; Day08.lisp
;;;; 2025 AOC Day 8 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: Thu Dec 11 15:51:26 2025
;;;; Finished: Fri 19 Dec 2025 08:55:40 PM PST

(defpackage :aoc.2025.day08
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day08)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day08/input.txt"
  "Downloaded from the AoC problem set")
;; setup ends here

;; [[file:Day08.org::*Example Data][Example Data:1]]
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

(defparameter *example2* (list "1,1,2"
                               "2,14,4"
                               "2,5,6"
                               "3,4,11"
                               "5,8,10"
                               "1,2,7"
                               "12,4,12"
                               "5,2,9"
                               "3,8,11"
                               "16,2,9"
                               "13,4,15"
                               "5,16,7"
                               "18,9,10"
                               "100,100,100")
  "Another perverse example from the fertile mind of Paul Holder Part 1 is the three circuit sizes 7, 2, 2 so the answer is 28, and part 2 is 18, 100 so the answer is 1800")
;; Example Data:1 ends here

;; [[file:Day08.org::struct-junction][struct-junction]]
(defstruct (junction
            (:conc-name j-)             ; short name is j-x, j-y. j-z
            (:constructor make-junc (x y z)))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(format t "Print representation: ~a~%"(make-junc 1 2 3))
;; struct-junction ends here

;; [[file:Day08.org::parse-input][parse-input]]
(sr:-> parse-input (list) vector)
(defun parse-input (input)
  (iter (for box in input)
    (collect
        (apply #'make-junc (mapcar #'parse-integer (sr:words box)))
      result-type vector)))

(format t "~%~a" (parse-input *example*))
;; parse-input ends here

;; [[file:Day08.org::dist][dist]]
(sr:-> dist (junction junction) fixnum)
(defun dist (j1 j2)
  "given two JUNCTIONS j1 and j2 return the euclidean distance rounded to the nearest integer between the two"
  (isqrt (+ (expt (- (j-x j1) (j-x j2)) 2)
            (expt (- (j-y j1) (j-y j2)) 2)
            (expt (- (j-z j1) (j-z j2)) 2))))

(5a:test dist-test
  (5a:is (= (dist (make-junc 1 2 3) (make-junc 4 5 6)) 5)))
;; dist ends here

;; [[file:Day08.org::get-dists][get-dists]]
(sr:-> get-dists (vector) hash-table)
(defun get-dists (junctions)
  "given a vector of JUNCTION structs, return a hash-table of the distances
 between any two pairs of junctions. The key will be the distance, the value the
 pair of junctions that are that far apart"
  (let* ((len (length junctions))
         (distances (make-hash-table :size len)))

    (iter (for i below len)
      (for j1 = (aref junctions i))
      (iter (for j from (1+ i) below len)
        (for j2 = (aref junctions j))

        (let ((d (dist j1 j2)))
          (setf (gethash d distances)
                (append (list j1 j2)
                        (gethash d distances))))))

    distances))
;; get-dists ends here

;; [[file:Day08.org::get-dists-test][get-dists-test]]
(let ((*trace-output* *standard-output*))
  (time (get-dists (parse-input (uiop:read-file-lines *data-file*)))))
;; get-dists-test ends here

;; [[file:Day08.org::10-junctions][10-junctions]]
(let* ((junctions (parse-input *example*))
       (distances (get-dists junctions))
       (keys (sort (hash-table-keys distances) #'<)))
  (iter (for i below 10)
    (format t "~&~a -> ~a" (gethash (nth i keys) distances) (nth i keys))))
;; 10-junctions ends here

;; [[file:Day08.org::sort-pairs-by-distance][sort-pairs-by-distance]]
(sr:-> sort-pairs-by-distance (list) list)
(defun sort-pairs-by-distance (input)
  "given a list of strings representing a list of JUNCTIONS, return a list of
 JUNCTION pairs sorted ascending by the distance between the pair"
  (let* ((distances (get-dists (parse-input input))) ; hash of dists->juncs
         (keys (sort (hash-table-keys distances) #'<))) ; list of sorted dists

    (iter (for k in keys)
      (collect (gethash k distances)))))
;; sort-pairs-by-distance ends here

;; [[file:Day08.org::sort-pairs-by-distance-profile][sort-pairs-by-distance-profile]]
(let ((*trace-output* *standard-output*))
  (time (sort-pairs-by-distance (uiop:read-file-lines *data-file*))))
;; sort-pairs-by-distance-profile ends here

;; [[file:Day08.org::sort-pairs-by-distance-test][sort-pairs-by-distance-test]]
(5a:test sort-pairs-by-distance-test
  (let ((closest (subseq (sort-pairs-by-distance *example*) 0 4)))
    (5a:is (equalp (first closest)
                   (list (make-junc 162 817 812) (make-junc 425 690 689))))
    (5a:is (equalp (second closest)
                   (list (make-junc 162 817 812) (make-junc 431 825 988))))
    (5a:is (equalp (third closest)
                   (list (make-junc 906 360 560) (make-junc 805 96 715))))
    (5a:is (equalp (fourth closest)
                   (list (make-junc 431 825 988) (make-junc 425 690 689))))))
;; sort-pairs-by-distance-test ends here

;; [[file:Day08.org::union-find][union-find]]
;; Union-Find data structure using hash tables (for struct keys)
(defun make-union-find ()
  "Create a new union-find structure. Returns (parent . rank) hash tables."
  (cons (make-hash-table :test 'equalp)   ; parent
        (make-hash-table :test 'equalp))) ; rank

(defun uf-find (uf node)
  "Find the root of NODE with path compression."
  (let ((parent (car uf)))
    ;; Initialize node if not seen
    (unless (gethash node parent)
      (setf (gethash node parent) node))
    ;; Find root with path compression
    (if (equalp (gethash node parent) node)
        node
        (setf (gethash node parent)
              (uf-find uf (gethash node parent))))))

(defun uf-union (uf node1 node2)
  "Union the sets containing NODE1 and NODE2. Returns T if they were separate."
  (let* ((parent (car uf))
         (rank (cdr uf))
         (root1 (uf-find uf node1))
         (root2 (uf-find uf node2)))
    (unless (gethash root1 rank) (setf (gethash root1 rank) 0))
    (unless (gethash root2 rank) (setf (gethash root2 rank) 0))
    (cond
      ((equalp root1 root2) nil)  ; already in same set
      ((< (gethash root1 rank) (gethash root2 rank))
       (setf (gethash root1 parent) root2)
       t)
      ((> (gethash root1 rank) (gethash root2 rank))
       (setf (gethash root2 parent) root1)
       t)
      (t
       (setf (gethash root2 parent) root1)
       (incf (gethash root1 rank))
       t))))

(defun uf-component-sizes (uf nodes)
  "Return a list of component sizes for all NODES in union-find UF."
  (let ((size-map (make-hash-table :test 'equalp)))
    (iter (for node in nodes)
      (let ((root (uf-find uf node)))
        (incf (gethash root size-map 0))))
    (hash-table-values size-map)))

(5a:test union-find-test
  (let ((uf (make-union-find)))
    ;; Initially separate
    (5a:is (uf-union uf 'a 'b))  ; returns T, now connected
    (5a:is-false (uf-union uf 'a 'b))  ; returns NIL, already connected
    (5a:is (uf-union uf 'c 'd))
    (5a:is (uf-union uf 'b 'c))  ; connects {a,b} with {c,d}
    (5a:is-false (uf-union uf 'a 'd))  ; all in same component now
    (5a:is (equal (list 4) (uf-component-sizes uf '(a b c d))))))
;; union-find ends here

;; [[file:Day08.org::get-all-edges][get-all-edges]]
(sr:-> get-all-edges (vector) list)
(defun get-all-edges (junctions)
  "Return a list of (distance j1 j2) for all pairs, sorted by distance."
  (let* ((len (length junctions))
         (edges nil))
    (iter (for i below len)
      (for j1 = (aref junctions i))
      (iter (for j from (1+ i) below len)
        (for j2 = (aref junctions j))
        (push (list (dist j1 j2) j1 j2) edges)))
    (sort edges #'< :key #'first)))
;; get-all-edges ends here

;; [[file:Day08.org::kruskal][kruskal]]
(defun kruskal (edges max-edges)
  "Process up to MAX-EDGES edges using Kruskal's algorithm.
   Returns (component-sizes last-j1 last-j2)."
  (let ((uf (make-union-find))
        (nodes nil)
        (last-j1 nil)
        (last-j2 nil)
        (edges-processed 0))
    (iter (for (dist j1 j2) in edges)
      (while (< edges-processed max-edges))
      ;; Track all nodes we've seen
      (pushnew j1 nodes :test 'equalp)
      (pushnew j2 nodes :test 'equalp)
      ;; Union the nodes
      (uf-union uf j1 j2)
      (setf last-j1 j1 last-j2 j2)
      (incf edges-processed))
    (values (uf-component-sizes uf nodes) last-j1 last-j2)))
;; kruskal ends here

;; [[file:Day08.org::day08-1][day08-1]]
(defun day08-1 (input max-edges)
  "Given a list of junction boxes, INPUT, return the result of multiplying
 together the three largest possible circuits that can be built by connecting
 the MAX-EDGES closest pairs. Uses Kruskal's algorithm with Union-Find."
  (let* ((junctions (parse-input input))
         (edges (get-all-edges junctions))
         (sizes (kruskal edges max-edges)))
    (sr:~> sizes
           (sort _ #'>)
           (subseq _ 0 (min 3 (length _)))
           (apply #'* _))))
;; day08-1 ends here

;; [[file:Day08.org::*Test][Test:1]]
(5a:test day08-1-test
  (5a:is (= 40 (day08-1 *example* 10))))
;; Test:1 ends here

;; [[file:Day08.org::day08-2][day08-2]]
(defun day08-2 (input)
  "Find the pair that connects all junction boxes into a single circuit.
   Return the product of the X coordinates of that pair."
  (let* ((junctions (parse-input input))
         (edges (get-all-edges junctions))
         (uf (make-union-find))
         (n (length junctions))
         (components n)  ; start with n separate components
         (last-j1 nil)
         (last-j2 nil))

    ;; Initialize all nodes in union-find
    (iter (for j in-vector junctions)
      (uf-find uf j))

    ;; Process edges until we have 1 component (all connected)
    (iter (for (dist j1 j2) in edges)
      (while (> components 1))
      (when (uf-union uf j1 j2)
        ;; This edge merged two separate components
        (decf components)
        (setf last-j1 j1 last-j2 j2)))

    (* (j-x last-j1) (j-x last-j2))))
;; day08-2 ends here

;; [[file:Day08.org::*Test][Test:1]]
(5a:test day08-2-test
  (5a:is (= 25272 (day08-2 *example*))))
;; Test:1 ends here

;; [[file:Day08.org::*Run Solutions and Timings][Run Solutions and Timings:1]]
;; now solve the puzzle!

(let ((*trace-output* *standard-output*))
  (time (format t "The answer to AOC 2025 Day 8 Part 1 is ~a"
                (day08-1 (uiop:read-file-lines *data-file*) 1000)))

(time (format t "The answer to AOC 2025 Day 8 Part 2 is ~a"
              (day08-2 (uiop:read-file-lines *data-file*)))))
;; Run Solutions and Timings:1 ends here
