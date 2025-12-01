;;;; Day08.lisp
;;;; 2023 AOC Day 08 solution
;;;; Leo Laporte
;;;; 3-5 January 2024

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day08
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day08)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/day08/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 8: Haunted Wasteland ---
--- Part One ---

"one of the documents contains a list of left/right instructions, and
the rest of the documents seem to describe some kind of network of
labeled nodes.

It seems like you're meant to use the left/right instructions to
navigate the network.

AAA is where you are now, and you have to follow the left/right
instructions until you reach ZZZ.

If you run out of left/right instructions, repeat the whole sequence
of instructions as necessary

Starting at AAA, follow the left/right instructions. How many steps
are required to reach ZZZ?"

LEO'S NOTES: The input is a list of lines, first line is the
left/right instructions, lines 2 throught the end of the list are the
nodes in the form of "NODE = (NODE, NODE)"

Looks like we can store the left-right instructions as a string or
list. Maybe list would be easier to work with. And the nodes should be
a hash in the form of NODE=>(cons NODE NODE).

I know it's really a binary tree. But does that help me? This is tree
traversal, true and it would be natural to implement it that way but
honestly it's probably faster to use hash-tables, especially since I'm
not walking the tree but jumping around. Maybe in part 2 I'll regret
that.

------------------------------- Parse Code ---------------------------- |#

(defparameter *test-data*
  '("LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)"))

(defparameter *node-regex* (re:create-scanner
                            "([A-Z]{3}).+([A-Z]{3}).+([A-Z]{3})")
  "the regular expression to match the node string e.g. VCD = (GDJ, GGM)
- creates three groups, one per node")

(defun parse-data (los)
  (let ((directions (coerce (first los) 'list)) ; turn string into a list of dirs

        ;; and make a hash table for storing the nodes
        (tree-hash (make-hash-table :test 'equalp :size (- (length los) 2))))

    (dolist (n (rest (rest los)))  ; use regex to populate the hash
      (re:register-groups-bind (key left right) (*node-regex* n)
        (setf (gethash key tree-hash) (cons left right))))

    (values directions tree-hash))) ; return list of dirs and a hash of nodes

#|---------------------------- Working Code ----------------------------|#

(defun Day08-1 (los)
  ;; parse the data into a list of directions and a tree of nodes
  (multiple-value-bind (directions tree) (parse-data los)

    ;; set the end of directions to point to the beginning so it's circular
    (setf (cdr (last directions)) directions)

    (do ((d directions (rest d))        ; loop eternally
         (node "AAA")                   ; the starting node
         (steps 0 (incf steps))) ; number of steps so far (increments every loop)

        ;; termination clause - end when we reach node "ZZZ"
        ;; return the number of steps it took
        ((equalp node "ZZZ") steps)

      ;; body of the look - walk the tree
      (if (equalp (first d) #\L)
          (setf node (car (gethash node tree)))     ; left node
          (setf node (cdr (gethash node tree))))))) ; right node

(5a:test parse-data-test
  "Test that parse-data correctly parses directions and node mappings"
  (multiple-value-bind (directions tree) (parse-data *test-data*)
    (5a:is (equal directions '(#\L #\L #\R)))
    (5a:is (equal (gethash "AAA" tree) '("BBB" . "BBB")))
    (5a:is (equal (gethash "BBB" tree) '("AAA" . "ZZZ")))
    (5a:is (equal (gethash "ZZZ" tree) '("ZZZ" . "ZZZ")))
    (5a:is (= (hash-table-count tree) 3))))

(5a:test Day08-1-test
  (5a:is (= (Day08-1 *test-data*) 6)))

#| -----------------------------------------------------------------------------
--- Part Two ---

After examining the maps a bit longer, your attention is drawn to a
curious fact: the number of nodes with names ending in A is equal to
the number ending in Z! If you were a ghost, you'd probably just start
at every node that ends with A and follow all of the paths at the same
time until they all simultaneously end up at nodes that end with Z.

Simultaneously start on every node that ends with A. How many steps
does it take before you're only on nodes that end with Z?

LEO'S NOTES: Well I'm glad I didn't spend any time building tree
structures. In fact it almost seems like I'm doing lock cracking. Each
A node is a tumblr, I spin it until it reaches Z. The trick is getting
them all to reach Z at the same time (and the lock opens!) I think I
can use the same code but adding a check for ending in A and ending in
Z.

Hol' on pardner. It's been running ALL night and the sheriff is
getting suspicious. Looks like getting all the tumblers to synch is
going to take more than a few cycles. Hmm.

LCM to the rescue! I do each tumbler one at a time then multiply the
results together to get the moment (way into the future) when they all
click! (Instead of multiplying find the least common multiplier using #'LCM)

------------------------------------------------------------------------------|#

(defparameter *test-data2*
  '("LR"
    ""
    "AAA = (AAB, XXX)"
    "AAB = (XXX, AAZ)"
    "AAZ = (AAB, XXX)"
    "BBA = (LLB, XXX)"
    "LLB = (MMC, MMC)"
    "MMC = (JJZ, JJZ)"
    "JJZ = (LLB, LLB)"
    "XXX = (XXX, XXX)")
  "this is the provided example, but I had to replace the numbers with
letters - why did Eric do that?")

(defun ends-with-a? (node)
  "returns true if a node ends with the letter A"
  (equalp #\A (elt node 2)))

(defun ends-with-z? (node)
  "returns true if a node ends with the letter Z"
  (equalp #\Z (elt node 2)))

(5a:test ends-with-a-test
  "Test that ends-with-a? correctly identifies nodes ending with A"
  (5a:is (ends-with-a? "AAA"))
  (5a:is (ends-with-a? "BBA")) 
  (5a:is (ends-with-a? "XYA"))
  (5a:is (not (ends-with-a? "AAB")))
  (5a:is (not (ends-with-a? "ZZZ")))
  (5a:is (not (ends-with-a? "XYZ"))))

(5a:test ends-with-z-test
  "Test that ends-with-z? correctly identifies nodes ending with Z"
  (5a:is (ends-with-z? "AAZ"))
  (5a:is (ends-with-z? "BBZ"))
  (5a:is (ends-with-z? "XYZ"))
  (5a:is (not (ends-with-z? "AAA")))
  (5a:is (not (ends-with-z? "ZZA")))
  (5a:is (not (ends-with-z? "XYB"))))

(defun traverse-from-a-to-z (directions tree node)
  "given a node, walk it through the tree using directions until the node
ends in z, return the number of steps"
  (do
   ;; set up variables
   ((d directions (rest d))             ; loop eternally
    (steps 0 (incf steps)))             ; count the steps

   ;; termination condition, node ends with Z
   ((ends-with-z? node) steps)          ; so return steps

    ;; loop body - walk the tree
    (setf node
          (if (equalp (first d) #\L)         ; lookup next node
              (car (gethash node tree))      ; use left
              (cdr (gethash node tree))))))  ; use right

(5a:test traverse-from-a-to-z-test
  "Test that traverse-from-a-to-z correctly counts steps from A to Z nodes"
  (multiple-value-bind (directions tree) (parse-data *test-data2*)
    (setf (cdr (last directions)) directions) ; make circular
    (5a:is (= (traverse-from-a-to-z directions tree "AAA") 2))
    (5a:is (= (traverse-from-a-to-z directions tree "BBA") 3))))

(defun Day08-2 (los)
  "given a list of strings describing a set of directions and a tree of
nodes to walk return the number of steps it takes to get all nodes
ending with A to end with Z simultaneously"
  ;; parse the data
  (multiple-value-bind (directions tree) (parse-data los)

    ;; set the end of directions to point to the beginning so it's circular
    (setf (cdr (last directions)) directions)

    ;; generate a list of starting nodes (ending in A)
    (let ((node-list (loop for node being the hash-keys in tree
                           when (ends-with-a? node)
                             collect node)))

      ;; walk each node through the tree until it reaches Z
      ;; then LCM the number of steps each took together
      ;; for the result
      (reduce #'lcm (mapcar
                     #'(lambda (n) (traverse-from-a-to-z directions tree n))
                     node-list)))))

(5a:test Day08-2-test
  (5a:is (= (Day08-2 *test-data2*) 6)))

(5a:test single-step-navigation-test
  "Test navigation requiring only one step"
  (let ((single-step-data '("R"
                           ""
                           "AAA = (BBB, ZZZ)"
                           "ZZZ = (ZZZ, ZZZ)")))
    (5a:is (= (Day08-1 single-step-data) 1))))

(defparameter *edge-case-data*
  '("L"
    ""
    "AAA = (ZZZ, BBB)"
    "ZZZ = (ZZZ, ZZZ)")
  "Test data for immediate arrival at destination")

(5a:test edge-case-immediate-arrival-test
  "Test case where we start one step away from ZZZ"
  (5a:is (= (Day08-1 *edge-case-data*) 1)))

(5a:test parse-data-edge-cases-test
  "Test parse-data with minimal valid input"
  (let ((minimal-data '("L" "" "AAA = (BBB, CCC)")))
    (multiple-value-bind (directions tree) (parse-data minimal-data)
      (5a:is (equal directions '(#\L)))
      (5a:is (= (hash-table-count tree) 1))
      (5a:is (equal (gethash "AAA" tree) '("BBB" . "CCC"))))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 08 Part 1 is ~a"
              (day08-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 08 Part 2 is ~a"
              (day08-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------


;; The answer to AOC 2023 Day 08 Part 1 is 19631
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001603 seconds of total run time (0.001517 user, 0.000086 system)
;; 200.00% CPU
;; 262,064 bytes consed

;; The answer to AOC 2023 Day 08 Part 2 is 21003205388413
;; Evaluation took:
;; 0.007 seconds of real time
;; 0.007394 seconds of total run time (0.007330 user, 0.000064 system)
;; 100.00% CPU
;; 327,584 bytes consed
