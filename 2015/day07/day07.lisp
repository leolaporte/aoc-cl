;;;; Day07.lisp
;;;; 2015 AOC Day 7 solution
;;;; Leo Laporte, Oct 2022

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage day07
  (:use #:cl
	#:fiveam     ; testing
	#:cl-ppcre)) ; regex

(in-package :day07)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---

Each wire has an identifier (some lowercase letters) and can
carry a 16-bit signal (a number from 0 to 65535). A signal
is provided to each wire by a gate, another wire, or some
specific value. Each wire can only get a signal from one
source, but can provide its signal to multiple destinations.
A gate provides no signal until all of its inputs have a signal.

Gates (and their lisp equivalents)
AND -> logand
OR  -> logior
NOT -> lognot
LSHIFT -> ash n n
RSHIFT -> ash n -n

what signal is ultimately provided to wire a?

NOTES: This might be the first time ever Eric has hinted how
NOT to solve this in the clue. "If, FOR SOME REASON, you'd like
to emulate the circuit..." So maybe I don't need to emulate
the whole circuit? In the coming years there are many problems
that invite "oversolving" and I've learned it's best to focus
on the one thing you need to know. In this case, what is the
value of a. So I think the way to solve this is to work backward
from a. Building a tree of operations that gets us to a from some
integer value or values. I can ignore the intermediate values
(the wire names) and just keep track of the operations, working
backwards until I get to an integer value. Then reverse direction
to calculate a. Seems like I'm going to be building a tree beginning
with an integer value or values and leading to the node a. The tree
can consist of operations only,

e.g.

(list "20 -> a" "40 -> b" "60 -> c" "a RHIFT 1 -> d" "NOT b -> e" "c RSHIFT 1 -> f"
"d LSHIFT 1 -> g" "e OR f -> h" "g AND h -> i")

forms the tree:

i ................................AND
................................../ \
g h ........................LSHIFT  OR
............................./      /\
d e f ....................RSHIFT  NOT RSHIFT
.........................../      /    \
a b c ....................20     40     60

Which translates into (note we throw out intermediate wire names):

(eval (logand (ash (ash 20 -1) 1) (logior (ash 60 -1) (lognot 40))))
--> 20

So to start, I'll create a hash table of all the wires and the
calculations that lead to them.
|#

(defparameter *aoc-data* (uiop:read-file-lines "~/cl/AOC/2015/day07/input.txt")
  "the problem data set as a list of strings")

;; pre-compile some regexp for efficiency
(defparameter *command-regex* (create-scanner "(.+) -> (.+)")
  "precompiled regex to build a hash - signal -> wire")

(defparameter *numeric* (create-scanner "^-?[0-9]+$")
  "precompiled regex to find number strings")

(defparameter *wire* (create-scanner"^[a-z]+$")
  "precompiled regex for wires")

(defun make-signal-hash (signals)
  "takes a list of wires and signals and returns a hash keyed
 on wire names"
  (let ((signal-hash
	  (make-hash-table :test 'equal :size (length signals))))
    (mapcar #'(lambda (x) (register-groups-bind (op wire)
			      (*command-regex* x)
			    (setf (gethash wire signal-hash) op)))
	    signals)
    ;; don't care about the list created by mapcar - just the hash
    signal-hash))

(defun print-signal-hash (h)
  "utility function to print the hash key value pairs"
  (maphash #'(lambda (key val) (format t "~a: ~a~&" key val)) h))

(defun number-str-p (s)
  "returns true if a string contains a number"
  (scan *numeric* s))

(defun wire-p (s)
  "returns true if a string is a wire"
  (scan *wire* s))

(defun build-op-tree (wire signal-hash)
  "builds an expression starting with the given wire and using the operations from the signal-hash"
  (let ((op (uiop:split-string (gethash wire signal-hash)))) ; turn string into a list of strings

    (cond
      ;; the signal is a bare number (a leaf node) return it alone
      ((and (= (length op) 1) (number-str-p (first op)))
       (parse-integer (first op))) ; return the number and end recursion on this branch

      ;; it's just a wire, recurse on the wire
      ((= (length op) 1)
       (build-op-tree (first op) signal-hash))

      ;; it's a NOT, cons lognot onto the recursion, recurse on the wire
      ((= (length op) 2)
       (list 'lognot
	     (if (number-str-p (second op))
		 (parse-integer (second op))
		 (build-op-tree (second op) signal-hash))))

      ;; it's an RSHIFT, cons RSHIFT and recurse
      ((equalp "RSHIFT" (second op))
       (if (number-str-p (first op)) ; reduces to a bare number so don't recurse
	   (list 'ash (parse-integer (first op)) (-(parse-integer (third op))))
	   (cons 'ash (list (build-op-tree (first op) signal-hash) (-(parse-integer (third op)))))))

      ;; it's an LSHIFT
      ((equalp "LSHIFT" (second op))
       (if (number-str-p (first op))
	   (list 'ash (parse-integer (first op)) (parse-integer (third op)))
	   (cons 'ash (list (build-op-tree (first op) signal-hash) (parse-integer (third op))))))

      ;; it's an OR, cons logior and recurse both operands
      ;; note - either operand can be just a number
      ((equalp "OR" (second op))
       (cons 'logior
	     (list (if (number-str-p (first op)) ; is it a bare number?
		       (parse-integer (first op))
		       (build-op-tree (first op) signal-hash))
		   (if (number-str-p (third op))
		       (parse-integer (third op))
		       (build-op-tree (third op) signal-hash)))))

      ;; it's an AND, cons logand and recurse both operands
      ((equalp "AND" (second op))
       (cons 'logand
	     (list (if (number-str-p (first op)) ; is it a bare number?
		       (parse-integer (first op))
		       (build-op-tree (first op) signal-hash))
		   (if (number-str-p (third op))
		       (parse-integer (third op))
		       (build-op-tree (third op) signal-hash)))))

      (t (error "Malformed entry ~a" op)))))

(test build-op-tree-test
  (is (= 30 (eval (build-op-tree "a" (make-signal-hash (list "10 OR 20 -> a"))))))
  (is (= 0 (eval (build-op-tree "a" (make-signal-hash (list "10 AND 20 -> a"))))))
  (is (= 30 (eval (build-op-tree "a" (make-signal-hash (list "20 OR b -> a" "10 -> b"))))))
  (is (= 0 (eval (build-op-tree "a" (make-signal-hash (list "20 AND b -> a" "10 -> b"))))))
  (is (= 0 (eval (build-op-tree "a" (make-signal-hash (list "NOT -1 -> a"))))))
  (is (= -11 (eval (build-op-tree "a" (make-signal-hash (list "NOT b -> a" "10 -> b"))))))
  (is (= 5 (eval (build-op-tree "a" (make-signal-hash (list "b RSHIFT 2 -> a" "20 -> b"))))))
  (is (= 80 (eval (build-op-tree "a" (make-signal-hash (list "b LSHIFT 2 -> a" "20 -> b"))))))
  (is (= 20 (eval (build-op-tree "a" (make-signal-hash (list "20 -> a"))))))
  (is (= 10 (eval (build-op-tree "a" (make-signal-hash (list "b -> a" "10 -> b"))))))

  (let ((test-hash  ;; from my notes above
	  (make-signal-hash
	   (list "20 -> a" "40 -> b" "60 -> c" "a RSHIFT 1 -> d" "NOT b -> e" "c RSHIFT 1 -> f" "d LSHIFT 1 -> g" "e OR f -> h" "g AND h -> i"))))
    (is (= 20 (eval (build-op-tree "i" test-hash))))))

(defun day07-1 (wire input-data)
  "given a list of circuit wirings and a  wire return the value of the wire"
  (eval (build-op-tree wire (make-signal-hash input-data))))

#|
--- Part Two ---

|#




;; (time (for-mat t "The answer to AOC 2015 Day 07 Part 1 is ~a" (day07-1 "a" *aoc-data*)))
;; (time (format t "The answer to AOC 2015 Day 07 Part 2 is ~a" (day07-2 *aoc-data*)))
