;;;; Day07.lisp
;;;; 2015 AOC Day 7 solution
;;;; Leo Laporte, 4 Nov 2022

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage day07
  (:use #:cl
	#:fiveam         ; testing
	#:cl-ppcre))     ; regex

(in-package :day07)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))

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

NOTES: This might be the first example of Eric leaving a clue on how
NOT to solve this: "If, FOR SOME REASON, you'd like
to emulate the circuit..."So maybe I don't need to emulate
the whole circuit?

In the coming years there are many problems
that invite "oversolving" and I've learned it's best to focus
on the one thing you need to know. In this case, what is the
value of a.

This is a very natural recursion problem. I'll attack this backward
from a. Building a tree of operations that gets us to a from some
integer value or values. I can ignore the intermediate values
(the wire names) and just keep track of the operations, working
backward until I get to an integer value. Then reverse direction
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
  "precompiled regex to build a hash of signal -> wire")

(defparameter *numeric* (create-scanner "^-?[0-9]+$")
  "precompiled regex to find number strings")

(defparameter *wire* (create-scanner "^[a-z]+$")
  "a wire - one or two lower case letters alone")

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

(defun wirep (s)
  "returns true if a string is a wire (one or two lower case letters)"
  (scan *wire* s))

;; UGH my first attempt crashes Emacs - the full tree is too huge (sub trees
;; like "s" work fine so it's not the logic). First I'll try memoizaion using
;; DEFCACHED just to speed it up.

;; ok using DEFCACHED does make a big difference in speed and memory
;; So it works. But the tree is still too damn big. It's crashing on the full
;; data set.

;; next step - try building the expression in chunks. Whenever I can I'll solve the
;; signal chain and store the numeric value on that wire (it's basically manual
;; memoization). There will be recursion but it will be limited and the expression
;; will reduce whenever it's able. With luck the tree will never get too big for
;; memory. (That worked and it's really fast!)

(defun parse-signal-hash (wire signal-hash)
  "parses the hash starting with wire - returns the final value of the given wire"

  (when (stringp (gethash wire signal-hash)) ;; if it's an INTEGER we're done
    (let ((op (uiop:split-string (gethash wire signal-hash))))
      ;; an OP is a list of strings with a length of
      ;; 1 - it has to be either a number or a wire (as a string)
      ;; 2 - it has to be a NOT
      ;; 3 - it can be an LSHIFT, RSHIFT, AND, or OR
      ;;
      ;; the elements of an OP can be a number-, wire-, or command-string
      ;; in each cond I do more tests than logically necessary just to catch
      ;; any errors

      (cond
	;; it's a number string, convert to INT, store it, and end recursion
	((and (= (length op) 1) (number-str-p (first op)))
	 (setf (gethash wire signal-hash)
	       (parse-integer (first op))))

	;; it's a wire, recurse down the wire
	((and (= (length op) 1) (wirep (first op)))
	 (setf (gethash wire signal-hash)
	       (parse-signal-hash (first op) signal-hash)))

	;; it's a NOT, so check the operand and either perform a lognot or recurse
	((and (= (length op) 2) (equalp "NOT" (first op)))
	 (setf (gethash wire signal-hash)
	       (lognot
		(cond ((number-str-p (second op))
		       (parse-integer (second op))) ; convert to int
		      ((wirep (second op))
		       (parse-signal-hash (second op) signal-hash)) ; reduce
		      (t (error "unknown op ~a in NOT" op))))))

	;; it's an RSHIFT
	((and (= (length op) 3)
	      (equalp "RSHIFT" (second op))
	      (number-str-p (third op)))
	 (setf (gethash wire signal-hash)
	       (ash
		(cond ((number-str-p (first op))
		       (parse-integer (first op)))
		      ((wirep (first op))
		       (parse-signal-hash (first op) signal-hash)) ; recurse
		      (t (error "unknown op ~a on RSHIFT" op)))
		(-(parse-integer (third op))))))

	;; it's an LSHIFT
	((and (= (length op) 3)
	      (equalp "LSHIFT" (second op))
	      (number-str-p (third op)))
	 (setf (gethash wire signal-hash)
	       (ash
		(cond ((number-str-p (first op))
		       (parse-integer (first op)))
		      ((wirep (first op))
		       (parse-signal-hash (first op) signal-hash)) ;  recurse
		      (t (error "unknown op ~a on LSHIFT" op)))
		(parse-integer (third op)))))

	;; it's an OR
	((and (= (length op) 3) (equalp "OR" (second op)))
	 (setf (gethash wire signal-hash)
	       (logior
		(cond  ((number-str-p (first op)) (parse-integer (first op)))
		       ((wirep (first op)) (parse-signal-hash (first op) signal-hash))
		       (t (error "unknown op ~a on OR" op)))

		(cond  ((number-str-p (third op)) (parse-integer (third op)))
		       ((wirep (third op)) (parse-signal-hash (third op) signal-hash))
		       (t (error "unknown op ~a on OR" op))))))

	;; it's an AND
	((and (= (length op) 3) (equalp "AND" (second op)))
	 (setf (gethash wire signal-hash)
	       (logand
		(cond  ((number-str-p (first op)) (parse-integer (first op)))
		       ((wirep (first op)) (parse-signal-hash (first op) signal-hash))
		       (t (error "unknown op ~a on AND" op)))

		(cond  ((number-str-p (third op)) (parse-integer (third op)))
		       ((wirep (third op)) (parse-signal-hash (third op) signal-hash))
		       (t (error "unknown op ~a on AND" op))))))

	(t (error "Malformed entry ~a" op)))))
  (gethash wire signal-hash)) ; return the final INTEGER value of specified wire

(test psh-test
  ;; first some simple one and two branch trees
  (is (= 30 (parse-signal-hash "a" (make-signal-hash (list "10 OR 20 -> a")))))
  (is (= 0 (parse-signal-hash "a" (make-signal-hash (list "10 AND 20 -> a")))))
  (is (= 30 (parse-signal-hash "a" (make-signal-hash (list "20 OR b -> a" "10 -> b")))))
  (is (= 0 (parse-signal-hash "a" (make-signal-hash (list "20 AND b -> a" "10 -> b")))))
  (is (= 0 (parse-signal-hash "a" (make-signal-hash (list "NOT -1 -> a")))))
  (is (= -11 (parse-signal-hash "a" (make-signal-hash (list "NOT b -> a" "10 -> b")))))
  (is (= 5 (parse-signal-hash "a" (make-signal-hash (list "b RSHIFT 2 -> a" "20 -> b")))))
  (is (= 80 (parse-signal-hash "a" (make-signal-hash (list "b LSHIFT 2 -> a" "20 -> b")))))
  (is (= 20 (parse-signal-hash "a" (make-signal-hash (list "20 -> a")))))
  (is (= 10 (parse-signal-hash "a" (make-signal-hash (list "b -> a" "10 -> b")))))

  ;; now using subsets of the provided data
  (is (= -515 (parse-signal-hash "q" (make-signal-hash *aoc-data*))))
  (is (= 0 (parse-signal-hash "s" (make-signal-hash *aoc-data*))))
  (is (= 14146 (parse-signal-hash "b" (make-signal-hash *aoc-data*))))
  (is (= -171 (parse-signal-hash "bx" (make-signal-hash *aoc-data*))))

  ;; finally from my notes above
  (let ((test-hash
	  (make-signal-hash
	   (list "20 -> a" "40 -> b" "60 -> c" "a RSHIFT 1 -> d" "NOT b -> e" "c RSHIFT 1 -> f" "d LSHIFT 1 -> g" "e OR f -> h" "g AND h -> i"))))
    (is (= 20  (parse-signal-hash "i" test-hash)))))

(defun day07-1 (wire signals)
  (parse-signal-hash wire (make-signal-hash signals)))

#|

--- Part Two ---

Now, take the signal you got on wire a, override wire b to that signal, and reset the other wires (including wire a). What new signal is ultimately provided to wire a?

|#

(defun day07-2 (wire signals)
  (let* ((new-hash (make-signal-hash signals))
	 (previous-result (day07-1 "a" *aoc-data*)))
    (setf (gethash "b" new-hash) previous-result)
    (parse-signal-hash wire new-hash)))

(time (format t "The answer to AOC 2015 Day 07 Part 1 is ~a" (day07-1 "a" *aoc-data*)))
(time (format t "The answer to AOC 2015 Day 07 Part 2 is ~a" (day07-2 "a" *aoc-data*)))

;; Timings from M2 Macbook Air with 24GB RAM

;; The answer to AOC 2015 Day 07 Part 1 is 956
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000335 seconds of total run time (0.000310 user, 0.000025 system)
;; 100.00% CPU
;; 131,056 bytes consed

;; The answer to AOC 2015 Day 07 Part 2 is 40149
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000667 seconds of total run time (0.000615 user, 0.000052 system)
;; 100.00% CPU
;; 261,600 bytes consed
