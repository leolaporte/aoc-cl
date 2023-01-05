;;;; Day13.lisp
;;;; 2022 AOC Day 13 solution
;;;; Leo Laporte, 5 Jan 2023

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------
(ql:quickload '(:fiveam))

(defpackage :day13
  (:use #:cl)
  (:local-nicknames
   (:5a :fiveam)))

(in-package :day13)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging

(defparameter *data-file* "~/cl/AOC/2022/day13/input.txt")  ; supplied data from AoC

#| -----------------------------------------------------------------------------

--- Day 13: Distress Signal ---

--- Part One ---

Rules:

1. If both values are integers, the lower integer should come first. If the left integer
is lower than the right integer, the inputs are in the right order. If the left integer
is higher than the right integer, the inputs are not in the right order. Otherwise, the
inputs are the same integer; continue checking the next part of the input.

2. If both values are lists, compare the first value of each list, then the second value,
and so on. If the left list runs out of items first, the inputs are in the right order. If
the right list runs out of items first, the inputs are not in the right order. If the lists
are the same length and no comparison makes a decision about the order, continue checking
the next part of the input.

3. If exactly one value is an integer, convert the integer to a list which contains that
integer as its only value, then retry the comparison. For example, if comparing [0,0,0]
and 2, convert the right value to [2] (a list containing 2); the result is then found by
instead comparing [0,0,0] and [2].

What are the indices of the pairs that are already in the right order? (The first pair has
index 1, the second pair has index 2, and so on.) In the above example, the pairs in the
right order are 1, 2, 4, and 6; the sum of these indices is 13.

Determine which pairs of packets are already in the right order. What is the sum of the
indices of those pairs?
--------------------------------------------------------------------------------------- |#

(defparameter *tst*
  '("[1,1,3,1,1]"
    "[1,1,5,1,1]"
    ""
    "[[1],[2,3,4]]"
    "[[1],4]"
    ""
    "[9]"
    "[[8,7,6]]"
    ""
    "[[4,4],4,4]"
    "[[4,4],4,4,4]"
    ""
    "[7,7,7,7]"
    "[7,7,7]"
    ""
    "[]"
    "[3]"
    ""
    "[[[]]]"
    "[[]]"
    ""
    "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

;; NOTES These look a lot like lisp lists. So first, convert them into lists then
;; work through each pair comparing until the order becomes apparent. One
;; little thing: the comparison terminates as soon as the order is determined so
;; we'll have to break out of the loop then.
;;
;; ASSUME: there's always an answer for any pair and all brackets are balanced.

(defun lispify (str)
  "given a string with [] and , turn it into a proper lisp s-expression"
  (read-from-string  ; magic incantation that turns a string into a proper s-expression
   (substitute #\space #\,
	       (substitute #\( #\[
			   (substitute #\) #\] str)))))

(defun lispify-input (los)
  "given a list of string pairs separated by a blank line, return a list of list pairs"
  (cond ((null los) '())
	((equalp (first los) "") (lispify-input (rest los)))  ; skip blank line
	(t (cons (list (lispify (first los))
		       (lispify (second los)))
		 (lispify-input (rest (rest los)))))))

;; the main workhorse - translate the (somewhat weird) rules into code
(defun check-order (left right)
  "compares two elements, returns :less :more :equal depending on the rules"
  (cond
    ((and (numberp left) (numberp right))                       ; rule 1 - conpare integers
     (cond ((= left right) :equal)                              ; equal - recurse (see below)
	   ((< left right) :less)                               ; in order
	   (t :more)))                                          ; out of order

    ((and (listp left) (listp right))
     (cond ((and (null left) (null right)) :equal)              ; rule 2
	   ((null left) :less)                                  ; left ran out first
	   ((null right) :more)                                 ; right ran out first
	   (t (let ((res (check-order (car left) (car right)))) ; otherwise work through the list
		(if (equal res :equal)
		    (check-order (cdr left) (cdr right))        ; recurse if =
		    res)))))                                    ; otherwise, we have an order

    ((numberp right)                                            ; rule 3
     (check-order left (list right)))                           ; wrap and recurse

    ((numberp left)
     (check-order (list left) right))))

(5a:test check-order-test
  (5a:is (equal :less
		(check-order (lispify "[1,1,3,1,1]")
			     (lispify "[1,1,5,1,1]"))))

  (5a:is (equal :less
		(check-order (lispify "[[1],[2,3,4]]")
			     (lispify "[[1],4]"))))
  (5a:is (equal :more
		(check-order (lispify "[9]")
			     (lispify "[[8,7,6]]"))))

  (5a:is (equal :less
		(check-order (lispify "[[4,4],4,4]")
			     (lispify "[[4,4],4,4,4]"))))

  (5a:is (equal :more
		(check-order (lispify "[7,7,7,7]")
			     (lispify  "[7,7,7]"))))

  (5a:is (equal :less
		(check-order (lispify "[]")
			     (lispify "[3]"))))

  (5a:is (equal :more
		(check-order (lispify "[[[]]]")
			     (lispify "[[]]"))))

  (5a:is (equal :more
		(check-order (lispify "[1,[2,[3,[4,[5,6,7]]]],8,9]")
			     (lispify "[1,[2,[3,[4,[5,6,0]]]],8,9]"))))

  ;; some additional tricky strings from Reddit
  (5a:is (equal :more
		(check-order (lispify "[[8,[[7]]]]")
			     (lispify "[[[[8]]]]"))))

  (5a:is (equal :less
		(check-order (lispify "[[], [[], 0, 2], [9, []]]")
			     (lispify "[[0, []]]"))))

  (5a:is (equal :more
		(check-order (lispify "[[1],[2,3,4]]")
			     (lispify "[[1],2,3,4]")))))

(defun day13-1 (los)
  (let ((pairs (lispify-input los)))
    (loop for i below (length pairs)
	  summing
	  (if (equal :less (check-order (first (elt pairs i)) (second (elt pairs i))))
	      (1+ i)
	      0))))

(5a:test day13-1-test
  (5a:is (= 13 (day13-1 *tst*))))

#| ----------------------------------------------------------------------------------------------------
--- Part Two ---

The distress signal protocol also requires that you include two additional divider packets:

[[2]]
[[6]]

Using the same rules as before, organize all packets (plus dividers) into the correct order.
Then locate the indices of the  divider packets and multiply them together.
---------------------------------------------------------------------------------------------------- |#

(defparameter *div1* '((2)))
(defparameter *div2* '((6)))

(defun packet< (x y)
  "returns true if packet x is less than packet y"
  (equal :less (check-order x y)))

(defun sort-packets (packet-strings)
  "Lispifies and sorts the strings in los (plus divider packets) according to packet order"
  (sort
   (append (list *div1* *div2*)                                              ; add dividers
	   (mapcar #'lispify                                                 ; to lispified
		   (remove-if #'(lambda (x) (equal x "")) packet-strings)))  ; list of packets
   #'packet<))                                                               ; sort using check-order

(defun index (lst item)
  "returns the index number of the first instance of item in the list, nil if not found"
  (dotimes (i (length lst))
    (when (equal item (elt lst i))
      (return i))))

(defun day13-2 (los)
  (let ((sorted (sort-packets los)))
    (* (+ (index sorted *div1*) 1) (+ (index sorted *div2*) 1))))

(5a:test day13-2-test
  (5a:is (= 140 (day13-2 *tst*))))


;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 13 Part 1 is ~a"
	      (day13-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2022 Day 13 Part 2 is ~a"
	      (day13-2  (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM (with  (speed 3) (safety 0) (debug 0)
;; ----------------------------------------------------------------------------------------------------

;; The answer to AOC 2022 Day 13 Part 1 is 5208
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.001893 seconds of total run time (0.001730 user, 0.000163 system)
;; 100.00% CPU
;; 586,992 bytes consed

;; The answer to AOC 2022 Day 13 Part 2 is 25792
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001871 seconds of total run time (0.001750 user, 0.000121 system)
;; 200.00% CPU
;; 521,296 bytes consed
