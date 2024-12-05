;;;; Day05.lisp
;;;; 2024 AOC Day 05 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 4 Dec 2024 0900 Pacific
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day05
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day05)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/Day_05/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 5: Print Queue ---
--- Part One ---

"The first section specifies the page ordering rules, one per line. The new pages for the safety manuals must be printed in a very specific order. The notation X|Y means that if both page number X and page number Y are to be produced as part of an update, page number X must be printed at some point before page number Y.

The second section specifies the page numbers of each update.

To get the printers going as soon as possible, start by identifying which updates are already in the right order.

What do you get if you add up the middle page number from those correctly-ordered updates?"

LEO'S NOTES:

There are two tasks. 1 Identify the correct updates. 2. Add the middle page number from each and return the result.

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
    ""
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"))

(defun extract-numbers (string)
  "takes a string in the form 12|34 and returns a list of the two
 integers"
  (mapcar #'parse-integer (re:split "\\|" string)))

(defun parse-rules-and-updates (los)
  "given a two part list of strings containing rules in the form ##|##
and updates consisting of a list of comma separated numbers, the two
parts separated by an empty line, return a hash table of rules and a
list of updates"
  (let* ((rules (make-hash-table :test 'equal)) ; the rules
         (updates '())                      ; the updates
         (list-break (position "" los :test 'string=))) ; empty line in input

    ;; take the first half of the input and create the rules hash
    (iter (for i below list-break) ; process the first half of the data
      (let* ((nlst (extract-numbers (nth i los))) ; list of integers in string
             (vals (gethash (first nlst) rules))) ; get any existing vals

        ;; the rules hash will have the first number as key and the val
        ;; will be a list of numbers with the same first number
        (setf (gethash (first nlst) rules) (cons (second nlst) vals))))

    ;; take the second half of the input and create the updates list
    (iter (for i from (1+ list-break) below (length los)) ; now the second half
      (push (mapcar #'parse-integer (re:split "," (nth i los))) updates))

    (values rules (reverse updates))))

(defun ph (hash)
  "a little utility to display a hash table"
  (maphash (lambda (key value)
             (format t "~a => ~a~%" key value)) hash))

(defun l2-is-a-member-of-l1? (lst1 lst2)
  "returns true if every number in lst2 is also in lst1"
  (= (length lst2) (length (intersection lst1 lst2 :test 'equal))))

(5a:test l2-is-a-member-of-l1?-t
  (5a:is-true (l2-is-a-member-of-l1? '(1 2 3 4) '(4)))
  (5a:is-false (l2-is-a-member-of-l1? '(1 2 3 4) '(5)))
  (5a:is-true (l2-is-a-member-of-l1? '(1 2 3 4) '(4 3 2)))
  (5a:is-false (l2-is-a-member-of-l1? '(1 2 3 4) '(4 3 2 1 5))))

(defun right-order? (update rules)
  "returns true if the given update is correct according to the rules"
  (cond ((= (length update) 1) t) ; last page, all done!
        (t (and (l2-is-a-member-of-l1? (gethash (first update) rules)
                                       (rest update))
                (right-order? (rest update) rules)))))

(5a:test right-order?-t
  (multiple-value-bind (rules updates) (parse-rules-and-updates *example*)
    (5a:is-true (right-order? (first updates) rules))
    (5a:is-true (right-order? (second updates) rules))
    (5a:is-true (right-order? (third updates) rules))
    (5a:is-false (right-order? (fourth updates) rules))
    (5a:is-false (right-order? (fifth updates) rules))
    (5a:is-false (right-order? (sixth updates) rules))))

(defun update-middle (update)
  "returns the middle value in the update list"
  (let ((len (length update)))
    (nth (floor len 2) update)))

(5a:test update-middle-t
  (5a:is (= 3 (update-middle '(1 2 3 4 5)))))

(defun Day_05-1 (los)
  (multiple-value-bind (rules updates) (parse-rules-and-updates los)
    (iter (for u in updates)
      (summing (if (right-order? u rules) (update-middle u) 0)))))

(5a:test Day_05-1-test
  (5a:is (= 143 (Day_05-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 05 Part 1 is ~a"
	      (day_05-1 (uiop:read-file-lines *data-file*))))

;; (time (format t "The answer to AOC 2024 Day 05 Part 2 is ~a"
;;	      (day_05-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
