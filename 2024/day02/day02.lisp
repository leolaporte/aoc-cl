;;;; Day02.lisp
;;;; 2024 AOC Day 02 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 1 Dec 2024 9p
;;;; Finished:1 Dec 2024 10:48p

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day02
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/day02/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 2: Red-Nosed Reports ---
--- Part One ---

"A report only counts as safe if both of the following are true:

The levels are either all increasing or all decreasing.

Any two adjacent levels differ by at least one and at most three.

How many reports are safe?"

LEO's NOTES: Streamed this again...
https://youtu.be/ihzF0jmiBf4?si=zDG8u1yjuTgJAnAb

Nothing much to say here. Read the input into a list of
newline separated strings, then turn each line into a list of
integers (PARSE-REPORTS). Then, check items in the list to see if 1:
all integers are either increasing or decreasing and 2: the difference
between any successive ints is between 1 and 3. I'll create a
predicate function, SAFE-REPORT?, to do the test.

My original SAFE-REPORT worked but was a little convoluted. I
converted it to a simple AND statement with inspiration from ynadji on
reddit. https://www.reddit.com/user/ynadji/ (I had forgotten that I
could use (apply #'> list) to test for an always ascending list!)

The final function will test each report and return the number of safe
reports.
---------------------------------------------------------------------------- |#

(defparameter *example*
  '("7 6 4 2 1"
    "1 2 7 8 9"
    "9 7 6 2 1"
    "1 3 2 4 5"
    "8 6 4 4 1"
    "1 3 6 7 9"))

(defun parse-reports (los)
  "reads a list of strings, each containing a set of number strings,
 and returns a list of integer lists"
  (let ((reports '())) ; start with an empty list
    (dolist (str los) ; for every string in the provided list of string
      (push
       (mapcar #'parse-integer (re:split "\\s+" str)) ; split then convert
       reports)) ; add integer list to reports
    (reverse reports))) ; return the populated list (after reversing)

(defun safe-report? (lon)
  "returns true if the list of numbers is either increasing or decreasing
and always between one and three numbers apart"
  (and (or (apply #'> lon)                        ; always ascending
           (apply #'< lon))                       ; always descending

       (every (lambda (x) (< 0 x 4))              ; differences always in range?
              (iter (for (a b) on lon by #'cdr)   ; step through each pair
                (when b                           ; ignore nil at end
                  (collecting (abs (- a b)))))))) ; list all the differences

(5a:test safe-report?-t
  (let ((rpts (parse-reports *example*)))
    (5a:is-true (safe-report? (first rpts)))
    (5a:is-false (safe-report? (second rpts)))
    (5a:is-false (safe-report? (third rpts)))
    (5a:is-false (safe-report? (fourth rpts)))
    (5a:is-false (safe-report? (fifth rpts)))
    (5a:is-true (safe-report? (sixth rpts)))))

(defun Day02-1 (report)
  (let ((rpts (parse-reports report)))
    (iter (for r in rpts)
      (summing (if (safe-report? r) 1 0)))))

(5a:test Day02-1-test
  (5a:is (= 2 (Day02-1 *example*))))


#| ----------------------------------------------------------------------------
--- Part Two ---

"Now, the same rules apply as before, except if removing a single level
from an unsafe report would make it safe, the report instead counts as
safe."

LEO's NOTES: I can still use the parsing function from part 1 but I'll
have to write a new predicate function, RELAXED-SAFE-REPORT?, to
accomodate the new rule. And, as it turns out, I'll want a function
that can remove a single item from a list by its index into the list,
REMOVE-ITEM index list. The rest is a piece of cake.
---------------------------------------------------------------------------- |#

(defun remove-item (index list)
  "returns a list with the item at index removed"
  (append (subseq list 0 index) (subseq list (1+ index))))

(5a:test remove-item-t
  (5a:is (equalp '(0 1 2 3 4) (remove-item 0 '(0 0 1 2 3 4))))
  (5a:is (equalp '(0 1 2 3 4) (remove-item 1 '(0 0 1 2 3 4))))
  (5a:is (equalp '(0 0 2 3 4) (remove-item 2 '(0 0 1 2 3 4))))
  (5a:is (equalp '(0 0 1 3 4) (remove-item 3 '(0 0 1 2 3 4))))
  (5a:is (equalp '(0 0 1 2 4) (remove-item 4 '(0 0 1 2 3 4))))
  (5a:is (equalp '(0 0 1 2 3) (remove-item 5 '(0 0 1 2 3 4)))))

(defun relaxed-safe-report? (rpt)
  (when (safe-report? rpt)
    (return-from relaxed-safe-report? t))    ; naturally safe - return t

  (iter (for i below (length rpt))           ; how about if we remove one digit?
    (when (safe-report? (remove-item i rpt))
      (return-from relaxed-safe-report? t)))

  nil)                                       ; never safe

(5a:test relaxed-safe-report?-t
  (let ((rpts (parse-reports *example*)))
    (5a:is-true (relaxed-safe-report? (first rpts)))
    (5a:is-false (relaxed-safe-report? (second rpts)))
    (5a:is-false (relaxed-safe-report? (third rpts)))
    (5a:is-true (relaxed-safe-report? (fourth rpts)))
    (5a:is-true (relaxed-safe-report? (fifth rpts)))
    (5a:is-true (relaxed-safe-report? (sixth rpts)))))

(defun Day02-2 (reports)
  (let ((rpts (parse-reports reports)))
    (iter (for r in rpts)
      (summing (if (relaxed-safe-report? r) 1 0)))))

(5a:test Day02-2-test
  (5a:is (= 4 (Day02-2 *example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 02 Part 1 is ~a"
              (Day02-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2024 Day 02 Part 2 is ~a"
              (Day02-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2024 Day 02 Part 1 is 663
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001098 seconds of total run time (0.001034 user, 0.000064 system)
;; 100.00% CPU
;; 786,208 bytes consed

;; The answer to AOC 2024 Day 02 Part 2 is 692
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001249 seconds of total run time (0.001182 user, 0.000067 system)
;; 100.00% CPU
;; 1,048,352 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 2   00:57:52  13855      0   01:48:11  12571      0
;; 1   00:47:09   9351      0   01:10:39   9932      0
