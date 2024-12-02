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

(defparameter *data-file* "~/common-lisp/AOC/2024/Day_02/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 2: Red-Nosed Reports ---
--- Part One ---

"A report only counts as safe if both of the following are true:

The levels are either all increasing or all decreasing.

Any two adjacent levels differ by at least one and at most three.

How many reports are safe?"

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
 and returns a list of number lists"
  (let ((reports '()))
    (dolist (str los)
      (push (mapcar #'parse-integer (re:split "\\s+" str)) reports))
    (reverse reports)))

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

(defun Day_02-1 (report)
  (let ((rpts (parse-reports report)))
    (iter (for r in rpts)
      (summing (if (safe-report? r) 1 0)))))

(5a:test Day_02-1-test
  (5a:is (= 2 (Day_02-1 *example*))))


#| ----------------------------------------------------------------------------
--- Part Two ---

"Now, the same rules apply as before, except if removing a single level
from an unsafe report would make it safe, the report instead counts as
safe."


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

(defun Day_02-2 (reports)
  (let ((rpts (parse-reports reports)))
    (iter (for r in rpts)
      (summing (if (relaxed-safe-report? r) 1 0)))))

(5a:test Day_02-2-test
  (5a:is (= 4 (Day_02-2 *example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 02 Part 1 is ~a"
              (day_02-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2024 Day 02 Part 2 is ~a"
              (day_02-2 (uiop:read-file-lines *data-file*))))

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
