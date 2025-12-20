;;;; Day05.lisp
;;;; 2025 AOC Day 5 solution
;;;; Common Lisp solutions by Leo Laporte
;;;; Started: 04 Dec 2025 at 21:00
;;;; Finished: 05 Dec 2025 at 09:45

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day05
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day05)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day05/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;                           --- Day 5: Cafeteria ---
;;                              --- Part One ---
;;
;; LEO'S NOTES: How many of the numbers in the second section appear in the
;; ranges described in the first section. Chosing the right data format is half
;; the job.
;;
;; The only hitch is that because an ingredient can appear in multiple ranges we
;; have to stop counting matches as soon as the first match shows up in order
;; not to count multiple matches.
;;
;; ----------------------------------------------------------------------------

(defparameter *example* (list "3-5"
                              "10-14"
                              "16-20"
                              "12-18"
                              ""
                              "1"
                              "5"
                              "8"
                              "11"
                              "17"
                              "32"))

(sr:-> parse-input (list) (values list list))
(defun parse-input (input)
  "given a list of strings representing numeric ranges represented as 1-2 and
ingredients represented as a number string return a list of ranges and
ingredients"
  (let ((ranges nil)
        (ingredients nil))

    (iter (for line in input)
      (cond ((re:scan-to-strings "\\d+-\\d+" line)
             (push (mapcar #'parse-integer (sr:words line)) ranges))

            ((re:scan-to-strings "\\d+" line)
             (push (parse-integer line) ingredients))))

    (values ranges ingredients)))

(sr:-> in-range? (number list) boolean)
(defun in-range? (num range)
  "returns t if NUM is in the range, inclusive"
  (<= (first range) num (second range)))

(5a:test in-range?-test
  (5a:is-true (in-range? 10 '(10 15)))
  (5a:is-true (in-range? 14 '(10 15)))
  (5a:is-true (in-range? 15 '(10 15)))
  (5a:is-false (in-range? 9 '(10 15)))
  (5a:is-false (in-range? 16 '(10 15))))

(sr:-> day05-1 (list) number)
(defun day05-1 (input)
  "given a list of ingredients and a list of ranges of fresh ingredients return
the total number of fresh ingredients."
  (let ((fresh 0))
    (multiple-value-bind (ranges ingredients) (parse-input input)
      (iter (for i in ingredients)
        (iter (for r in ranges)
          (when (in-range? i r)
            (incf fresh)
            (finish)))))                ; end inside loop and continue
    fresh))

(5a:test day05-1-test
  (5a:is (= 3 (day05-1 *example*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Two --
;;
;; "How many ingredient IDs are considered to be fresh according to the fresh
;; ingredient ID ranges?"
;;
;; LEO'S NOTES: Wow. Easy but sloooooooow. OK I can't expand the ranges, so I
;; just have to figure out where they overlap then take the length of the
;; remaining ranges.
;;
;; I went to bed with part 2 unfinished but I had a dream in which I explained
;; the solution to someone! The dream wasn't fully accurate but it was very
;; close.
;;
;; The trick is to merge all overlapping ranges then sum the lengths of each
;; range. I wrote OVERLAP? to test whether ranges overlap (which I later
;; realized must include ranges fully within each other). Then MERGE-RANGES
;; which takes overlapping ranges and reduces them to a single range (again
;; accounting for ranges which are totally within another). Both functions
;; require ranges that are sorted - both by start and end. Fortunately the
;; built-in sort is very fast.
;;
;; I discovered the overlapping range issue when the test passed but the final
;; answer was too low. Paul Holder provided a perverse example which
;; demonstrated the issue with ranges within ranges as well as the need to sort
;; by both start and finish. The final function MERGE-ALL-RANGES recursively
;; works through the sorted ranges and merges them down as much as possible.
;;
;; ----------------------------------------------------------------------------

(defparameter *example2* (list "1-100"
                               "1-10"
                               "1-15"
                               "2-8"
                               "2-14"
                               "2-20")
  "a perverse example from Paul Holder with ranges WITHIN ranges and requiring
a secondary sort (first ends, then starts)")

(sr:-> overlap? (list list) boolean)
(defun overlap? (r1 r2)
  "returns t if two number pairs overlap, assumes pairs are sorted
 ascending by their second then first digits"
  (or
   ;; the first range fully engulfs the second
   (and (<= (first r1) (first r2)) (<= (second r2) (second r1)))
   ;; the end of the first is lower than the start of the second
   (>= (second r1) (first r2))))

(5a:test overlap?-test
  (5a:is-true (overlap? '(1 5) '(4 10)))
  (5a:is-true (overlap? '(1 5) '(5 10)))
  (5a:is-false (overlap? '(1 5) '(6 10)))
  (5a:is-true (overlap? '(1 5) '(2 4)))
  (5a:is-true (overlap? '(1 5) '(2 5)))
  (5a:is-true (overlap? '(1 5) '(1 5))))

(sr:-> merge-ranges (list list) list)
(defun merge-ranges (r1 r2)
  "merges two overlapping ranges into a single range that encompasses them both"
  (let ((points (flatten (list r1 r2))))
    (list (apply #'min points)
          (apply #'max points))))

(5a:test merge-ranges-test
  (5a:is (equal (merge-ranges '(1 5) '(5 10)) '(1 10)))
  (5a:is (equal (merge-ranges '(1 6) '(5 10)) '(1 10)))
  (5a:is (equal (merge-ranges '(1 5) '(2 4)) '(1 5))))

(sr:-> merge-all-ranges (list list) list)
(defun merge-all-ranges (ranges merged)
  "takes a list of number pairs, RANGES, and merges the overlapping pairs,
returns MERGE, a list of all pairs reduced to non-overlapping pairs, assumes
 pairs are sorted ascending by the second then first digits"
  (cond ((null (cdr ranges))
         ;; done so push the last range and return
         (reverse (push (first ranges) merged)))

        ((overlap? (first ranges) (first (rest ranges)))
         ;; merge the overlaps then continue with the merged range and the
         ;; rest of the list
         (merge-all-ranges
          (cons (merge-ranges (first ranges) (first (rest ranges)))
                (rest (rest ranges))) merged))

        (t ;; not overlapping so save and continue with rest
         (push (first ranges) merged)
         (merge-all-ranges (rest ranges) merged))))

(5a:test merge-all-ranges-test
  (5a:is (equal (merge-all-ranges (list '(3 5) '(10 14) '(12 18) '(16 20)) '())
                (list '(3 5) '(10 20))))
  (5a:is (equal (merge-all-ranges (list '(3 5) '(10 14) '(12 18) '(19 20)) '())
                (list '(3 5) '(10 18) '(19 20))))
  (5a:is (equal (merge-all-ranges
                 (list '(3 5) '(10 14) '(12 18) '(19 20) '(21 22)) '())
                (list '(3 5) '(10 18) '(19 20) '(21 22))))
  (5a:is (equal (merge-all-ranges
                 (list '(3 5) '(6 14) '(12 18) '(19 20)) '())
                (list '(3 5) '(6 18) '(19 20)))))

(sr:-> day05-2 (list) number)
(defun day05-2 (input)
  "given a list of strings describing a set of ranges, returns the total
 number of items in those ranges"
  (sr:~> (parse-input input)            ; get the ranges as a list of pairs
         (sort _ #'< :key #'second)     ; sort by the end of each pair
         (sort _ #'< :key #'first)      ; then by the start
         (merge-all-ranges _ '())       ; merge overlapping pairs
         (mapcar
          (lambda (x) (1+ (- (second x) (first x)))) _) ; get lengths
         (apply #'+ _)))                                ; add lengths

(5a:test day05-2-test
  (5a:is (= 14 (day05-2 *example*)))
  (5a:is (= 100 (day05-2 *example2*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to AOC 2025 Day 5 Part 1 is ~a"
              (day05-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2025 Day 5 Part 2 is ~a"
              (day05-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 5 Part 1 is 509
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.002734 seconds of total run time (0.002685 user, 0.000049 system)
;; 150.00% CPU
;; 401,680 bytes consed

;; The answer to AOC 2025 Day 5 Part 2 is 336790092076620
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001319 seconds of total run time (0.001294 user, 0.000025 system)
;; 100.00% CPU
;; 393,120 bytes consed
