;;;; Day02.lisp
;;;; 2025 AOC Day 2 solution
;;;; Common Lisp solutions by Leo Laporte
;;;; Started: 01 Dec 2025 at 21:00
;;;; Finished: 01 Dec 2025 at 22:40

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day02
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day02/input.txt"
  "Downloaded from the AoC problem set")

;; ----------------------------------------------------------------------------
;;
;;                              --- Part One ---
;;
;; LEO'S NOTES: We're given a string with a list of number
;; ranges. We're to count the invalid ids in those ranges. An ID is
;; invalid if it is "made only of some sequence of digits repeated
;; twice." Sounds like a job for regex. A doubled sequence would look
;; like this: ^(\\d+)\\1$
;; ----------------------------------------------------------------------------

(Defparameter *example* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659, 824824821-824824827,2121212118-2121212124")

(defparameter *twice-repeated-sequence* (re:create-scanner "^(\\d+)\\1$")
  "a regex for finding numbers consisting only of a twice repeating digit
sequence e.g. 11 234234 56785678")

(sr:-> parse-input (string) list)
(defun parse-input (input)
  "given a string containing comma separated number ranges, return a
 list of integer pairs representing the start and end of a number
 range"
  (let ((digits (mapcar #'parse-integer (sr:words input))))

    (iter (for (x y) on digits by #'cddr)
      (collect (list x y)))))

(sr:-> valid-id? (number function) boolean)
(defun valid-id? (n regex)
  "returns t if an id is valid (has no repeating digit sequences using
the specified regex)"
  (zerop (re:count-matches regex (write-to-string n))))

(5a:test invalid-id?-test
  (5a:is-true (valid-id? 123456 *twice-repeated-sequence*))
  (5a:is-false (valid-id? 38593859 *twice-repeated-sequence*))
  (5a:is-false (valid-id? 1111 *twice-repeated-sequence*))
  (5a:is-false (valid-id? 446446 *twice-repeated-sequence*))
  (5a:is-true (valid-id? 4464467 *twice-repeated-sequence*)))

(sr:-> sum-invalid-ids (number number function) number)
(defun sum-invalid-ids (start end regex)
  "given a range from start to end inclusive, count the number of invalid
IDs in the range"
  (iter (for n from start to end)
    (summing
      (if (not (valid-id? n regex)) n 0))))

(5a:test count-invalid-ids-test
  (5a:is (= 33 (sum-invalid-ids 11 22 *twice-repeated-sequence*)))
  (5a:is (= 99 (sum-invalid-ids 95 115 *twice-repeated-sequence*)))
  (5a:is (= 1188511885
            (sum-invalid-ids 1188511880 1188511890 *twice-repeated-sequence*)))
  (5a:is (= 0 (sum-invalid-ids 1698522 1698528 *twice-repeated-sequence*))))

(sr:-> day02-1 (string function) number)
(defun day02-1 (input regex)
  "count the number of invalid ids in the list of ID ranges provided by input"
  (let ((ranges (parse-input input)))

    (iter (for range in ranges)
      (summing (sum-invalid-ids (first range) (second range) regex)))))

(5a:test day02-1-test
  (5a:is (= 1227775554 (day02-1 *example* *twice-repeated-sequence*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Two --
;;
;; LEO'S NOTES: Just change the regex to "^(\\d+)\\1+$ (adding the
;; plus after the 1) - I'll refactor part one to allow different
;; regexes and we're done.
;;
;; ----------------------------------------------------------------------------

;; The modified regex (I use create-scanner to optimize the regex scan)
(defparameter *multiple-repeated-sequence* (re:create-scanner "^(\\d+)\\1+$")
  "a regex for finding numbers consisting only of a repeating
digit sequence e.g. 1111 234234234234 56785678567856785678")

(5a:test day02-1-test
  (5a:is (= 4174379265 (day02-1 *example* *multiple-repeated-sequence*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to AOC 2025 Day 2 Part 1 is ~a"
              (day02-1 (read-file-into-string *data-file*)
                       *twice-repeated-sequence*)))

(time (format t "The answer to AOC 2025 Day 2 Part 2 is ~a"
              (day02-1 (read-file-into-string *data-file*)
                       *multiple-repeated-sequence*)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 2 Part 1 is 23560874270
;; Evaluation took:
;; 0.393 seconds of real time
;; 0.394625 seconds of total run time (0.392416 user, 0.002209 system)
;; [ Real times consist of 0.007 seconds GC time, and 0.386 seconds non-GC time. ]
;; [ Run times consist of 0.007 seconds GC time, and 0.388 seconds non-GC time. ]
;; 100.51% CPU
;; 286,780,400 bytes consed

;; The answer to AOC 2025 Day 2 Part 2 is 44143124633
;; Evaluation took:
;; 0.404 seconds of real time
;; 0.406438 seconds of total run time (0.403048 user, 0.003390 system)
;; [ Real times consist of 0.009 seconds GC time, and 0.395 seconds non-GC time. ]
;; [ Run times consist of 0.009 seconds GC time, and 0.398 seconds non-GC time. ]
;; 100.50% CPU
;; 350,507,072 bytes consed
