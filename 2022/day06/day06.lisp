;;;; Day06.lisp
;;;; 2022 AOC Day 06 solution
;;;; Leo Laporte, 6 Dec 2022

;; Prologue code for setup - same every day
(ql:quickload '(:fiveam :cl-ppcre :str))

(defpackage :day06
  (:use #:cl
	#:fiveam       ; for inline testing
	#:cl-ppcre))   ; regex

  (in-package :day06)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day06/input.txt")  ; supplied data from AoC

#|

--- Day 6: Tuning Trouble ---

--- Part One ---

The device will send your subroutine a datastream buffer (your puzzle input); your
subroutine needs to identify the first position where the four most recently received
characters were all different. Specifically, it needs to report the number of characters
from the beginning of the buffer to the end of the first such four-character marker.

How many characters need to be processed before the first start-of-packet marker is detected?

NOTES: Say again? Oh, find the first group of four characters without any duplicated chars.
Well that seems awfully easy.
|#

(defparameter *tst0* "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(defparameter *tst1* "bvwbjplbgvbhsrlpgdmjqwftvncz") ;  first marker after character 5
(defparameter *tst2* "nppdvjthqldpwncqszvftbrmjlhg") ; first marker after character 6
(defparameter *tst3* "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ; first marker after character 10
(defparameter *tst4* "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") ; first marker after character 11


(defun duped? (s)
  "returns true if any characters in string s are duplicated - iterative version"
  (do* ((l (coerce s 'list))      ; easier to do this with a list
	(c l (rest c)))           ; the character in the list to check
       ((null c) nil)             ; never found a dupe? False
    (when (find (first c) (remove (first c) l :count 1)) ; remove one occurrence of the char first
      (return t))))              ; found a dupe!

(defun recursive-duped? (s)
  "returns true if any characters in string s are duplicated - recursive version"
  (let ((list-from-string (coerce s 'list)))
    (labels ((walk-list (c lst)  ; internal recursive function to walk through the list
	       (cond ((null c) nil)  ; walked entire list with no dupes, return false
		     ((find (first c) (remove (first c) lst :count 1)) t) ; duped? return t
		     (t (walk-list (rest c) lst)))))                      ; nope, keep going
      (walk-list list-from-string list-from-string))))   ; trampoline into recursion

(test duped?-test
  (is-true (duped? "abcb"))
  (is-false (duped? "abcd"))
  (is-true (recursive-duped? "fgfe"))
  (is-false (recursive-duped? "fghi")))

(defun find-marker (s)
  "returns the total number of characters that have to be read in string s to find the
first group of four letters that aren't duplicated in the group"
  (dotimes (i (length s))        ; step through the string
    (when (not (recursive-duped? (subseq s i (+ i 4)))) ; taking a four character chunk at a time
      (return (+ 4 i)))))        ; total characters read

(test find-marker-test
  (is (= 7 (find-marker *tst0*)))
  (is (= 5 (find-marker *tst1*)))
  (is (= 6 (find-marker *tst2*)))
  (is (= 10 (find-marker *tst3*)))
  (is (= 11 (find-marker *tst4*))))

(defun day06-1 (f)
  (find-marker (uiop:read-file-line f)))  ; read this one as a single string

#|
--- Part Two ---

A start-of-message marker is just like a start-of-packet marker, except it consists
of 14 distinct characters rather than 4.

How many characters need to be processed before the first start-of-message marker
is detected?

|#

(defun find-message (s)
  "returns the total number of characters that have to be read in string s to find the
first group of fourteen letters that aren't duplicated in the group"
  (dotimes (i (length s))
    (when (not (recursive-duped? (subseq s i (+ i 14)))) ; same thing, bigger chunk
      (return (+ 14 i)))))                       ; don't forget to add the message length

(test find-message-test
  (is (= 19 (find-message *tst0*)))
  (is (= 23 (find-message *tst1*)))
  (is (= 23 (find-message *tst2*)))
  (is (= 29 (find-message *tst3*)))
  (is (= 26 (find-message *tst4*))))


(defun day06-2 (f)
  (find-message (uiop:read-file-line f)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 06 Part 1 is ~a"
	      (day06-1 *data-file*)))

(time (format t "The answer to AOC 2022 Day 06 Part 2 is ~a"
	      (day06-2 *data-file*)))

;; --------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; --------------------------------------------------------------------------------

;; The answer to AOC 2022 Day 06 Part 1 is 1833
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000330 seconds of total run time (0.000256 user, 0.000074 system)
;; 100.00% CPU
;; 260,864 bytes consed

;; The answer to AOC 2022 Day 06 Part 2 is 3425
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000974 seconds of total run time (0.000782 user, 0.000192 system)
;; 100.00% CPU
;; 1,107,376 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 6   01:02:38  19233      0   01:07:16  18804      0
;; 5   03:01:38  23370      0   03:55:49  26420      0
;; 4   01:01:11  15964      0   01:16:38  16172      0
;; 3   00:42:32  12585      0   01:17:33  13957      0
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
