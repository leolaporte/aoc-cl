;;;; Day03.lisp
;;;; 2024 AOC Day 03 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 2 Dec 2024 21:00
;;;; Finished: 2 Dec 2024 21:58

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day03
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day03)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/day03/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 3: Mull It Over ---
--- Part One ---

"It seems like the goal of the program is just to multiply some numbers.
It does that with instructions like mul(X,Y), where X and Y are each
1-3 digit numbers.

However, because the program's memory has been corrupted, there are
also many invalid characters that should be ignored, even if they look
like part of a mul instruction. Sequences like mul(4*, mul(6,9!,
?(12,34), or mul ( 2 , 4 ) do nothing.

What do you get if you add up all of the results of the
multiplications?"

LEO NOTES: We streamed this one again at
https://youtube.com/@leolaporte - thanks to Paul Holder and Cyphase
who kibbitzed and provided some very valuable direction as I worked.

Looks like a regexp for mul(#,#) will do the job - seems pretty
straightforward. I did have to mess with the parsing of the input
which was provided as a list of five strings intended to be
interpreted as one. Using the regex modifier (?s) ignores the
newlines. So if I use (uiop:read-file-string) to read in the input
then parse it ignoring the newlines all should be well.

---------------------------------------------------------------------------- |#

(defparameter *example* "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defparameter *valid* (re:create-scanner "(?s)mul\\(\\d+,\\d+\\)")) ; the regex

(defun extract-proper-instructions (str)
  "given a string with some proper instructions and some corrupt
instructions, return a list of all the proper instruction strings."
  (re:all-matches-as-strings *valid* str))

(defun execute-instruction (instr)
  "given an instruction string in the form mul(##,##), return the result
of multiplying the numbers"
  (apply #'*
         (mapcar #'parse-integer (re:all-matches-as-strings "\\d+" instr))))

(defun Day03-1 (progstr)
  "given a string containing proper and improper instructions,
return the sum of the proper multiplcations in all the strings"
  (iter (for inst in (extract-proper-instructions progstr))
    (summing (execute-instruction inst))))

(5a:test Day03-1-test
  (5a:is (= 161 (Day03-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"There are two new instructions you'll need to handle:

The do() instruction enables future mul instructions.  The don't()
instruction disables future mul instructions.  Only the most recent
do() or don't() instruction applies. At the beginning of the program,
mul instructions are enabled."

LEO's NOTES: Again with the regexp. This time I'll extract the chunks
with do() at the beginning and ending with don't() and ignore
everything else. Then do as I did in part one with those valid
chunks.

---------------------------------------------------------------------------- |#

(defparameter *example2*
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defun extract-chunks (str)
  "given a string containing instructions including do() and don't(),
return a list of strings of instructions between do and don't
i.e. proper instructions"
  (re:all-matches-as-strings
   "(?s)do\\(\\)(.*?)don't\\(\\)" ; the regex
   (concatenate 'string "do()" str "don't()"))) ; put do and don't at ends

(defun Day03-2 (progstr)
  (iter (for chunk in (extract-chunks progstr))
    (summing (Day03-1 chunk))))

(5a:test Day03-2-test
  (5a:is (= 48 (Day03-2 *example2*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 03 Part 1 is ~a"
              (Day03-1 (uiop:read-file-string *data-file*))))

(time (format t "The answer to AOC 2024 Day 03 Part 2 is ~a"
              (Day03-2 (uiop:read-file-string *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2024 Day 03 Part 1 is 173419328
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000733 seconds of total run time (0.000680 user, 0.000053 system)
;; 100.00% CPU
;; 413,152 bytes consed

;; The answer to AOC 2024 Day 03 Part 2 is 90669332
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000654 seconds of total run time (0.000626 user, 0.000028 system)
;; 100.00% CPU
;; 414,608 bytes consed


;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 3   00:50:37  14142      0   01:58:36  16752      0
;; 2   00:57:52  13855      0   01:48:11  12571      0
;; 1   00:47:09   9351      0   01:10:39   9932      0
