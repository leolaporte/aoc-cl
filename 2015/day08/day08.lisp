;;;; Day08.lisp
;;;; 2015 AOC Day 08 solution
;;;; Leo Laporte, Sept 2022

(ql:quickload '(:fiveam :cl-ppcre :alexandria))

(defpackage :day08
  (:use #:cl
	#:cl-ppcre    ; for regex
	#:alexandria  ; lil utilities
	#:fiveam))    ; for testing, since these are all small programs I do the testing inline

(in-package :day08)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))

(defparameter *data-file* "~/cl/AOC/2015/day08/input.txt")

#|

Matchsticks (sounds like we'll be using regex!)

--- Part One ---

Santa's list is a file that contains many double-quoted string literals, one on each line.
The only escape sequences used are \\ (which represents a single backslash), \" (which
represents a lone double-quote character), and \x plus two hexadecimal characters (which
represents a single character with that ASCII code).

Disregarding the whitespace in the file, what is the number of characters of code for string
literals minus the number of characters in memory for the values of the strings in total for
the entire file?

|#

#|
NOTES: well how about that. The CL length command counts escaped literals properly. I will
have to replace the \x## with a digit but otherwise there's no need to process the literal.
I do have to calculate the UNescaped string length however. That turns out that's the real
challengd.

|#

(defun code-len (f)
  "calculate the raw length of a file minus returns"
  (let ((in (open f :if-does-not-exist nil))
	(c-len 0))
    (when in
      (setf c-len
	    (length (remove 10 (read-file-into-byte-vector in))))) ; without linebreaks
    (close in)
    c-len))

;; Pre-compiled regular expxressions for escape codes
(defparameter *hex-char* (create-scanner "\[Xx][0-9A-Fa-f][0-9A-Fa-f]")
  "matches \x## - a hexadecimal string")

(defparameter *slash-quote* (create-scanner "\\\"")
  "matches \" - an escaped quote")

(defparameter *slash-slash* (create-scanner "\\{2}")
  "matches \\ - an escaped slash")

(defun clean-string (s)
  "given an escaped string return the unescaped string - replace all the escaped strings"
  (regex-replace-all *hex-char* s "."))

(defun val-len (f)
  "calculates the escaped length of all the strings in f"
  (let ((lines (uiop:read-file-lines f)))
    (reduce #'+ (mapcar #'(lambda (x) (length (clean-string x))) lines))))

(defun day08-1 (f)
  "returns the raw byte length of all the strings minus the escaped length"
  (- (code-len f) (val-len f)))

(test day08-1-test
  (is (= 12 (day08-1 "test.txt"))))

#|
--- Part Two ---

|#


;; (time (format t "The answer to AOC 2015 Day 08 Part 1 is ~a" (day08-1 *data-file*)))
;; (time (format t "The answer to AOC 2015 Day 08 Part 2 is ~a" (day08-2 data)))
