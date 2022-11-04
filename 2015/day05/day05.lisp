;;;; Day005.lisp
;;;; 2015 AOC Day 005 solution
;;;; Leo Laporte, Sept 2022

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day05
  (:use #:cl
	#:cl-ppcre  ; regular expressions for part 2
	#:fiveam))  ; for testing, since these are all small programs I do the testing inline

(in-package :day05)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or
aabbccdd (aa, bb, cc, or dd).

It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the
other requirements.

How many strings are nice?
|#

(defun forbidden-chars-p (bad-str-list str)
  "returns true if str contains any string in the bad-str-list (recursive style)"
  (if (null bad-str-list)
      nil
      (or (search (first bad-str-list) str :test #'equalp)
	  (forbidden-chars-p (rest bad-str-list) str))))

(test forbidden-chars-test
  (is-false (forbidden-chars-p '("ab" "cd" "pq" "xy") "this should be false"))
  (is-true (forbidden-chars-p '("ab" "cd" "pq" "xy") "this absolutely should be true"))
  (is-true (forbidden-chars-p '("this") "this is true!"))
  (is-false (forbidden-chars-p '() "it doesn't matter!")))

(defun count-letters (chars-to-count-list str)
  "returns the count of chars in the chars-to-count-list that appear in a string (do style)"
  (do ((i chars-to-count-list (rest i))
       (cnt 0 (+ cnt (count (first i) str :test #'equalp))))
      ((null i) cnt)))

(test count-letters-test
  (is (= 3 (count-letters '(#\a #\e #\i #\o #\u) "three vowls")))
  (is (= 0 (count-letters '(#\a #\e #\i #\o #\u) "xyz")))
  (is (= 8 (count-letters '(#\a #\e #\i #\o #\u) "lots of vowels up in here"))))

(defun doubled-letter-p (str)
  "returns true if there is at least one doubled letter in string (do style)"
  (do ((i 0 (1+ i)))
      ((= i (1- (length str))) nil)                ; end of string and no doubled char
    (when (equalp (char str i) (char str (1+ i)))    ; doubled char?
      (return i))))                              ; return index of first doubled char

(test doubled-letter-test
  (is-false (doubled-letter-p "there is no double"))
  (is-true (doubled-letter-p "there is a doubled letter"))
  (is-true (doubled-letter-p "the double ocurs at the endd")))

(defun nicep (str)
  "returns true if a string is nice"
  (and
   (not (forbidden-chars-p '("ab" "cd" "pq" "xy") str))  ; contains no forbidden strings
   (>= (count-letters '(#\a #\e #\i #\i \#o #\u) str) 3) ; contains at least three vowels
   (doubled-letter-p str)))                              ; contains at least one doubled letter

(test nicep-test  ; examples from AoC
  (is-true (nicep "ugknbfddgicrmopn"))
  (is-true (nicep "aaa"))
  (is-false (nicep "jchzalrnumimnmhp"))
  (is-false (nicep "haegwjzuvuyypxyu"))
  (is-false (nicep "dvszwmarrgswjxmb")))

(defun day05-1 (input)
  "returns the number of nice strings in input"
  (length (remove-if-not #'nicep input)))

#|
--- Part Two ---

Now, a nice string is one with all of the following properties:

It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
|#

;; pre-scan regexes since they'll be called 1000 times each
(defvar dblpr-regex (create-scanner "(..).*\\1")) ; regex for a doubled pair
(defvar rptbet-regex (create-scanner "(.).\\1"))  ; regex for a repeated pair

(defun doubled-pair-p (str)
  "returns true if any pair of letters repeats later in the string (using regex for this)"
  (scan dblpr-regex str))

(test doubled-pair-test
  (is-true (doubled-pair-p "aabaa"))
  (is-true (doubled-pair-p "xyxy"))
  (is-true (doubled-pair-p "aabcdefgaa"))
  (is-false (doubled-pair-p "aaa")))

(defun repeat-between-p (str)
  "returns true if at least one letter repeats with one letter between (regex, too)"
  (scan rptbet-regex str))

(test repeat-between-test
  (is-true (repeat-between-p "xyx"))
  (is-true (repeat-between-p "abcdefeghi"))
  (is-true (repeat-between-p "aaa"))
  (is-false (repeat-between-p "abc")))

(defun nice2-p (str)
  "returns true if str satisfies the part two conditions"
  (and
   (doubled-pair-p str)
   (repeat-between-p str)))

(test nice2p-test
  (is-true (nice2-p "qjhvhtzxzqqjkmpb"))
  (is-true (nice2-p "xxyxx"))
  (is-false (nice2-p "uurcxstgmygtbstg"))
  (is-false (nice2-p "ieodomkazucvgmuy")))

(defun day05-2 (input)
  (length (remove-if-not #'nice2-p input)))

(defvar data (uiop:read-file-lines "~/cl/AOC/2015/day05/input.txt"))

(time (format t "The answer to AOC 2015 Day 005 Part 1 is ~a" (day05-1 data)))
(time (format t "The answer to AOC 2015 Day 005 Part 2 is ~a" (day05-2 data)))

;; Timings on M2 Macbook Air with 24GB RAM

;; The answer to AOC 2015 Day 005 Part 1 is 0
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000660 seconds of total run time (0.000660 user, 0.000000 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to AOC 2015 Day 005 Part 2 is 89
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000809 seconds of total run time (0.000806 user, 0.000003 system)
;; 100.00% CPU
;; 0 bytes consed
