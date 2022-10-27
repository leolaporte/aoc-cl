;;;; Day04.lisp
;;;; 2015 AOC Day 04 solution
;;;; Leo Laporte, 30 Sept 2022

(ql:quickload '(:fiveam :cl-ppcre :md5))

(defpackage :day04
  (:use #:cl
	#:md5
	#:fiveam))  ; for testing, since these are all small programs I do the testing inline

(in-package :day04)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---

MD5 hashes in hexadecimal start with at least five zeroes. The input to the MD5 hash
is some secret key (your puzzle input, given below) followed by a number in decimal.
To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes:
1, 2, 3, ...) that produces such a hash.
|#

(defun five-leading-zeros-p (md5-hash)
  "return true if an MD5 hash begins with 5 leading zeroes - I had to crib this solutionfrom https://github.com/iamFIREcracker/adventofcode/blob/master/src/2015/day04.lisp because
the lisp reader always returns 00 as 0 - this nice workaround counts each 0 as 00"
  (and (zerop (aref md5-hash 0)) ; two zeros
       (zerop (aref md5-hash 1)) ; another two zeroes
       (zerop (ldb (byte 4 4) (aref md5-hash 2))))) ; just need one more

(defun day04-1 (key)
  "find the numeric suffix to key that produces an MD5 hash beginning with 5 or more zeroes"
  (do ((n 1 (1+ n)))
      ((five-leading-zeros-p (md5sum-string (concatenate 'string key (write-to-string n))))
       n)))

(test 04-1  ; examples from AoC
  (is (= 609043 (day04-1 "abcdef")))
  (is (= 1048970 (day04-1 "pqrstuv"))))

#|
--- Part Two ---

Now do it with six zeroes
|#

(defun six-leading-zeros-p (md5-hash)
  "return true if an MD5 hash begins with 6 leading zeroes"
  (and (zerop (aref md5-hash 0)) ; two zeros
       (zerop (aref md5-hash 1)) ; another two zeroes
       (zerop (aref md5-hash 2)))) ; and two more

(defun day04-2 (key)
  "find the numeric suffix to key that produces an MD5 hash beginning with 6 or more zeroes"
  (do ((n 1 (1+ n)))
      ((six-leading-zeros-p (md5sum-string (concatenate 'string key (write-to-string n))))
       n)))

(defvar key "yzbqklnj")
(time (format t "The answer to AOC 2015 Day 04 Part 1 is ~a" (day04-1 key)))
(time (format t "The answer to AOC 2015 Day 04 Part 2 is ~a" (day04-2 key)))

;; Timings on M2 Macbook Air with 24GB RAM

;; The answer to AOC 2015 Day 04 Part 1 is 282749
;; Evaluation took:
;; 0.181 seconds of real time
;; 0.181605 seconds of total run time (0.167061 user, 0.014544 system)
;; [ Run times consist of 0.004 seconds GC time, and 0.178 seconds non-GC time. ]
;; 100.55% CPU
;; 149,150,288 bytes consed

;; The answer to AOC 2015 Day 04 Part 2 is 9962624
;; Evaluation took:
;; 6.449 seconds of real time
;; 6.472073 seconds of total run time (5.928452 user, 0.543621 system)
;; [ Run times consist of 0.149 seconds GC time, and 6.324 seconds non-GC time. ]
;; 100.36% CPU
;; 5,260,096,256 bytes consed
