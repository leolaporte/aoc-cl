;;;; Day09.lisp
;;;; 2023 AOC Day 09 solution
;;;; Leo Laporte
;;;; 5 Jan 2024
;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day09
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day09)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_09/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 9: Mirage Maintenance ---
--- Part One ---

" Each line in the report contains the history of a single value. Your
environmental report should include a prediction of the next value in
each history.

To do this, start by making a new sequence from the difference at each
step of your history. If that sequence is not all zeroes, repeat this
process, using the sequence you just generated as the input
sequence. Once all of the values in your latest sequence are zeroes,
you can extrapolate what the next value of the original history should
be.

Analyze your OASIS report and extrapolate the next value for each
history. What is the sum of these extrapolated values?"

LEO'S NOTES: OK so we're creating a solver for stepped intervals of
numbers. Quite a clever method. Wish I'd known that when I took the
math SATs. This will definitely benefit from some analysis - a good
representation of the data should go a long way toward an efficient
routine.

Perhaps a multi-dimensional array? But we don't know the dimensions
ahead of time. I'l just start with lists and lists of lists and see
how it goes.

----------------------------- Parsing --------------------------------------- |#

(defparameter *test-data*
  '("0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"))

(defun parse-histories (los)
  "given a list of lists of number strings, return a list of lists of
numbers"
  (let ((histories '()))

    (dolist (l los)
      (push
       (mapcar #'parse-integer (cl-ppcre:split "\\s+" l))
       histories))

    (reverse histories)))

#|---------------------------- Working Code ----------------------------------|#

(defun build-sequence (loi)
  "given a list of integers return a list of the differences between
 each number"
  (cond ((null (rest loi)) nil)
        (t (cons (- (second loi) (first loi))
                 (build-sequence (rest loi))))))

(5a:test build-sequence-test
  (let ((td (parse-histories *test-data*)))
    (5a:is (equal (build-sequence (first td)) '(3 3 3 3 3)))
    (5a:is (equal (build-sequence (second td)) '(2 3 4 5 6)))
    (5a:is (equal (build-sequence (third td)) '(3 3 5 9 15)))))

(defun next-in-sequence (history)
  "given a list of numbers return the next number in the sequence"
  ;; loop updating seq and result each time
  (do* ((seq history (build-sequence seq))
        (sub-seqs (list seq) (push seq sub-seqs)))

       ;; until seq is all zeros, then return the final item in the
       ;; sequence
       ((every #'zerop seq) ; all zeros?

        ;; then add up the last digits in all the sequences
        (reduce #'+ (mapcar #'(lambda (l) (car (last l)))
                            sub-seqs)))))

(5a:test next-in-sequence-test
  (let ((td (parse-histories *test-data*)))
    (5a:is (equal (next-in-sequence (first td)) 18))
    (5a:is (equal (next-in-sequence (second td)) 28))
    (5a:is (equal (next-in-sequence (third td)) 68))))

(defun Day09-1 (los)
  (let ((histories (parse-histories los)))
    (reduce #'+ (mapcar #'next-in-sequence histories))))

(5a:test Day09-1-test
  (5a:is (= (day09-1 *test-data*) 114)))

#| -----------------------------------------------------------------------------
--- Part Two ---

"Rather than adding a zero to the end and filling in the next values
of each previous sequence, you should instead add a zero to the
beginning of your sequence of zeroes, then fill in new first values
for each previous sequence.

Analyze your OASIS report again, this time extrapolating the previous
value for each history. What is the sum of these extrapolated values?"

LEO'S NOTES: Um reverse?

------------------------------------------------------------------------------|#

(Defun Day09-2 (los)
  (let ((histories (parse-histories los)))
    (reduce #'+ (mapcar #'next-in-sequence
                        (mapcar #'reverse histories)))))

(5a:test Day09-2-test
  (5a:is (= (day09-2 *test-data*) 2)))


;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 09 Part 1 is ~a"
              (day09-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 09 Part 2 is ~a"
              (day09-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 09 Part 1 is 1877825184
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001466 seconds of total run time (0.001217 user, 0.000249 system)
;; 100.00% CPU
;; 1,113,424 bytes consed

;; The answer to AOC 2023 Day 09 Part 2 is 1108
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001355 seconds of total run time (0.001199 user, 0.000156 system)
;; 100.00% CPU
;; 1,113,840 bytes consed
