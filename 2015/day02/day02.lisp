;;;; Day02.lisp
;;;; 2015 AOC Day 2 solution
;;;; Leo Laporte, 24 Sept 2022

(defpackage :day02
  (:import-from :fiveam :test :is) ; testing
  (:use :cl))

(in-package :day02)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---
...
Fortunately, every present is a box (a perfect right rectangular prism),
which makes calculating the required wrapping paper for each gift a little easier: find
the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l.
The elves also need a little extra paper for each present: the area of the smallest side.
How many total square feet of wrapping paper should they order?
...
|#

(defun wrapping-paper (dim)
  "Calculate the total wrapping paper needed given the dims"
  (let ((d1 (elt dim 0))
	(d2 (elt dim 1))
	(d3 (elt dim 2)))
    (+ (* 2 d1 d2)
       (* 2 d2 d3)
       (* 2 d1 d3)
       (* d1 d2))))

(test wrapping-paper  ; (examples from AoC)
  (is (= (+ (* 2 6) (* 2 12) (* 2 8) 6) (wrapping-paper '(2 3 4))))
  (is (= 43 (wrapping-paper '(1 1 10)))))

(defun day2-1 (dims)
  "calculate the wrapping paper required given a list of box dimensions"
  (apply #'+ (mapcar #'wrapping-paper dims)))

#|
--- Part Two ---
...
The ribbon required to wrap a present is the shortest distance around its sides,
or the smallest perimeter of any one face. Each present also requires a bow made
out of ribbon as well; the feet of ribbon required for the perfect bow is equal
to the cubic feet of volume of the present.
How many total feet of ribbon should they order?
...
|#

(defun ribbon (dim)
  "calculate the amount of ribbon needed to wrap a gift (the smallest perimeter plus the
total volume of the gift)"
  (let ((d1 (elt dim 0))
	(d2 (elt dim 1))
	(d3 (elt dim 2)))
    (+ (* 2 (+ d1 d2))  ; perimeter of the smallest face
       (* d1 d2 d3))))

(defun day2-2 (dims)
  "calculate all the ribbon needed by the boxes defined by dims"
  (apply #'+ (mapcar #'ribbon dims)))

(test ribbon ; test Part 2 code
  (is (= (ribbon '(2 3 4)) (+ (+ 2 2 3 3) (* 2 3 4))))
  (is (= (ribbon '(1 1 10)) 14)))

;; this is the only difficult part of the whole problem: massaging the data into a form I can use
(defun parse-input (file)
  "Turn input file into a list of a list of dimensions stored as '(### ### ###) - sorted from low to high"
  (mapcar (lambda (x) (sort x #'<))                           ; sort the dims from lowest to highest
	  (mapcar (lambda (x) (mapcar #'parse-integer x))     ; turn the dim strs into numbers
		  (mapcar (lambda (x) (uiop:split-string x :separator "x"))  ; split the dims on 'x'
			  (str:words                          ; separate the string into a list of dim str
			   (uiop:read-file-string file))))))  ; get the input as a big string

(defvar data (parse-input "input.txt"))
(time (format t "The answer to AOC 2015 Day 2 Part 1 is ~a" (day2-1 data)))
(time (format t "The answer to AOC 2015 Day 2 Part 2 is ~a" (day2-2 data)))

;; The answer to AOC 2015 Day 2 Part 1 is 1586300
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000050 seconds of total run time (0.000046 user, 0.000004 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to AOC 2015 Day 2 Part 2 is 3737498
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000040 seconds of total run time (0.000035 user, 0.000005 system)
;; 100.00% CPU
;; 65,024 bytes consed
