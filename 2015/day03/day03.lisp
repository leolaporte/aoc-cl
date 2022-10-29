;;;; Day03.lisp
;;;; 2015 AOC Day 3 solution
;;;; Leo Laporte, 29 Sept 2022

(defpackage :day03
  (:use #:cl
	#:fiveam))  ; for testing, since these are all small programs I do the testing inline

(in-package :day03)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---
...
Moves are always exactly one house to the north (^), south (v), east (>), or west (<).
After each move, he delivers another present to the house at his new location. How many
houses receive at least one present?
...
|#

(defun next-house (dir house)
  "calculate coordinates of next house given a direction and the previous house"
  (case dir                                       ; calculate next house visited
    (#\^ (setf (cdr house) (1- (cdr house))))     ; north
    (#\v (setf (cdr house) (1+ (cdr house))))     ; south
    (#\> (setf (car house) (1+ (car house))))     ; east
    (#\< (setf (car house) (1- (car house)))))    ; west
  house)

(defun day3-1 (dirs)
  "calculate the number of houses visited given the directions"
  (do ((i 0 (1+ i))                              ; index into dirs string
       (house (cons 0 0)                         ; house position, start at 0,0
	      (next-house (char dirs i) house))  ; get next location
       (visited '((0 . 0))                       ; list of visited houses
		(cons (cons (car house) (cdr house)) visited)))  ; add loc of next house to list
      ;; note: I can't use house in the cons because it conses the reference not the value
      ((= i (length dirs))                       ; terminate when we've processed all dirs
       (length (remove-duplicates visited        ; return number of distinct houses visited
				  :test #'(lambda (x y)
					    (and (= (car x) (car y))
						 (= (cdr x) (cdr y)))))))))

(test 3-1 ; tests provided by AoC
  (is (= 2 (day3-1 ">")))
  (is (= 4 (day3-1 "^>v<")))
  (is (= 2 (day3-1 "^v^v^v^v^v"))))

#|
--- Part Two ---
...
Santa and Robo-Santa start at the same location (delivering two presents to the same
starting house), then take turns. How many houses receive at least one present?
...
|#

(defun day3-2 (dirs)
  "calculate the number of houses visited given the directions"
  (do ((i 0 (1+ i))				       	; index into dirs string
       (santa-house (cons 0 0))				; Santa starts at (0.0)
       (robo-house (cons 0 0))				; Robo-Santa starts at (0.0)
       (h '())                                          ; the current house (Santa or Robo)
       (visited '((0 . 0))))				; list of visited houses
      ((= i (length dirs)) 			       	; terminate when we've processed all dirs
       (length (remove-duplicates visited	        ; return number of distinct houses visited
				  :test #'(lambda (x y)
					    (and (= (car x) (car y))
						 (= (cdr x) (cdr y)))))))
    ;; body, repeat until all dirs are processed
    (progn
      (if (oddp i)  ; alternate visitations
	  (setf h santa-house)
	  (setf h robo-house))
      (setf h (next-house (char dirs i) h))
      (setf visited (cons (cons (car h) (cdr h)) visited)))))

(test 3-2
  (is (= 3 (day3-2 "^>")))
  (is (= 3 (day3-2 "^>v<")))
  (is (= 11 (day3-2 "^v^v^v^v^v"))))

(defvar data (uiop:read-file-string "input.txt"))
(time (format t "The answer to AOC 2015 Day 3 Part 1 is ~a" (day3-1 data)))
(time (format t "The answer to AOC 2015 Day 3 Part 2 is ~a" (day3-2 data)))

;; Timings with M2 Macbook Air

;; The answer to AOC 2015 Day 3 Part 1 is 372
;; Evaluation took:
;; 0.007 seconds of real time
;; 0.007102 seconds of total run time (0.007065 user, 0.000037 system)
;; 100.00% CPU
;; 195,072 bytes consed

;; The answer to AOC 2015 Day 3 Part 2 is 363
;; Evaluation took:
;; 0.006 seconds of real time
;; 0.006557 seconds of total run time (0.006526 user, 0.000031 system)
;; 116.67% CPU
;; 195,072 bytes consed
