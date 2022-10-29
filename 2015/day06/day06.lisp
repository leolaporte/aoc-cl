;;;; Day06.lisp
;;;; 2015 AOC Day 06 solution
;;;; Leo Laporte, 8 Oct 2022

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day06
  (:use #:cl
	#:cl-ppcre  ; for regex
	#:fiveam))  ; for testing, since these are all small programs I do the testing inline

(in-package :day06)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)

#|
--- Part One ---
...
Set up your lights by doing the instructions Santa sent you in order.

e.g. "turn off 0,0 through 1,1" or "turn on 0,0 through 1,1" or "toggle 0,0 through 1,1"
the provided input contains 300 such instructions

After following the instructions, how many lights are lit?
...

First I'll do it as an array of integers, but that's so wasteful. All we really need is
one bit per light. And then I can use the faster(?) bitwise operators.
|#

(defconstant +OFF+ 0)
(defconstant +ON+ 1)

(defparameter *lights* (make-array '(1000 1000) :initial-element +OFF+)) ; our light array, all lights off
(defparameter *command-regex* (create-scanner "(.+) (\\d+),(\\d+) through (\\d+),(\\d+)"))

(defun switch (state x1 y1 x2 y2)
  "switches lights in the *lights* array in the range (x1,y1)->(x2,y2) to state"
  (do ((y y1 (1+ y)))     ; for each row
      ((> y y2))
    (do ((x x1 (1+ x)))   ; for each column in that row
	((> x x2))
      (setf (aref *lights* x y) state))))

(defun toggle (x1 y1 x2 y2)
  "toggles the state of each light in the *lights* array in the range (x1,y1)->(x2,y2)"
  (do ((y y1 (1+ y)))     ; for each row
      ((> y y2))
    (do ((x x1 (1+ x)))   ; for each column in that row
	((> x x2))
      (if (= (aref *lights* x y) +ON+)
	  (setf (aref *lights* x y) +OFF+)
	  (setf (aref *lights* x y) +ON+)))))

(test switch-toggle-test
  (switch +ON+ 0 0 1 1)
  (is (= 1 (aref *lights* 0 0)))
  (switch +OFF+ 0 0 1 1)
  (is (= 0 (aref *lights* 0 0)))
  (toggle 0 0 1 1)
  (is (= 1 (aref *lights* 1 1)))
  (switch +ON+ 0 0 999 999)
  (is (= 1 (aref *lights* 500 500)))
  (switch +OFF+ 0 0 999 999)
  (is (= 0 (aref *lights* 998 999)))
  (toggle 0 0 999 999)
  (is (= 1 (aref *lights* 72 87))))

(defun execute-command (cstr)
  "executes a command string in the form 'command start-point through stop-point'
commands are 'turn on' 'turn off' and 'toggle'"
  (register-groups-bind
      (cmd (#'parse-integer x1 y1 x2 y2))
      (*command-regex* cstr)
    (cond
      ((string= cmd "turn on") (switch +ON+ x1 y1 x2 y2))
      ((string= cmd "turn off") (switch +OFF+ x1 y1 x2 y2))
      ((string= cmd "toggle") (toggle x1 y1 x2 y2))
      (t (error "Hunh? Got an illegal command!")))))

(test execute-command-test
  (switch +OFF+ 0 0 999 999) ; reset lights
  (execute-command "turn on 0,0 through 1,1")
  (is (= 1 (aref *lights* 0 0)))
  (execute-command "turn off 53,53 through 53,53")
  (is (= 0 (aref *lights* 53 53)))
  (setf (aref *lights* 1 1) 0)
  (execute-command "toggle 1,1 through 1,1")
  (is (= 1 (aref *lights* 1 1)))
  (execute-command "turn on 0,0 through 999,999")
  (is (= 1 (aref *lights* 500 500)))
  (execute-command "turn off 0,0 through 999,999")
  (is (= 0 (aref *lights* 998 999)))
  (execute-command "toggle 0,0 through 999,999")
  (is (= 1 (aref *lights* 72 87))))

(defun sum-lights ()
  "sums the items in the 2D array *lights*"
  (reduce #'+ (make-array (array-total-size *lights*) :displaced-to *lights*))) ; flatten array

(defun day06-1 (input)
  "sets all the lights in *lights* to the state commanded by input then counts the on lights"
  (switch +OFF+ 0 0 999 999) ; reset all *lights*
  (mapcar #'execute-command input)  ; side-effect city - ignore the results
  (sum-lights))

(test day06-1-test  ; examples from AoC
  (is (= (* 1000 1000) (day06-1 '("turn on 0,0 through 999,999"))))
  (is (= 1000 (day06-1 '("toggle 0,0 through 999,0"))))
  (is (= 1000 (day06-1 '("toggle 0,0 through 0,999")))))

#|
--- Part Two ---

The phrase turn on actually means that you should increase the brightness of those lights by 1.

The phrase turn off actually means that you should decrease the brightness of those lights by 1,
to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?
|#

;; so in the second part I have to change the meaning of turn on, off, and toggle

(defun turn-up (x1 y1 x2 y2)
  "increments lights in the *lights* array in the range (x1,y1)->(x2,y2)"
  (do ((y y1 (1+ y)))     ; for each row
      ((> y y2))
    (do ((x x1 (1+ x)))   ; for each column in that row
	((> x x2))
      (incf (aref *lights* x y)))))

(defun turn-down (x1 y1 x2 y2)
  "decrements lights in the *lights* array in the range (x1,y1)->(x2,y2)"
  (do ((y y1 (1+ y)))     ; for each row
      ((> y y2))
    (do ((x x1 (1+ x)))   ; for each column in that row
	((> x x2))
      (if (> (aref *lights* x y) 0)
	  (decf (aref *lights* x y))))))

(defun execute-new-command (cstr)
  "executes a command string in the form 'command start-point through stop-point'
commands are 'turn on' (add 1) 'turn off' (subtract 1)  and 'toggle' (add 2)"
  (register-groups-bind
      (cmd (#'parse-integer x1 y1 x2 y2)) ; string plus four integers
      (*command-regex* cstr)
    (cond
      ((string= cmd "turn on")
       (turn-up x1 y1 x2 y2))
      ((string= cmd "turn off")
       (turn-down x1 y1 x2 y2))
      ((string= cmd "toggle")
       (turn-up x1 y1 x2 y2)
       (turn-up x1 y1 x2 y2))
      (t (error "Hunh? Got an illegal command!")))))

(test execute-new-command-test
  (switch +OFF+ 0 0 999 999) ; reset lights
  (execute-new-command "turn on 0,0 through 1,1")
  (is (= 1 (aref *lights* 0 0)))
  (execute-new-command "turn off 53,53 through 53,53")
  (is (= 0 (aref *lights* 53 53)))
  (setf (aref *lights* 1 1) 0)
  (execute-new-command "toggle 1,1 through 1,1")
  (is (= 2 (aref *lights* 1 1)))
  (switch +OFF+ 0 0 999 999) ; reset lights
  (execute-new-command "turn on 0,0 through 999,999")
  (execute-new-command "turn on 0,0 through 999,999")
  (is (= 2 (aref *lights* 500 500)))
  (execute-new-command "turn off 0,0 through 999,999")
  (execute-new-command "turn off 0,0 through 999,999")
  (execute-new-command "turn off 0,0 through 999,999")
  (execute-new-command "turn off 0,0 through 999,999")
  (execute-new-command "turn off 0,0 through 999,999")
  (is (= 0 (aref *lights* 998 999)))
  (execute-new-command "toggle 0,0 through 999,999")
  (is (= 2 (aref *lights* 72 87))))

(defun day06-2 (input)
  "adjusts all the *lights* as commanded by input then counts the on lights"
  (switch +OFF+ 0 0 999 999) ; reset all *lights*
  (mapcar #'execute-new-command input)  ; side-effect city - ignore the results
  (sum-lights))

(test day06-2-test  ; examples from AoC
  (is (= 1 (day06-2 '("turn on 0,0 through 0,0"))))
  (is (= 2000000 (day06-2 '("toggle 0,0 through 999,999")))))

(defvar data (uiop:read-file-lines "input.txt"))
(time (format t "The answer to AOC 2015 Day 6 Part 1 is ~a" (day06-1 data)))
(time (format t "The answer to AOC 2015 Day 6 Part 2 is ~a" (day06-2 data)))

;; Timings on M2 Macbook Air with 24GB RAM

;; The answer to AOC 2015 Day 6 Part 1 is 400410
;; Evaluation took:
;; 0.149 seconds of real time
;; 0.149483 seconds of total run time (0.148911 user, 0.000572 system)
;; 100.00% CPU
;; 131,056 bytes consed

;; The answer to AOC 2015 Day 6 Part 2 is 15343601
;; Evaluation took:
;; 0.308 seconds of real time
;; 0.308075 seconds of total run time (0.307034 user, 0.001041 system)
;; 100.00% CPU
;; 130,992 bytes consed
