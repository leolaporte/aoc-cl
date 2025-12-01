;;;; Day01.lisp
;;;; 2025 AOC Day 1 solution
;;;; Common Lisp solutions by Leo Laporte
;;;; Started: 30 Nov 2025 at 21:01
;;;; Finished: 1 Dec 2025 ar 13:10

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day01
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day01/input.txt"
  "Downloaded from the AoC problem set")

;; ---------------------------------------------------------------------------
;;                       --- Day 1: Secret Entrance ---
;;                              --- Part One ---
;;
;; LEO'S NOTES: I'm counting the number of times the position = 0. I'll
;; parse the input to replace R and L with + and - and just convert the
;; instructions into positive and negative addends. Then use MOD to wrap
;; around.
;;
;; ---------------------------------------------------------------------------

(defparameter *example* '("L68"
                          "L30"
                          "R48"
                          "L5"
                          "R60"
                          "L55"
                          "L1"
                          "L99"
                          "R14"
                          "L82"))

(sr:-> parse-input (list) list)
(defun parse-input (input)
  "given a list of strings describing turns of a dial replace the strings
with integers"
  (sr:~> input
         (mapcar (lambda (s) (substitute #\- #\L s)) _) ; "L20" -> "-20"
         (mapcar (lambda (s) (substitute #\+ #\R s)) _) ; "R20" -> "+20"
         (mapcar #'parse-integer _)))                   ; "-20" -> -20

(sr:-> next-position (number number) number)
(defun next-position (start move)
  "given a position on a safe dial with numbers from 0 to 99 and a move
in a negative or positive direction, return the new position"
  (mod (+ move start) 100))

(sr:-> day01-1 (list) number)
(defun day01-1 (input)
  "given a list of string describing the number of turns, left and
right, of a safe dial running from 0-99 return the number of times the
dial reaches 0 while making all the turns"
  (let ((instructions (parse-input input))
        (posn 50))                      ; dial starting position

    (Iter (for i in instructions)
      (setf posn (next-position posn i))
      (counting (= posn 0)))))

(5a:test day01-1-test
  (5a:is (= 3 (day01-1 *example*))))

;; ---------------------------------------------------------------------------
;;
;;                            --- Part Two --
;;
;; LEO'S NOTES: Now instead of counting the number of times the dial
;; lands on zero we need to count the number of times it passes
;; zero. Note that some of the turns go around multiple times. R1000
;; would pass zero as many as 10 times.
;;
;; This took me way too long. And my brain still hurts.
;; ---------------------------------------------------------------------------

(sr:-> count-zeros (number number) number)
(defun count-zeros (posn turn)
  "given a current dial position and a positive or negative turn, return
how many 0 are passed or landed on on a safe dial that goes from 0 to
99"
  (if (< turn 0)
      ;; turning to the left
      ;; add first time around the dial..
      (+ (if (or (zerop posn)           ; doesn't pass 0 on first turn
                 (< 0 (next-position posn turn) posn)) ; first time still on dial
             0 1)

         ;; ... to subsequent turns around the dial
         (truncate (abs turn) 100))

      ;; else turning to the right
      ;; add first time around the dial...
      (+ (if (<= posn (next-position posn turn) 99) 0 1)
         ;; ... to subsequent turns around the dial
         (truncate turn 100))))

;; I had to make a lot of tests for this - more trial and error than logic
(5a:test count-zeros-test
  (5a:is (= 0 (count-zeros 99 -98)))
  (5a:is (= 1 (count-zeros 99 -99)))
  (5a:is (= 1 (count-zeros 99 -198)))
  (5a:is (= 2 (count-zeros 99 -199)))
  (5a:is (= 3 (count-zeros 99 -299)))
  (5a:is (= 0 (count-zeros 1 98)))
  (5a:is (= 1 (count-zeros 1 99)))
  (5a:is (= 1 (count-zeros 1 198)))
  (5a:is (= 2 (count-zeros 1 199)))
  (5a:is (= 3 (count-zeros 1 299)))
  (5a:is (= 0 (count-zeros 0 1)))
  (5a:is (= 0 (count-zeros 0 99)))
  (5a:is (= 1 (count-zeros 0 100)))
  (5a:is (= 0 (count-zeros 0 -1)))
  (5a:is (= 0 (count-zeros 0 -99)))
  (5a:is (= 1 (count-zeros 0 -100)))
  ;; now the examples given
  (5a:is (= 1 (count-zeros 50 -68)))
  (5a:is (= 0 (count-zeros 82 -30)))
  (5a:is (= 1 (count-zeros 52 48)))
  (5a:is (= 0 (count-zeros 0 -5)))
  (5a:is (= 1 (count-zeros 95 60)))
  (5a:is (= 1 (count-zeros 55 -55)))
  (5a:is (= 0 (count-zeros 0 -1)))
  (5a:is (= 1 (count-zeros 99 -99)))
  (5a:is (= 0 (count-zeros 0 14)))
  (5a:is (= 1 (count-zeros 14 -82))))

(sr:-> day01-2 (list) number)
(defun day01-2 (input)
  "given a list of strings representing turns of a safe dial left and
right, return the number of times the dial lands on or passes zero
while executing all the turns"
  (let ((instructions (parse-input input))
        (posn 50))

    (iter (for i in instructions)
      (let ((start posn))
        (setf posn (mod (+ posn i) 100)) ; get next position
        (summing                  ; count zero crossings from old posn
          (count-zeros start i))))))

(5a:test day01-2-test
  (5a:is (= 6 (day01-2 *example*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to AOC 2025 Day 1 Part 1 is ~a"
              (day01-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2025 Day 1 Part 2 is ~a"
              (day01-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 1 Part 1 is 1123
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001037 seconds of total run time (0.000976 user, 0.000061 system)
;; 100.00% CPU
;; 782,688 bytes consed

;; The answer to AOC 2025 Day 1 Part 2 is 6695
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001033 seconds of total run time (0.001017 user, 0.000016 system)
;; 100.00% CPU
;; 786,352 bytes consed
