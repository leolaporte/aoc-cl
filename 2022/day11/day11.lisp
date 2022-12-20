;;;; Day11.lisp
;;;; 2022 AOC Day 11 solution
;;;; Leo Laporte, 11 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------
(ql:quickload '(:fiveam :cl-ppcre :alexandria :str))

(defpackage :day11
  (:use #:cl)
  (:local-nicknames
   (:ax :alexandria)
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day11)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day11/input.txt")  ; supplied data from AoC

#| ----------------------------------------------------------------------------------------------------
--- Day 11: Monkey in the Middle ---

--- Part One ---

You take some notes (your puzzle input) on the items each monkey currently has, how worried you are
about those items, and how the monkey makes decisions based on your worry level.

After each monkey inspects an item but before it tests your worry level, your relief that the monkey's inspection didn't damage the item causes your worry level to be divided by three and rounded down to the nearest integer.

The monkeys take turns inspecting and throwing items. On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed. Monkey 0 goes first, then monkey 1, and so on until each monkey has had one turn. The process of each monkey taking a single turn is called a round.

When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list. A monkey that starts a round with no items could end up inspecting and throwing many items by the time its turn comes around. If a monkey is holding no items at the start of its turn, its turn ends.

Count the total number of times each monkey inspects items over 20 rounds:

Monkey 0 inspected items 101 times.
Monkey 1 inspected items 95 times.
Monkey 2 inspected items 7 times.
Monkey 3 inspected items 105 times.

In this example, the two most active monkeys inspected items 101 and 105 times. The level of monkey business in this situation can be found by multiplying these together: 10605.

Figure out which monkeys to chase by counting how many items they inspect over 20 rounds. What is the level of monkey business after 20 rounds of stuff-slinging simian shenanigans?

---------------------------------------------------------------------------------------------------- |#

;; ----------------------------------------------------------------------------------------------------
;; DATA
;; ----------------------------------------------------------------------------------------------------

;; First set up the data parsing
(defparameter *tst0*  ; provided example monkey list
  '("Monkey 0:"
    "Starting items: 79, 98"
    "Operation: new = old * 19"
    "Test: divisible by 23"   ; THIS is a clue (he says many days later)
    "If true: throw to monkey 2"
    "If false: throw to monkey 3"
    ""
    "Monkey 1:"
    "Starting items: 54, 65, 75, 74"
    "Operation: new = old + 6"
    "Test: divisible by 19"
    "If true: throw to monkey 2"
    "If false: throw to monkey 0"
    ""
    "Monkey 2:"
    "Starting items: 79, 60, 97"
    "Operation: new = old * old"
    "Test: divisible by 13"
    "If true: throw to monkey 1"
    "If false: throw to monkey 3"
    ""
    "Monkey 3:"
    "Starting items: 74"
    "Operation: new = old + 3"
    "Test: divisible by 17"
    "If true: throw to monkey 0"
    "If false: throw to monkey 1"))

(defstruct monkey
  (num 0 :type integer)              ; monkey number
  (items nil :type list)             ; list of worry levels for each item monkey is holding
  (op #\* :type base-char)           ; worry level change operation
  (operand 0 :type integer )         ; worry level operand (0 means worry level is squared)
  (worry-test 0 :type integer)       ; worry level test (mod level worry-test)
  (if-true 0 :type integer)          ; where to throw item if test is true
  (if-false 0 :type integer)         ; test is false
  (business 0 :type integer))        ; keep track of monkey business (for part 1)

(defparameter *digits* (re:create-scanner "(\\d+)"))      ; regex to extract digits from monkey info
(defparameter *operator* (re:create-scanner "[\\+|\\*]")) ; regex to extract operator

(defun parse-monkey (m)
  "given a list of strings describing a monkey return a monkey struct"
  (let ((monk (make-monkey)))      ; create an empty monkey record to stuff
    (setf (monkey-num monk)
	  (parse-integer (re:scan-to-strings *digits* (first m))))
    (setf (monkey-items monk)      ; worry list
	  (mapcar #'parse-integer (re:all-matches-as-strings *digits* (second m))))
    (setf (monkey-op monk)         ; operator
	  (char (re:scan-to-strings *operator* (third m)) 0))
    (setf (monkey-operand monk)    ; operand
	  (let ((operand (re:scan-to-strings *digits* (third m))))
	    (if operand
		(parse-integer (re:scan-to-strings *digits* (third m)))
		0)))               ; 0 if square
    (setf (monkey-worry-test monk) ; mod factor
	  (parse-integer (re:scan-to-strings *digits* (fourth m))))
    (setf (monkey-if-true monk)    ; what to do if true
	  (parse-integer (re:scan-to-strings *digits* (fifth m))))
    (setf (monkey-if-false monk)   ; false
	  (parse-integer (re:scan-to-strings *digits* (sixth m))))
    monk))

(defun make-monkeys (list-of-string)
  "given a list of monkey descriptions separated by a blank line, return a list of monkeys
as strings"
  (do ((l list-of-string (rest l))   ; work through list
       (monkeys '())                 ; list of monkey structs
       (monkey '()))                 ; current monkey

      ((null l) (reverse  (cons (reverse monkey) monkeys))) ; return list of structs

    (cond ((equal (first l) "")      ; if "" then finish this monkey
	   (setf monkeys (cons (reverse monkey) monkeys))
	   (setf monkey '()))        ; start a new monkey

	  (t (setf monkey (cons (first l) monkey)))))) ; else add to current monkey

(defun parse-monkeys (mlist)
  "given a list of monkeys return a list of monkey structs"
  (mapcar #'parse-monkey (make-monkeys mlist)))  ; turn monkey strings into monkey structs

;; ----------------------------------------------------------------------------------------------------
;; MAIN
;; ----------------------------------------------------------------------------------------------------

;; play all the rounds
(defun play-the-monkey-game (rounds monkeys worry)
  "process each monkey in the list, for each monkey, process its items list, use worry-level worry
handing off the items one at a time per the rules, do this rounds times, return the final list"
  (dotimes (r rounds)                  ; do this many rounds
    (dotimes (m (length monkeys))      ; for each monkey 0..n
      (setf monkeys (throw-monkey-items (find-monkey m monkeys) monkeys worry))))
  monkeys)

;; this does most of the work
(defun throw-monkey-items (monk monkeys worry)
  "given a monkey record and a monkey list, throw all the monkey's items, keeping track of the
number of inspections performed (monkey-business), return the updated monkey list"
  (do*
   ;; set up locals for each loop
   ((items (monkey-items monk) (monkey-items monk))  ; work our way through monk's list of items
    (item (first items) (first items))               ; each time through use next item

    (op (monkey-op monk))                            ; these are the same each time through
    (operand (monkey-operand monk))                  ; monkey constants
    (worry-test (monkey-worry-test monk))            ; modulo number for test
    (if-true (monkey-if-true monk))                  ; destination monkey if test is true
    (if-false (monkey-if-false monk))                ; destination if false
    (LCM (reduce #'* (loop for i below (length monkeys) ; LCM of all the worry tests
			   collecting (monkey-worry-test (find-monkey i monkeys))))))

   ;; end loop test
   ((null items) monkeys)         ; no more items in list, return updated monkeys

    ;; loop body
    (let*
	((new-item                                       ; new item value
	   (floor
	    (mod (cond                                   ; square or multiply/add operand
		   ((zerop operand) (* item item))       ; square item
		   ((equal #\* op) (* item operand))     ; multiply item
		   ((equal #\+ op) (+ item operand)))
		 LCM)                                    ; mod item value by LCM to keep it small
	    worry))                                      ; divide by 3 pt 1, 1 pt 2

	 (dest (if (zerop (mod new-item worry-test))     ; do the test
		   (find-monkey if-true monkeys)         ; find the monkey to throw to
		   (find-monkey if-false monkeys))))

      (setf dest (catch-item new-item dest))             ; and throw to dest monkey
      (setf (monkey-items monk) (rest items)))           ; take off top of list
    (incf (monkey-business monk))))                      ; keep track of monkey business

;; some short utility functions
(defun catch-item (item monk)
  "given a monkey record, add item to the end of its item list and return monkey"
  (setf (monkey-items monk)
	(reverse
	 (cons item (reverse (monkey-items monk)))))
  monk)

(defun find-monkey (num monkeys)
  "returns a monkey structure from monkeys when num = monkey-num, else returns nil"
  (let ((len (length monkeys)))
    (dotimes (l len)
      (when (= num (monkey-num (elt monkeys l)))
	(return (elt monkeys l)))
      nil)))

(defun collect-monkey-business (monkeys)
  "returns the product of the monkey business of the top two most active monkeys"
  (let* ((biz (sort (loop for m below (length monkeys)
			  collect (monkey-business (find-monkey m monkeys))) #'>)))
    (* (first biz) (second biz))))

(defun day11-1 (monkeys)
  (collect-monkey-business (play-the-monkey-game 20 monkeys 3)))

(5a:test day11-1-test
  (5a:is (= 10605 (day11-1 (parse-monkeys *tst0*)))))


#| ----------------------------------------------------------------------------------------------------
--- Part Two ---

You're worried you might not ever get your items back. So worried, in fact, that your relief that a monkey's inspection didn't damage an item no longer causes your worry level to be divided by three.

Unfortunately, that relief was all that was keeping your worry levels from reaching ridiculous levels. You'll need to find another way to keep your worry levels manageable.

At this rate, you might be putting up with these monkeys for a very long time - possibly 10000 rounds!

NOTES: The problem here is that the numbers get unmanageable large. The problem is, how do I
preserve the key information without using such big numbers). Such an early appearance of the
Chinese Remainder Theorem:

From Wikipedia: "The Chinese remainder theorem is widely used for computing with large integers, as it allows replacing a computation for which one knows a bound on the size of the result by several similar computations on small integers."

In other words (mod (* 1234 5678) 23) == (* (mod 1234 23) (mod 134 23))

For future reference, if you ever see mod 23 in a problem, it's probably the CRT.
---------------------------------------------------------------------------------------------------- |#

(defun day11-2 (monkeys)
  (collect-monkey-business (play-the-monkey-game 10000 monkeys 1))) ; 10000 rounds,  worry level 1

(5a:test day11-2-test
  (5a:is (= 2713310158 (day11-2 (parse-monkeys *tst0*)))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 11 Part 1 is ~a"
	      (day11-1 (parse-monkeys (uiop:read-file-lines *data-file*)))))


(time (format t "The answer to AOC 2022 Day 11 Part 2 is ~a"
	      (day11-2 (parse-monkeys (uiop:read-file-lines *data-file*)))))

;; ----------------------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ----------------------------------------------------------------------------------------------------
