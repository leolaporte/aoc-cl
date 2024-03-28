;;;; Day19.lisp
;;;; 2023 AOC Day 19 solution
;;;; Leo Laporte
;;;; Started: 19 March 2024, Cabo San Lucas, MX
;;;; Part 1 done: 26 March 2024, Petaluma, CA

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day19
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)               ; misc utilities
   (:ax :alexandria)             ; ditto
   (:re :cl-ppcre)               ; regex
   (:tr :trivia)                 ; pattern matching
   (:tp :trivia.ppcre)           ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day19)
(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_19/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
                       --- Day 19: Aplenty ---
                           --- Part One ---

"Sort through all of the parts you've been given; what do you get if
you add together all of the rating numbers for all of the parts that
ultimately get accepted?"

LEO'S NOTES: Looks like an immutable hash-table for the
instructions. They never change. The name of the rule is the hash key,
the instructions maybe represented as a function? Output of the
function is either 1. another key 2. rejected 3. accepted.

All parts start at key IN.

So can px=> a<2006:qkq,m>2090:A,rfg be represented as a function?
px =>
(lambda (x)
(cond ((< (first x) 2600) qkq)
((> (second x) 2090) A)
(t rfg)))

(funcall (gethash px workflows) x) ; where x is a list of x m a s

If any chain of funcalls returns A then add (+ x m a s) to total.

Ultimately it's just a problem of parsing. Let's go. Worked fine. But
now on to Part 2.

---------------------------------------------------------------------------- |#

(defparameter *test-data*
  "px{a<2006:qkq,m>2090:A,rfg}
  pv{a>1716:R,A}
  lnx{m>1548:A,A}
  rfg{s<537:gd,x>2440:R,A}
  qs{s>3448:A,lnx}
  qkq{x<1416:A,crn}
  crn{x>2662:A,R}
  in{s<1351:px,qqz}
  qqz{s>2770:qs,m<1801:hdj,R}
  gd{a>3333:R,R}
  hdj{m>838:A,pv}

  {X=787,m=2655,a=1222,s=2876}
  {x=1679,m=44,a=2067,s=496}
  {x=2036,m=264,a=79,s=2244}
  {x=2461,m=1339,a=466,s=291}
  {x=2127,m=1623,a=2188,s=1013}")

(defun data-to-lambda (code-str)
  "given a string representing a process, turn it into string
representing a function which can be run
using (funcall (eval (read-from-string exp)) xmas) e.g.

'qqz{s>2770:qs,m<1801:hdj,R}' becomes:
(values 'qqz
 '#'(lambda (x)
      (cond ((> (fourth x) 2770) qs)
            (< (second x) 1802) hdj)
            (t R)))'

Returns NAME (the hash key) and FUNC, a lambda function (the hash
value). The function name is a symbol (using INTERN) and the function
itself is a string"

  (let* ((parts (str:split "{" code-str))
         (name (intern (first parts))) ; func name as symbol - hash key
         (cmd-str (second parts))      ; given description of the func
         (func ""))                    ; the lambda func as a string

    (setf cmd-str (str:trim cmd-str :char-bag "}")) ; kill trailing }
    (setf cmd-str (re:split "," cmd-str)) ; split into separate commands

    (setf func
          (format nil "#'(lambda (x)~%(cond ")) ; opening code boilerplate

    ;; now walk the code-string
    (iter (for f in cmd-str)
      ;; two kinds of commands: > < and jump, R, or A
      (tr:match f

        ;; it's a < or >
        ((tp:ppcre "([xmas])([<>])(\\d+):(\\w+)" cat op num result)
         ;; the function parameter (x) will be the part parameter list
         ;; (list x m a s) so map x to its constituent parts
         (let ((xmas (tr:match cat
                       ("x" "(first x)")
                       ("m" "(second x)")
                       ("a" "(third x)")
                       ("s" "(fourth x)")
                       (otherwise "hunh?"))))

           ;; now append this clause to the func string
           (setf func
                 (str:concat func ; append new command
                             (format nil "((~A ~A ~A) ~S)~%"
                                     op xmas num result)))))

        ;; it's a bare result, R A or a key to jump to
        ;; always comes at the end of the COND
        ((tp:ppcre "^(\\w+)$" result)
         (setf func (str:concat func ; apppend cond's T clause (a bare result)
                                (format nil " (t ~S))))" result))))

        (otherwise (error "can't parse command string"))))

    (values name func)))

(defun parse-input (input-string)
  "given a list of strings describing a series of WORKFLOWS and a list
of part RATINGS to which to apply those instructions return WORKFLOWS,
a hash of functions, and RATINGS, a list of lists of values to process
in the form of (list integer integer integer integer)"
  (let* ((input (re:split "\\n\\n" input-string)) ; split on empty line
         (code (str:words (first input)))         ; list of instruction strs
         (ratings (str:words (second input)))     ; list of parts to process
         (workflows (make-hash-table :test 'equal :size (length code))))

    ;; process code first
    (iter (for inst in code)
      (multiple-value-bind (name func)
          (data-to-lambda inst) ; turn cryptic code str into a usable func
        (setf (gethash name workflows) func))) ; build name=>func hash

    ;; now data
    (setf ratings ; turn data into a list of lists of four integers
          (iter (for line in ratings)
            (collect (mapcar #'parse-integer
                             (re:all-matches-as-strings "\\d+" line)))))

    (values workflows ratings)))

(defun pht (hash)
  "utility to print hashes"
  (iter (for (key val) in-hashtable hash)
        (format t "~%~S => ~S" key val)))

(defun day19-1 (los)
  "parses the provided list of strings into two lists: a list of part ratings
and a hash-table of workflows, processes each part, and returns the
sum of all the part ratings that end up in the A bucket"
  (multiple-value-bind (workflows ratings)
      (parse-input los)

    ;; now loop through each part until it reaches R or A
    (do ((accepted '())                ; accepted parts
         (inst "in" "in")              ; starting instruction for each loop
         (rtngs ratings (rest rtngs))) ; work through each part in ratings

        ;; out of parts? return sum of all accepted part ratings
        ((null rtngs) (apply #'+ accepted))

      ;; for each part, loop through sorter instructions until A or R
      (iter (while (and (not (equal inst "A"))
                        (not (equal inst "R"))))
        (setf inst
              (funcall
               (eval (read-from-string (gethash (intern inst) workflows)))
               (first rtngs))))

      (when (equal inst "A") ; ignore (R)ejects
        (push (apply #'+ (first rtngs)) accepted))))) ; save sum, repeat

(5a:test day19-1-test
  (5a:is (= 19114 (day19-1 *test-data*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"Each of the four ratings (x, m, a, s) can have an integer value
ranging from a minimum of 1 to a maximum of 4000. Of all possible
distinct combinations of ratings, your job is to figure out which ones
will be accepted.  How many distinct combinations of ratings will be
accepted by the Elves' workflows?"

Ugh. So we have to whittle 4000^4 (256 trillion) possibilities to
what? a solution that's somewhat lower. But how do we whittle?

In effect I'll start a list of RANGES (0-4000 0-4000 0-4000 0-4000)
through the TESTs, starting at IN. When I reach {s<1351:px,qqz}, for
example, I'll split the RANGES into two: (0-4000 0-4000 0-4000 0-1350)
which goes to PX and (0-4000 0-4000 0-4000 1351-4000) which goes to
QQZ. At QQZ, {s>2770:qs,m<1801:hdj,R}, there will be multiple
splits. (0-4000 1801-4000 0-4000 0-1350) will go to R and I can
discard it. That leaves (0-4000 0-4000 0-4000 1351-4000) which will
split into (0-4000 0-4000 0-4000 1351-2770) which will become (0-4000
0-1800 0-4000 1351-2770) which will go to HDJ and (0-4000 0-4000
0-4000 2771-4000) which goes to QS, etc etc. At the end of this
process I will have some ranges in the A bucket which I can tally for
the answer.

Maybe a better metaphor is a Tunnel of Love. I'm sending a little BOAT
down the TUNNEL where it will enter a successive series of TRANSITs
to face a list of TESTs. Each test will whittle the number of
possibilities down and/or shunt the boat to a new TRANSIT. The boat
may split into multiple boats at each TRANSIT. At the end, each of
the multitude of boats will reach one of two docks, either R, to be
ignored, or A where all the ranges will be tallied up to provide the
problem's answer.

So the first issue is to think about how I represent the tunnel,
tests, and boats.

TUNNEL will be a hash-table of TRANSITs. Each TRANSIT will have a
name symbol (e.g. in, qx, qrs) and a list of TESTs to be performed on
the BOAT.

TEST will be a structure encapsulating the information provided by
the workflows: the category (or SEAT) affected, the operation (or
SLICER) to be performed, and the RANGE split. Each test will also have
a SHUNT which is where the subject BOAT will go next.

BOATs will have four SEATs, each reflecting a range of numbers from
START to END. Each boat will also have a NEXT slot reflecting the next
TRANSIT (or R and A docks) that it is being sent to.

---------------------------------------------------------------------------- |#

(defstruct (test)
  "the workflow tests - each hash-table key has one or more tests. Tests
with :num 0 will be jumps to either R A or another node"
  (seat "" :type string)
  (slicer "" :type string)
  (range 0 :type fixnum)
  (shunt nil :type symbol))

(defstruct (boat)
  "the little boats that will enter the tunnel of love, eventually
 reaching the final docks R and A"
  (next (intern "in") :type symbol) ; the next (or current) TRANSIT

  (x-start 0 :type fixnum)          ; the low end of the range for seat X
  (x-end 4000 :type fixnum)         ; the high end of the range for seat X, inclusive

  (m-start 0 :type fixnum)          ; the low end of the range for seat M
  (m-end 4000 :type fixnum)         ; the high end of the range for seat M

  (a-start 0 :type fixnum)          ; the low end of the range for seat A
  (a-end 4000 :type fixnum)         ; the high end of the range for seat A

  (s-start 0 :type fixnum)          ; the low end of the range for seat S
  (s-end 4000 :type fixnum))        ; the high end of the range for seat S

(defun str-to-transit (code-str)
  "given a string representing a process, turn it into a TRANSIT with a
name symbol and a list of TEST structures. Returns NAME (the hash key)
and a list of TESTs (the hash value)."

  (let* ((parts (str:split "{" code-str))
         (name (intern (first parts))) ; returns func name as symbol
         (cmd-str (second parts))      ; given description of the func
         (tests '()))                  ; returns list of test structs

    (setf cmd-str (str:trim cmd-str :char-bag "}")) ; kill trailing }
    (setf cmd-str (re:split "," cmd-str)) ; split into separate commands

    ;; now walk the code-string
    (iter (for test in cmd-str)
      ;; two kinds of commands: > < and jump, R, or A
      (tr:match test

        ;; it's a < or >
        ((tp:ppcre "([xmas])([<>])(\\d+):(\\w+)" seat slicer range shunt)
         (push
          (make-test :seat seat :slicer slicer
                     :range (parse-integer range) :shunt (intern shunt))
          tests))

        ;; it's a bare result, R A or a key to jump to
        ((tp:ppcre "^(\\w+)$" shunt)
         (push
          (make-test :shunt (intern shunt))
          tests))

        (otherwise (error "can't parse command string"))))

    (values name (reverse tests))))

(defun build-tunnel (input-string)
  "given a list of strings describing a series of TESTS and an (unsed) list
of parts, return TUNNEL, a hash of TRANSIT structs"
  (let* ((input (re:split "\\n\\n" input-string)) ; split on empty line
         (instructions (str:words (first input))) ; list of instruction strs
         (tunnel (make-hash-table :test 'equal :size (length code))))

    ;; process code
    (iter (for str in instructions)
      (multiple-value-bind (name transit)
          (str-to-transit str) ; create a TRANSIT from code string
        (setf (gethash name tunnel) transit)) ; build name=>func hash TUNNEL
      (finally (return tunnel)))))

(defun slicer (boat transform)
  "given a boat structure, and a TRANSIT, return a list of boats
 that emerge with new ranges and destinations"
  )


(5a:test day19-2-test
  (5a:is (= 167409079868000 (day19-2 *test-data*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 19 Part 1 is ~a"
	      (day19-1 (str:from-file *data-file*))))

;; (time (format t "The answer to AOC 2023 Day 19 Part 2 is ~a"
;;	      (day19-2 (str:from-file *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 19 Part 1 is 382440
;; Evaluation took:
;; 0.344 seconds of real time
;; 0.344194 seconds of total run time (0.336352 user, 0.007842 system)
;; [ Real times consist of 0.012 seconds GC time, and 0.332 seconds non-GC time. ]
;; [ Run times consist of 0.012 seconds GC time, and 0.333 seconds non-GC time. ]
;; 100.00% CPU
;; 1,275 forms interpreted
;; 6,922 lambdas converted
;; 468,546,272 bytes consed
