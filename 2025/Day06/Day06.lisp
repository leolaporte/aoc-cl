;;;; Day06.lisp
;;;; 2025 AOC Day 6 solution
;;;; Common Lisp solutions by Leo Laporte
;;;; Started: 06 Dec 2025 at 11:15
;;;; Finished: Mon Dec  8 15:45:41 2025

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :aoc.2025.day06
  (:use :cl :alexandria :iterate)      ; no prefix for these libraries
  (:local-nicknames                    ; short prefixes for these
   (:re :cl-ppcre)                     ; regex
   (:5a :fiveam)                       ; test framework
   (:sr :serapeum)                     ; CL extensions
   (:tr :trivia)))                     ; pattern matching

(in-package :aoc.2025.day06)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; automatic pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2025/Day06/input.txt"
  "Downloaded from the AoC problem set")


;; ----------------------------------------------------------------------------
;;                      --- Day 6: Trash Compactor ---
;;                              --- Part One ---
;;
;; LEO'S NOTES: Hmmm. Seems easy. Too easy. I've got a bad feeling about this.
;; I'll =PARSE-INPUT= into two values, an array of lists, each list will contain
;; all the integers from the columns, and then a list of operand strings. The
;; problem is practically done by then. I just apply each operand to its
;; respective column and sum the results. The list will be very long with the
;; provided data but I don't think 1000 digits lists are particularly
;; problematic. Let's see.
;;
;; ----------------------------------------------------------------------------

(defparameter *example* (list "123 328  51 64 "
                              " 45 64  387 23 "
                              "  6 98  215 314"
                              "*   +   *   +  "))


(sr:-> parse-input (list) (values array list))
(defun parse-input (input)
  "given a list of strings, return two values, an array of integers by column
and a list of strings representing operators"
  (let* ((rows (length input))
         (cols (length (sr:words (first input)))) ; actual numbers not spaces
         ;; an array of lists - each list contains all the integers from a column
         (digits (make-array cols :element-type 'list :initial-element nil)))

    (iter (for row below (1- rows))
      (for row-of-nums = (sr:words (nth row input))) ; chop it up first
      (iter (for col below cols)
        (setf (aref digits col)
              (push (parse-integer (nth col row-of-nums)) ; the number
                    (aref digits col)))))       ; that column's list

    ;; return array of lists of integers by column and list of operators
    (values digits (sr:tokens (lastcar input)))))

(sr:-> day06-1 (list) number)
(defun day06-1 (input)
  "given a list of strings of digits and a final string of operators, produce
the sum of the results of using each operator on the column above - most of the
work is done in the parsing"
  (multiple-value-bind (digits operators) (parse-input input)

    ;; now go through each column
    (iter (for col below (length digits))
      (summing
        ;; apply the apropriate operand
        (ecase (char (nth col operators) 0)
          (#\+ (apply #'+ (aref digits col)))
          (#\* (apply #'* (aref digits col))))))))

(5a:test day06-1-test
  (5a:is (= 4277556 (day06-1 *example*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Two ---
;;
;; LEO'S NOTES: Frickin' cephalpods. They're as bad as the lanternfish. So it
;; turns out that the columns are digit by digit. And spaces are
;; significant. This is going to require a completely different parser. I think
;; the final function can remain mostly the same as long as the parsing is done
;; correctly.
;;
;; In fact, I can pretty much use the same parser with slight
;; modifications. Instead of using =WORDS= to extract the digits I'll step
;; through the string a column at a time, replacing spaces with zeroes and
;; putting in the single digits.
;;
;; This is simple. Right? Right? Well not exactly. The integers to process are
;; combinations of all the digits in the column. So...
;;
;; #+begin_example
;; 64
;; 23
;; 314
;; +
;; #+end_example
;;
;; ends up being: 623 + 431 + 4. So I will have to chunk up the columns and then
;; index into them. A bit more complicated.
;;
;; The real question is how can I determine when to begin a new column? Is the
;; width consistent? Alas no. Examining the input file shows that operands are
;; usually separated by four columns but not always (unlike the example -
;; tricky!)
;;
;; Is a new column always begun by an operator? Can I use them as an anchor?
;; First, a test to see if the operators do really match the start of the
;; columns...
;;
;; ----------------------------------------------------------------------------

(sr:-> cephalapod-parse (list) array)
(defun cephalapod-parse (input)
  "given a list of strings, return a 2D array with number strings grouped by column"
  (let ((rows (length input))       ; number of lines will be rows in arr
        (column-starts '())             ; indices into line for each column
        (operators (lastcar input)))    ; the operators are the last line

    ;; use the operators to determine column starts
    (iter (for i below (length operators))
      (when (not (char= #\space (char operators i))) ; it's an operand
        (push i column-starts))) ; save the location of the operand
    (setf column-starts (reverse column-starts))

    ;; now fill the 2d array GRID with numbers by column groups
    (let ((grid (make-array (list rows (length column-starts))
                            :element-type 'list
                            :initial-element nil)))
      (iter (for line in input)
        (for row :from 0)
        (iter (for (start . rest) on column-starts)
          (for group :from 0)
          (setf (aref grid row group)
                (push (subseq line start (first rest))
                      (aref grid row group)))))
      grid)))

(sr:-> process-cols (list) number)
(defun process-cols (row)
  "Given a list of strings representing a row, with each number aligned with
spaces, and the last item representing an operator, return the result of
applying the operator to the digits formed by going from top to bottom column by
column."
  (let* ((operator-string (lastcar row))    ; last element of row is the op
         (digit-list (butlast row))         ; the list of number strings
         (cols (length (first digit-list))) ; length of each number string
         (numbers         ; list of the operands created by concatenating by col
           (iter (for c below cols)
             ;; collect the numbers by going down the columns
             (for digits =
                  (iter (for str in digit-list)
                    (collect (subseq str c (1+ c)) into pieces)
                    (finally (return (apply #'concatenate 'string pieces)))))
             (collect (parse-integer digits :junk-allowed t)))))

    (setf numbers (remove nil numbers)) ; the line of spaces at the end is nil
    (ecase (char operator-string 0)
      (#\+ (apply #'+ numbers))
      (#\* (apply #'* numbers)))))

(5a:test process-cols-test
  (5a:is (= (process-cols '("123 "
                            " 45 "
                            "  6 "
                            "*   ")) (* 1 24 356)))
  (5a:is (= (process-cols '("328 "
                            "64  "
                            "98  "
                            "+   ")) (+ 369 248 8)))
  (5a:is (= (process-cols '(" 51 "
                            "387 "
                            "215 "
                            "*   ")) (* 32 581 175)))
  (5a:is (= (process-cols '("64 "
                            "23 "
                            "314"
                            "+  ")) (+ 623 431 4))))

(sr:-> day06-2 (list) number)
(defun day06-2 (input)
  (let* ((grid (cephalapod-parse input)) ; make 2D array of input
         (column-list ; transpose it into a list of lists of strings
           (iter (for col below (array-dimension grid 1))
             (collect
                 (iter (for row below (array-dimension grid 0))
                   (collect (car (aref grid row col))))))))

    ;; process each number string by adding digits column by column
    (iter (for column in column-list)
      (summing (process-cols column)))))

(5a:test day06-2-test
  (5a:is (= 3263827 (day06-2 *example*))))

;; ----------------------------------------------------------------------------
;; now solve the puzzle!

(time (format t "The answer to AOC 2025 Day 6 Part 1 is ~a~%"
              (day06-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2025 Day 6 Part 2 is ~a~%"
              (day06-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on a 2023 MacBook Pro M3 Max with 64GB RAM and Tahoe 26.1
;; ----------------------------------------------------------------------------

;; The answer to AOC 2025 Day 6 Part 1 is 5873191732773
;; Evaluation took:
;; 0.002 seconds of real time
;; 0.002105 seconds of total run time (0.002067 user, 0.000038 system)
;; 100.00% CPU
;; 576,512 bytes consed

;; The answer to AOC 2025 Day 6 Part 2 is 11386445308378
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001045 seconds of total run time (0.001025 user, 0.000020 system)
;; 100.00% CPU
;; 1,505,872 bytes consed
