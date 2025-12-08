;; [[file:Day06.org::*Prologue - Setup][Prologue - Setup:1]]
;;;; Day06.lisp
;;;; 2025 AOC Day 6 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 06 Dec 2025 at 11:15
;;;; Finished:

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
;; Prologue - Setup:1 ends here

;; [[file:Day06.org::*Example Data][Example Data:1]]
(defparameter *example* (list "123 328  51 64 "
                              " 45 64  387 23 "
                              "  6 98  215 314"
                              "*   +   *   +  "))
;; Example Data:1 ends here

;; [[file:Day06.org::*Parser][Parser:1]]
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
;; Parser:1 ends here

;; [[file:Day06.org::parse-test][parse-test]]
(multiple-value-bind (digits operators) (parse-input *example*)
  (format t "Digits: ~S~%Operators: ~S~%" digits operators))
;; parse-test ends here

;; [[file:Day06.org::*Solution][Solution:1]]
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
;; Solution:1 ends here

;; [[file:Day06.org::part-1-test][part-1-test]]
(5a:test day06-1-test
  (5a:is (= 4277556 (day06-1 *example*))))
;; part-1-test ends here

;; [[file:Day06.org::operator-column-test][operator-column-test]]
(defun opstarts (input)
  (let* ((rows (1- (length input)))
         (op-string (nth rows input))
         (column-starts '()))

    ;; use op-string to determine column start and end
    (iter (for i below (length op-string))
      (when (not (char= #\space (char op-string i)))
        (push i column-starts)))        ; push column start

    (reverse column-starts)))

;; print the operator start list for *example*
(format t "Ops starts for *example*: ~a~%~%" (opstarts *example*))

;; compare the input to the operators (well just the first 70 characters)
(format t "First 70 chars of input: ~%~a~%~a~%~a~%~a~%~a~%"
        (subseq (first (uiop:read-file-lines *data-file*)) 0 70)
        (subseq (second (uiop:read-file-lines *data-file*)) 0 70)
        (subseq (third (uiop:read-file-lines *data-file*)) 0 70)        (subseq (fourth (uiop:read-file-lines *data-file*)) 0 70)
        (subseq (nth 4 (uiop:read-file-lines *data-file*)) 0 70))
;; operator-column-test ends here

;; [[file:Day06.org::cephlapod-parse][cephlapod-parse]]
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
;; cephlapod-parse ends here

;; [[file:Day06.org::lisp-063930][lisp-063930]]
(format t "Example: ~%~a" (cephalapod-parse *example*))
;; lisp-063930 ends here

;; [[file:Day06.org::arrange-cols][arrange-cols]]
(let ((arr (cephalapod-parse *example*)))
(iter (for col below (array-dimension arr 1))
    (collect
       (iter (for row below (array-dimension arr 0))
          (collect (car (aref arr row col)))))))
;; arrange-cols ends here

;; [[file:Day06.org::row2strings][row2strings]]
(let ((arr (cephalapod-parse *example*)))
       (iter (for col below (array-dimension arr 1))
          (print (car (aref arr col 0)))))
;; row2strings ends here

;; [[file:Day06.org::process-col][process-col]]
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
;; process-col ends here

;; [[file:Day06.org::*Solution][Solution:1]]
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
;; Solution:1 ends here

;; [[file:Day06.org::Solutions][Solutions]]
;; now solve the puzzle!
(time (format t "The answer to AOC 2025 Day 6 Part 1 is ~a~%"
              (day06-1 (uiop:read-file-lines *data-file*))))

 (time (format t "The answer to AOC 2025 Day 6 Part 2 is ~a~%"
 	      (day06-2 (uiop:read-file-lines *data-file*))))
;; Solutions ends here
