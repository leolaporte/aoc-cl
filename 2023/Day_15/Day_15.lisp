;;;; Day15.lisp
;;;; 2023 AOC Day 15 solution
;;;; Leo Laporte
;;;; 26-27 February 2024

;; -------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day15
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)   ; regular expressions
   (:tr :trivia)     ; pattern matching
   (:5a :fiveam)))   ; testing

(in-package :day15)
(setf 5a:*run-test-when-defined* t)          ; test as we go
(declaim (optimize (debug 3)))               ; max debugging info
;; (declaim (optimize (speed 3) (safety 0))) ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_15/input.txt"
  "Downloaded from the AoC problem set")

#| -------------------------------------------------------------------
--- Day 15: Lens Library ---
--- Part One ---

"The HASH algorithm is a way to turn any string of characters into a
single number in the range 0 to 255. To run the HASH algorithm on a
string, start with a current value of 0. Then, for each character in
the string starting from the beginning:

Determine the ASCII code for the current character of the string.
Increase the current value by the ASCII code you just determined.  Set
the current value to itself multiplied by 17.  Set the current value
to the remainder of dividing itself by 256.  After following these
steps for each character in the string in order, the current value is
the output of the HASH algorithm.

Run the HASH algorithm on each step in the initialization
sequence. What is the sum of the results? (The initialization sequence
is one long line; be careful when copy-pasting it.)

LEO'S NOTES: Nothing to say. It's about as simple as a math problem
can get. Which makes me very nervous for part 2.

------------------------------------------------------------------- |#

(defparameter *sample*
  "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defun parse-sequence (str)
  "turn a comma separated line into a list of strings"
  (re:split "," str))

(defun munge-char (val chr)
  "given a char, return the hash value for the character"
  (mod (* (+ val (char-code chr)) 17) 256))

(5a:test munge-chr-test
  (5a:is (= 200 (munge-char 0 #\H)))
  (5a:is (= 153 (munge-char 200 #\A)))
  (5a:is (= 172 (munge-char 153 #\S)))
  (5a:is (= 52 (munge-char 172 #\H))))

(defun munge-str (str)
  "given a string, return the processed hash"
  (let ((result 0))
    (iter (for c in (coerce str 'list))
      (setf result (munge-char result c))
      (finally (return result)))))

(5a:test munge-string-tst
  (5a:is (= 52 (munge-str "HASH"))))

(defun Day15-1 (line)
  (let ((init-str (parse-sequence line)))
    (apply #'+ (mapcar #'munge-str init-str))))

(5a:test Day15-1-test
  (5a:is (= 1320 (Day15-1 *sample*))))

#| -------------------------------------------------------------------
--- Part Two ---

yada yada yada - lots of detailed instructions which boil
down to: interpret the provided data as a series of
lenses. Store the lenses then return the total focusing
power of the resulting configuration?

LEO'S NOTES: This is mostly a problem in precisely following
written instructions. I'll need a data structure to simulate
the boxes into which the lenses are stored. Each box has
ordered slots. It's important in subsequent operations to
preserve the order of the slots.

A box might look like this:  Box 3: [ot 7] [ab 5] [pc 6]

So a hash table with the box number as key and a list of
lenses as the value): 3 => (list (cons ot 7) (cons ab
5) (cons pc 6)

We'll call our hash table boxes and for each key (0...24)
there will be a list of lenses stored in the order they're
processed as (cons label focal-length)

------------------------------------------------------------------- |#

(defparameter *lens-regexp*
  (re:create-scanner "([a-z]+)(-|=)(\\d?)")
  "using cl-ppcre library: pre-compiled regular expression for
lenses containing three groups: 2 lower case characters,
 followed by either - or =, followed by an optional single
 digit representing the focal length")

(defun remove-lens (lens boxes)
  "removes a given lens from the boxes hash without
disturbing the order of the lenses in the box"
  (let* ((boxnum (munge-str lens))        ; which box?
         (lenses (gethash boxnum boxes))) ; that box's lens list

    (setf (gethash boxnum boxes)
          ;; modified lens list (does nothing if no match)
          (remove lens lenses :key #'car :test #'equalp)))

  boxes) ; return modified hash table

(defun insert-lens (lens focal boxes)
  "inserts a lens into a box - if lens already exists
updates its focal length without changing the lens
order, if lens is new adds it to the end of the box"
  (let* ((boxnum (munge-str lens))       ; which box?
         (lenses (gethash boxnum boxes)) ; that box's lens list
         (pos (position lens lenses      ; the lens index in list
                        :key #'car :test #'equalp)))

    (if pos ; lens exists
        ;; change focal length of existing lens
        (setf (cdr (nth pos lenses)) focal)
        ;; else add lens to end
        (setf (gethash boxnum boxes)
              (append lenses (list (cons lens focal))))))

  boxes)

(defun store-lenses (lenses)
  "given a list of lens strings store each in a hash-table of
boxes, returns boxes hash-table"
  (let ((boxes (make-hash-table :size 25)))

    (dolist (lens lenses)
      ;; parse the lens string into label, operation,
      ;; and focal length
      (re:register-groups-bind (label op focal)
          (*lens-regexp* lens)

        (setf boxes
              (if (equalp op "-")
                  (remove-lens label boxes)
                  (insert-lens label
                               (parse-integer focal)
                               boxes)))))
    boxes))

(defun focus (boxes)
  "returns the total focal length of all the lenses in the hash
table boxes"
  (iter (for (box lenses) in-hashtable boxes)
    (summing  ; add each box's focal length
     (iter (for slot from 1 to (length lenses))
       (summing ; add all the focal lengths in the box
        (* (1+ box) slot (cdr (nth (1- slot) lenses))))))))

(defun Day15-2 (line)
  "given a string containing comma separated strings
describing a set of lenses, return the total focal
length of the lenses"
  (let* ((lenses (parse-sequence line))
         (boxes (store-lenses lenses)))

    (focus boxes)))

(5a:test Day15-2-test
  (5a:is (= 145 (Day15-2 *sample*))))

;; now solve the puzzle!
(let ((data (uiop:read-file-line *data-file*)))

  (time (format t "The answer to AOC 2023 Day 15 Part 1 is ~a"
                (day15-1 data)))

  (time (format t "The answer to AOC 2023 Day 15 Part 2 is ~a"
                (day15-2 data))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 15 Part 1 is 511257
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000995 seconds of total run time (0.000994 user, 0.000001 system)
;; 100.00% CPU
;; 851,792 bytes consed

;; The answer to AOC 2023 Day 15 Part 2 is 239484
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001902 seconds of total run time (0.001901 user, 0.000001 system)
;; 200.00% CPU
;; 1,572,576 bytes consed
