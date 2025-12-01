;;;; Day13.lisp
;;;; 2023 AOC Day 13 solution
;;;; Leo Laporte
;;;; 16 February 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia :alexandria))

(defpackage :day13
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)
   (:tr :trivia)
   (:al :alexandria)
   (:5a :fiveam)))   ; testing

(in-package :day13)
(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/day13/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 13: Point of Incidence ---
--- Part One ---

"To find the reflection in each pattern, you need to find a perfect
reflection across either a horizontal line between two rows or across
a vertical line between two columns.

To summarize your pattern notes, add up the number of columns to the
left of each vertical line of reflection; to that, also add 100
multiplied by the number of rows above each horizontal line of
reflection."

LEO'S NOTES: Well the first thing I noticed is that UNLIKE the example
data, the actual data features rectangles of VARYING sizes. I'll have
to deal with each rectangle individually.

For each rectangle search for duplicated rows, if no rows, then
columns. Then count the rows to the left or above (1- mirror row/cols
in other words). For now each rect will be a list of equal length
strings.

Wait. This is more complicated than finding duplicate rows. I have to
find entire duplicated SIDES. It has to be able to fold at the row or
col and match up.

So, for example, if we think rows 2-3 in the first rect are the fold
line, we have to make sure that 1-4 and 0-5 are also dupes. 0-5 do NOT
match, so it's not a fold line.

So...

1. search for duplicate side-by-side rows (I'll call them fold
candidates).

2. when I have one check the rows on either side. If match, continue
until either there's no match, or I reach the edge on either side. If
I reach the edge then candidate-fold is actual-fold and I return the
index of the first fold row +1 (the rows are zero-based).

3. If rows don't match, do the same with cols (rotate rect and repeat)

FIXES: OK sneaky. I've found at least one rectangle with three folds -
only ; one is a mirror so I have to keep checking for multiple folds
in any given rectangle.

Oh and also apparently some people had NO mirrors in one or more rects
- in which case the result is 0. Which of course doesn't affect the
result - but it shouldn't cause an error by comparing nil to 0. So
I'll fix that.

----------------------------------------------------------------------------|#

(defparameter *test-data*
  '("#.##..##."
    "..#.##.#."
    "##......#"
    "##......#"
    "..#.##.#."
    "..##..##."
    "#.#.##.#."
    ""
    "#...##..#"
    "#....#..#"
    "..##..###"
    "#####.##."
    "#####.##."
    "..##..###"
    "#....#..#"))

(defun parse-input (los)
  "turns a list of strings into a list of lists of strings, each
representing a complete rectangle, creating a new rectangle at every
empty string"
  (let ((rect '())
        (rects '()))

    (dolist (l los)
      (if (equal l "")
          (progn
            (push (reverse rect) rects)
            (setf rect '()))
          (push l rect)))

    (reverse (push (reverse rect) rects))))

(defparameter *rects* (parse-input *test-data*))
(defparameter *rect1* (first *rects*))
(defparameter *rect2* (second *rects*))

(defun rotate-rect (rect)
  "turns a list of strings clockwise 90 degrees"
  (let ((rotated-rect '())
        (new-row ""))

    (iter (for col below (length (first rect)))
      (iter (for row below (length rect))
        (setf new-row
              (concatenate 'string
                           new-row
                           (string (char (nth row rect) col)))))
      (push new-row rotated-rect)
      (setf new-row ""))
    (reverse rotated-rect)))

(5a:test rotate-rect-test
  (5a:is (equal (rotate-rect (list "abc" "def" "hij"))
                (list "adh" "bei" "cfj")))
  (5a:is (equal (rotate-rect (rotate-rect *rect1*)) *rect1*)))

(defun find-mirror-exact (rect)
  "given a rect return the number of rows above the mirror
line (which includes the top side of the mirror), otherwise nil"
  (iter (for row below (length rect))   ; for each row in rect
    ;; is it a mirror?
    (when (string= (nth row rect) (nth (1+ row) rect)) ; lines match
      (if (zerop row)                                  ; first row?
          (return-from find-mirror-exact 1)            ; then done
          ;; otherwise check surrounding rows
          (when (iter
                  (for up from (1- row) downto 0)
                  (for down from (+ 2 row) below (length rect))
                  (always (string=
                           (nth up rect) (nth down rect))))
            ;; we have a mirror - return the row #
            (return-from find-mirror-exact (1+ row))))))

  ;; We've checked every line in the rect and there's no mirror so...
  nil) ; return false

(5a:test find-mirror-exact-test
  (5a:is-false (find-mirror-exact *rect1*))
  (5a:is (= (find-mirror-exact (rotate-rect *rect1*)) 5))
  (5a:is (= (find-mirror-exact *rect2*) 4))
  (5a:is-false (find-mirror-exact (rotate-rect *rect2*))))

(defun rows-before-mirror (rect)
  "given a rect return 100 * the number lines above or 1 * the number
of lines to the left of the mirror fold"
  (Let ((row (find-mirror-exact rect)))

    (if row
        (* 100 row )                               ; horiz mirror
        (or (find-mirror-exact (rotate-rect rect)) ; vertical mirror
            0))))                    ; no mirror in either direction

(5a:test rows-before-mirror-test
  (5a:is (= 5 (rows-before-mirror *rect1*)))
  (5a:is (= 400 (rows-before-mirror *rect2*))))

(defun day13-1 (los)
  (let ((rects (parse-input los)))
    (iter (for r in rects)
      (sum (rows-before-mirror r)))))

(5a:test day13-1-test
  (5a:is (= (day13-1 *test-data*) 405)))

#| ----------------------------------------------------------------------------
--- Part Two ---

"Upon closer inspection, you discover that every mirror has exactly
one smudge: exactly one . or # should be the opposite type.

In each pattern, you'll need to locate and fix the smudge that causes
a _different reflection line to be valid_. (The old reflection line
won't necessarily continue being valid after the smudge is fixed.)

In each pattern, fix the smudge and find the different line of
reflection. What number do you get after summarizing the new
reflection line in each pattern in your notes?"

LEO'S NOTES:

Well it doesn't look like there are any hints as to where that smudge
may be. So what's the best way to find it?

The smudge can be anywhere on a mirror. So I need to test for off by
one mirror lines. It's really not so different from part 1 - I just
have to relax the rules a bit for finding possible fold lines.

Refactoring part 1 so I can use a looser string compare function. And
creating a new off-by-exactly-one string compare.

The trick here is that the relaxed string compare can only be used
once in any given rectangle. So I need a closure that keeps track of
the state of the compare - once relaxed compare has been used it must
revert to an exact compare until the next rectangle.

FIXES: Hmm. The number is too high. I guess I really will have to
eliminate mirrors from part 1. (Remember that line: 'fix the smudge
that causes a DIFFERENT reflection line to be valid'?) Drat.

It's getting ugly up there. Instead of continuing to Frankenstein my
solution from Part 1 I'm going to leave it as is and create new
definitions for part 2 below.

----------------------------------------------------------------------------|#

(defparameter *rsc*
  ;; relaxed string count to be used as a closure - i.e.  called with
  ;; funcall to preserve state, e.g.

  ;; (funcall *rsc* str1 str2 &optional mode)

  ;; Initial mode is 'INEXACT, returns true if strings differ by no
  ;; more than one character, but if the strings do differ by one,
  ;; future calls require an exact match until the function is reset
  ;; by adding the optional parameter 'INEXACT (or force exact string
  ;; matching with 'EXACT anytime.)
  (let ((state 'INEXACT))               ; state persists between calls
    #'(lambda (str1 str2 &optional (mode))

        (when mode (setf state mode))   ; override default 'INEXACT mode?

        ;; if either string is nil return false (this is what string= does)
        (if (or (null str1) (null str2))
            nil

            ;; otherwise count the number of non-matching characters
            (let ((matches
                    (iter (for c below (length str1))
                      (sum (if (equal (char str1 c) (char str2 c)) 0 1)))))

              (cond
                ;; state is 'INEXACT and it's off by one
                ((and (equal state 'INEXACT) (= matches 1))
                 (setf state 'EXACT) ; from now on the match must be exact
                 t)                  ; but we'll let it go just this once

                (t (zerop matches)))))))) ; return true if string=

;; This is a fun test. It starts with an inexact match but as soon as
;; we get a one-off match it becomes exact, until I reset it with
;; 'INEXACT
(5a:test *rsc*-test
  (5a:is-true (funcall *rsc* "abc" "abc" 'INEXACT)) ; exact match
  (5a:is-false (funcall *rsc* "abc" "ade"))         ; no match
  (5a:is-false (funcall *rsc* "abc" nil))           ; no match
  (5a:is-true (funcall *rsc* "abc" "abd")) ; one off, so now exact
  (5a:is-false (funcall *rsc* "abc" "abd"))         ; no match!
  (5a:is-true (funcall *rsc* "abc" "abd" 'INEXACT)) ; reset, match
  (5a:is-false (funcall *rsc* "abc" "abd")))        ; again, no match

(defun find-mirror-inexact (rect &optional (start-row 0))
  "returns the mirror line in a rect using an inexact match"
  (iter (for row from start-row below (length rect))   ; for each row in rect
    ;; is it a mirror?
    (when (funcall *rsc* (nth row rect) (nth (1+ row) rect) 'INEXACT)
      (if (zerop row)                   ; first row?
          (return-from find-mirror-inexact 1)    ; then done
          ;; otherwise check surrounding rows
          (when (iter                   ; do surrounding rows match?
                  (for up from (1- row) downto 0)
                  (for down from (+ 2 row) below (length rect))
                  (always (funcall *rsc*
                                   (nth up rect) (nth down rect))))
            ;; we have a mirror - return the row #
            (return-from find-mirror-inexact (1+ row))))))
  nil)

(5a:test find-mirror-inexact-test
  (5a:is (= 3 (find-mirror-inexact *rect1*)))
  (5a:is (= 1 (find-mirror-inexact *rect2*))))

(defun rows-before-smudged-mirror (rect)
  "returns the mirror row using the more relaxed string=, ensures that
the smudged row is different from the exact mirror row in part 1"
  (labels ((smudged-mirror-row (rect)
             ;; searches for a mirror row not seen in part 1
             ;; returns nil if none is found
             (let ((exact (find-mirror-exact rect))      ; part 1 mirror
                   (inexact (find-mirror-inexact rect))) ; pssble smudged mir
               (cond ((null inexact) nil) ; couldn't find a mirror at all
                     ((equal exact inexact) ; if they're the same
                      (find-mirror-inexact rect exact)) ; try again
                     (t inexact))))) ; else return smudged mirror row

    (let ((result (smudged-mirror-row rect)))
      (if result
          (* 100 result)                      ; got a horizontal row
          (or (smudged-mirror-row (rotate-rect rect)) ; got vert row
              0)))))                                  ; got nuthin'

(5a:test rows-before-smudged-mirror-test
  (5a:is (= 300 (rows-before-smudged-mirror *rect1*)))
  (5a:is (= 100 (rows-before-smudged-mirror *rect2*))))

(defun day13-2 (los)
  (let ((rects (parse-input los)))
    (iter (for r in rects)
      (sum (rows-before-smudged-mirror r)))))

(5a:test day13-2-test
  (5a:is (= (day13-2 *test-data*) 400))
  (5a:is (= (day13-2 *other-rects*) 2800)))

(time (format t "The answer to AOC 2023 Day 13 Part 1 is ~a"
              (day13-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 13 Part 2 is ~a"
              (day13-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------


;; The answer to AOC 2023 Day 13 Part 1 is 33735
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.000765 seconds of total run time (0.000593 user, 0.000172 system)
;; 100.00% CPU
;; 917,216 bytes consed

;; The answer to AOC 2023 Day 13 Part 2 is 38063
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000753 seconds of total run time (0.000730 user, 0.000023 system)
;; 100.00% CPU
;; 851,792 bytes consed
