;;;; Day04.lisp
;;;; 2024 AOC Day 04 solution
;;;; Common Lisp solutions by Leo Laporte (with lots of help)
;;;; Started: 3 Dec 2024 2100
;;;; Finished: 3 Dec 2024 2330

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day04
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :day04)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2024/day04/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

"This word search allows words to be horizontal, vertical, diagonal,
written backwards, or even overlapping other words. It's a little
unusual, though, as you don't merely need to find one instance of XMAS
- you need to find all of them. Here are a few ways XMAS might appear,
where irrelevant characters have been replaced with .

Take a look at the little Elf's word search. How many times does XMAS appear?"

LEOS NOTES: Streamed this solving session live again today.
https://www.youtube.com/live/9tc1AsWCkwQ?si=Ddvhu0XFRuiddKqV

Thanks to cyphase and Paul Holder for their help!

---------------------------------------------------------------------------- |#

(defparameter *example*
  '("MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"))

(defun parse-xmas-strings (los)
  "turn the supplied list of strings into a hash with the keys being
 the x,y position stored as (cons x y) and the values being the
 character at that point"
  (let ((string-hash (make-hash-table :test 'equal))
        (width (length (first los)))
        (height (length los)))

    (iter (for x-posn from 0 below width)
      (iter (for y-posn from 0 below height)
        (setf (gethash (cons x-posn y-posn) string-hash)
              (subseq (nth y-posn los) x-posn (1+ x-posn)))))

    string-hash))

(defun ph (hash)
  "a little utility to display a hash table"
  (maphash (lambda (key value)
             (format t "~%~a => ~a~%" key value)) hash))

(defparameter *directions*
  (list
   (cons -1 -1)                         ; NW
   (cons 0 -1)                          ; N
   (cons 1 -1)                          ; NE
   (cons 1 0)                           ; E
   (cons 1 1)                           ; SE
   (cons 0 1)                           ; S
   (cons -1 1)                          ; SW
   (cons -1 0))                         ; W
  "the eight directions, to be added to the point")

(defun add-point (posn dir)
  "given a point and a direction, returns the position after moving
 one square in that direction"
  (cons (+ (car posn) (car dir)) (+ (cdr posn) (cdr dir))))

(defun xmas-from-here? (posn dir string-hash)
  "returns true if I can get an XMAS in the given direction"
  (and (equal "X" (gethash posn string-hash)) ; stops right away if not X
       (equal "M" (gethash (add-point posn dir) string-hash))
       (equal "A" (gethash (add-point (add-point posn dir) dir) string-hash))
       (equal "S" (gethash (add-point (add-point (add-point posn dir) dir) dir) string-hash))))

(5a:test xmas-from-here?-t
  (let ((shash (parse-xmas-strings *example*)))
    (5a:is-true (xmas-from-here? (cons 5 0) (cons 1 0) shash))
    (5a:is-false (xmas-from-here? (cons 4 2) (cons -1 1) shash))
    (5a:is-true (xmas-from-here? (cons 9 9) (cons -1 -1) shash))
    (5a:is-false (xmas-from-here? (cons 9 9) (cons 1 0) shash))))

(defun count-xmas-at-a-point (posn string-hash)
  "given an x,y coordinate on a grid, return the number of XMASs from
 that point"
  (iter (for d in *directions*)
    (summing (if (xmas-from-here? posn d string-hash) 1 0))))

(5a:test count-xmas-at-a-point-t
  (let ((shash (parse-xmas-strings *example*)))
    (5a:is (= 0 (count-xmas-at-a-point (cons 0 0) shash)))
    (5a:is (= 2 (count-xmas-at-a-point (cons 9 9) shash)))
    (5a:is (= 2 (count-xmas-at-a-point (cons 9 3) shash)))
    (5a:is (= 1 (count-xmas-at-a-point (cons 0 4) shash)))))

(defun Day04-1 (los)
  "puts it all together, going through each point on the provided grid
and counts the total number of XMASs possible."
  (let ((shash (parse-xmas-strings los))
        (width (length (first los)))
        (height (length los)))

    (iter (for x-posn from 0 below width)
      (summing (iter (for y-posn from 0 below height)
                 (summing (count-xmas-at-a-point (cons x-posn y-posn) shash)))))))

(5a:test Day04-1-test
  (5a:is (= 18 (Day04-1 *example*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

Now "find two MAS in the shape of an X. How many times does an X-MAS appear?"

LEOS NOTES: Here are the four legal positions:

M.S
.A.
M.S

S.M
.A.
S.M

M.M
.A.
S.S

S.S
.A.
M.M

With a little help from Cyphase I realized I could use a string
compare to solve this part!

---------------------------------------------------------------------------- |#

(defparameter *masses* '("MSMS" "SMSM" "MMSS" "SSMM")
  "these are the valid letters at the four corners: NW NE SW and SE")

(defun mas-from-here? (posn string-hash)
  "returns true if we can reach an X-MAS from current posn"
  (when (not (equal (gethash posn string-hash) "A"))
    (return-from mas-from-here? nil)) ; has to start from an A square

  (let ((test-str ; gather the chars at the four corners going clockwise from NW
          (concatenate 'string
                       (gethash (add-point posn (cons -1 -1)) string-hash)  ; NW
                       (gethash (add-point posn (cons 1 -1)) string-hash)   ; NE
                       (gethash (add-point posn (cons -1 1)) string-hash)   ; SW
                       (gethash (add-point posn (cons 1 1)) string-hash)))) ; SE

    (member test-str *masses* :test 'equal))) ; does it match the valid combos?

(defun Day04-2 (los)
  "count the number of X-MASs in the grid described by the provided list
of strings"
  (let ((shash (parse-xmas-strings los))
        (width (length (first los)))
        (height (length los)))

    (iter (for x-posn from 0 below width)
      (summing (iter (for y-posn from 0 below height)
                 (summing
                   (if (mas-from-here? (cons x-posn y-posn) shash) 1 0)))))))

(5a:test Day04-2-test
  (5a:is (= 9 (Day04-2 *example*))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2024 Day 04 Part 1 is ~a"
              (Day04-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2024 Day 04 Part 2 is ~a"
              (Day04-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on an M4 Pro Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2024 Day 04 Part 1 is 2530
;; Evaluation took:
;; 0.007 seconds of real time
;; 0.007721 seconds of total run time (0.007573 user, 0.000148 system)
;; 114.29% CPU
;; 4,329,440 bytes consed

;; The answer to AOC 2024 Day 04 Part 2 is 1921
;; Evaluation took:
;; 0.003 seconds of real time
;; 0.003232 seconds of total run time (0.003202 user, 0.000030 system)
;; 100.00% CPU
;; 4,106,864 bytes consed

;; --------Part 1--------   --------Part 2--------
;; Day       Time   Rank  Score       Time   Rank  Score
;; 4   01:48:58  15379      0   02:25:38  14896      0
;; 3   00:50:37  14142      0   01:58:36  16752      0
;; 2   00:57:52  13855      0   01:48:11  12571      0
;; 1   00:47:09   9351      0   01:10:39   9932      0
