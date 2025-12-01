;;;; Day02.lisp
;;;; 2023 AOC Day 02 solution
;;;; 9 Dec 2023 Leo Laporte

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day02
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)))

(in-package :day02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/day02/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 2: Cube Conundrum ---
--- Part One ---

Each time you play this game, he will hide a secret number of cubes of
each color in the bag, and your goal is to figure out information
about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf
will reach into the bag, grab a handful of random cubes, show them to
you, and then put them back in the bag. He'll do this a few times per
game.

Determine which games would have been possible if the bag had been
loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What
is the sum of the IDs of those games?

NOTE: The order of the colors is random.  We'll need a test -
possible? - that determines if a game is possible given the
parameters. Better yet, possible should return the game number if
possible, 0 if not.

It's impossible if: the number of cubes of a given color is higher
than the max.

So it's sufficient to find the highest number seen of each color in a
game. Then compare that number to the max for the possible? test.
----------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defparameter *max-red* 12)
(defparameter *max-green* 13)
(defparameter *max-blue* 14)

(defun possible (game-string)
  "given a game string, return the game number if the game is possible,
 0 if not"
  (let ((game-list (parse-game game-string)))
    (if (and
         (<= (second game-list) *max-red*)
         (<= (third game-list) *max-green*)
         (<= (fourth game-list) *max-blue*))
        (first game-list) ; return the game number
        0)))              ; or 0 if it's not possible

(5a:test possible-test
  (5a:is (equal (possible (first *test-data*)) 1))
  (5a:is (equal (possible (second *test-data*)) 2))
  (5a:is (equal (possible (third *test-data*)) 0))
  (5a:is (equal (possible (fourth *test-data*)) 0))
  (5a:is (equal (possible (fifth *test-data*)) 5)))

(defun parse-game (input-string)
  "returns a list of four integers: game number, max red, max green, max
blue stones seen"
  (let ((game-number
          (parse-integer
           (elt
            (nth-value 1 (re:scan-to-strings "Game (\\d+):" input-string))
            0)))

        (max-reds (max-cubes
                   (re:all-matches-as-strings "\\d+ red" input-string)))

        (max-greens (max-cubes
                     (re:all-matches-as-strings "\\d+ green" input-string)))

        (max-blues (max-cubes
                    (re:all-matches-as-strings "\\d+ blue" input-string))))


    (list game-number max-reds max-greens max-blues)))

(5a:test parse-game-test
  (5a:is (equal (parse-game (first *test-data*)) '(1 4 2 6)))
  (5a:is (equal (parse-game (second *test-data*)) '(2 1 3 4)))
  (5a:is (equal (parse-game (third *test-data*)) '(3 20 13 6)))
  (5a:is (equal (parse-game (fourth *test-data*)) '(4 14 3 15)))
  (5a:is (equal (parse-game (fifth *test-data*)) '(5 6 3 2))))

(defun max-cubes (cubes)
  "given a list of cube counts, return the highest number"
  (apply #'max
         (mapcar #'(lambda (cube)
                     (parse-integer (re:scan-to-strings "\\d+" cube)))
                 cubes)))

(5a:test max-cubes-test
  (5a:is (equal (max-cubes '("29 blue" "4 blue" "2 blue")) 29))
  (5a:is (equal (max-cubes '("0 blue" "15 blue" "41 blue")) 41))
  (5a:is (equal (max-cubes '("4 blue")) 4)))

(defun Day02-1 (list-of-strings)
  (apply #'+ (mapcar #'possible list-of-strings)))

(5a:test Day02-1-test
  (5a:is (equal (Day02-1 *test-data*) 8)))

#| -----------------------------------------------------------------------------
--- Part Two ---

As you continue your walk, the Elf poses a second question: in each
game you played, what is the fewest number of cubes of each color that
could have been in the bag to make the game possible?

The power of a set of cubes is equal to the numbers of red, green, and
blue cubes multiplied together.

NOTE: This should be easy using the parse-game function. The second,
third, and fourth items in the list returned are the max number of red
green and blue cubes. Walk the list, multiplying those three values
together and adding the result.
------------------------------------------------------------------------------|#

(defun game-power (input-string)
  (let ((game (parse-game input-string))) ; the cube list
    (* (second game) (third game) (fourth game))))

(5a:test game-power-test
  (5a:is (equal (game-power (first *test-data*)) 48))
  (5a:is (equal (game-power (second *test-data*)) 12))
  (5a:is (equal (game-power (third *test-data*)) 1560))
  (5a:is (equal (game-power (fourth *test-data*)) 630))
  (5a:is (equal (game-power (fifth *test-data*)) 36)))

(defun Day02-2 (list-of-strings)
  (apply #'+ (mapcar #'game-power list-of-strings)))

(5a:test Day02-2-test
  (5a:is (equal (Day02-2 *test-data*) 2286)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 02 Part 1 is ~a"
              (Day02-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 02 Part 2 is ~a"
              (day02-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 02 Part 1 is 2776
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001542 seconds of total run time (0.001403 user, 0.000139 system)
;; 200.00% CPU
;; 327,536 bytes consed

;; The answer to AOC 2023 Day 02 Part 2 is 68638
;; Evaluation took:
;; 0.001 seconds of real time
;; 0.001436 seconds of total run time (0.001353 user, 0.000083 system)
;; 100.00% CPU
;; 262,080 bytes consed
