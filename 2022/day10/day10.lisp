;;;; Day10.lisp
;;;; 2022 AOC Day 10 solution
;;;; Leo Laporte, 10 Dec 2022

;; ----------------------------------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre))

(defpackage :day10
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam))) ; for inline testing

(in-package :day10)

(setf fiveam:*run-test-when-defined* t) ; test when compiling test code (for quick iteration)
(declaim (optimize (debug 3)))          ; max debugging info

(defparameter *data-file* "~/cl/AOC/2022/day10/input.txt")  ; supplied data from AoC.
(defparameter *test-file* "~/cl/AOC/2022/day10/test.txt")   ; test info
ggxf
(defun parse-inst (inst)
  (let ((i (re:split " " inst)))
    (if (equal "noop" (first i))
	"noop"
	(parse-integer (second i)))))

(defun parse-insts (inst-list)
  (mapcar #'parse-inst inst-list))

#| ----------------------------------------------------------------------------------------------------
--- Day 10: Cathode-Ray Tube ---
--- Part One ---

Start by figuring out the signal being sent by the CPU. The CPU has a single register, X,
which starts with the value 1. It supports only two instructions:

addx V takes two cycles to complete. After two cycles, the X register is increased
by the value V. (V can be negative.)

noop takes one cycle to complete. It has no other effect.

For now, consider the signal strength (the cycle number multiplied by the value of the X register)
during the 20th cycle and every 40 cycles after that (that is, during the 20th, 60th, 100th, 140th,
180th, and 220th cycles).

Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles. What is the
sum of these six signal strengths?
---------------------------------------------------------------------------------------------------- |#

;; we can do the whole thing in a single do loop - love these little factories
(defun execute-instructions (insts)
  (do
   ;; local variables
   ((i insts (rest i))                               ; step through instructions
    (cycle 0)                                        ; current clock cycle
    (X 1)                                            ; current value of register X
    (signal-strength 0))                             ; total signal strength

   ;; done?
   ((or (null insts) (> cycle 220)) signal-strength) ; all done, return signal-strength

    ;; body
    (cond
      ((equalp (first i) "noop")                      ; it's a noop
       (incf cycle)                                   ; add a cycle
       (when (time-to-save-signal? cycle)             ; time to save signal?
	 (incf signal-strength (* cycle X))))         ; yes

      (t (dotimes (tick 2)                            ; it's addx so burn two cycles
	   (incf cycle)                               ; one at a time
	   (when (time-to-save-signal? cycle)         ; check for signal save
	     (incf signal-strength (* cycle X))))
	 (incf X (first i))))))                       ; after two cycles, addx

(defun time-to-save-signal? (cycle)
  (zerop (mod (- cycle 20) 40)))

(5a:test time-to-save-signal?-test
  (5a:is-true (time-to-save-signal? 20))
  (5a:is-true (time-to-save-signal? 60))
  (5a:is-true (time-to-save-signal? 100))
  (5a:is-true (time-to-save-signal? 140))
  (5a:is-true (time-to-save-signal? 180))
  (5a:is-true (time-to-save-signal? 220))
  (5a:is-false (time-to-save-signal? 221))
  (5a:is-false (time-to-save-signal? 10)))

(defun day10-1 (f)
  (execute-instructions (parse-insts (uiop:read-file-lines f))))

(5a:test day10-1-test
  (5a:is (= 13140 (day10-1 *test-file*))))

#| ----------------------------------------------------------------------------------------------------
--- Part Two ---

You count the pixels on the CRT: 40 wide and 6 high. This CRT screen draws the top row of pixels
left-to-right, then the row below that, and so on. The left-most pixel in each row is in position 0,
and the right-most pixel in each row is in position 39.

Like the CPU, the CRT is tied closely to the clock circuit: the CRT draws a single pixel during each
cycle.

"by carefully timing the CPU instructions and the CRT drawing operations, you should be able to
determine whether the sprite is visible the instant each pixel is drawn. If the sprite is positioned
such that one of its three pixels is the pixel currently being drawn, the screen produces a lit pixel
(#); otherwise, the screen leaves the pixel dark (.).

Render the image given by your program. What eight capital letters appear on your CRT?"

NOTE: the only stumbling block here was I confused cycles with pixels. And got a one off error.
Changing the variable name helped me see the error of my ways.

---------------------------------------------------------------------------------------------------- |#

(defun render-screen (insts)
  (do
   ;; local variables
   ((i insts (rest i))            ; step through instructions
    (pixel 0)                     ; current pixel, resets every 40 dots
    (sprite 1))                   ; current value of sprite

   ;; done?
   ((null i) 'DONE)               ; all done

    ;; body
    (cond
      ((equalp (first i) "noop")                 ; it's a noop
       (setf pixel (draw-pixel pixel sprite)))   ; display a pixel

      ((numberp (first i))                       ; it's addx
       (dotimes (tick 2)                         ; print two pixels
	 (setf pixel (draw-pixel pixel sprite)))
       (incf sprite (first i)))                  ; then move sprite

      (t (error "Can't understand instruction. Aborting.")))))

(defun draw-pixel (pixel sprite)
  "draws the CRT screen, plotting a # for a lit pixel and a . for a dark pixel, calculates
the end of line, and resets pixel"
  (cond ((zerop (mod pixel 40)) ; end of line
	 (format t "~&~a"       ; emit newline, reset pixel to 0
		 (if (<= (1- sprite) 0 (1+ sprite))  "#" "."))  ; pixel 0 in a sprite?
	 1)                     ; next pixel is 1

	(t
	 (format t "~a" (if (<= (1- sprite) pixel (1+ sprite))  "#" ".")) ; emit a dot
	 (1+ pixel)))) ; next pixel

(defun day10-2 (f)
  (render-screen (parse-insts (uiop:read-file-lines f))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2022 Day 10 Part 1 is ~a"
	      (day10-1 *data-file*)))

(time (format t "~%The answer to AOC 2022 Day 10 Part 2 is ~a"
	      (day10-2  *data-file*)))


;; ----------------------------------------------------------------------------------------------------
;; Timings with SBCL on M2 MacBook Air with 24GB RAM
;; ----------------------------------------------------------------------------------------------------

;; The answer to AOC 2022 Day 10 Part 1 is 15680
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000093 seconds of total run time (0.000070 user, 0.000023 system)
;; 100.00% CPU
;; 0 bytes consed

;; ####.####.###..####.#..#..##..#..#.###..
;; ...#.#....#..#.#....#..#.#..#.#..#.#..#.
;; ..#..###..###..###..####.#....#..#.#..#.
;; .#...#....#..#.#....#..#.#.##.#..#.###..
;; #....#....#..#.#....#..#.#..#.#..#.#....
;; ####.#....###..#....#..#..###..##..#....
;; The answer to AOC 2022 Day 10 Part 2 is DONE
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000099 seconds of total run time (0.000082 user, 0.000017 system)
;; 100.00% CPU
;; 65,536 bytes consed

;; --------Part 1--------       --------Part 2--------
;; Day     Time   Rank  Score       Time   Rank  Score
;; 10      >24h  65590      0       >24h  64371      0
;; 9       >24h  62565      0       >24h  61541      0
;; 8       >24h  75284      0       >24h  69823      0
;; 7       >24h  79100      0       >24h  77516      0
;; 6   01:02:38  19233      0   01:07:16  18804      0
;; 5   03:01:38  23370      0   03:55:49  26420      0
;; 4   01:01:11  15964      0   01:16:38  16172      0
;; 3   00:42:32  12585      0   01:17:33  13957      0
;; 2   01:25:57  19891      0   01:57:08  20821      0
;; 1   00:36:07  10562      0   00:46:09  10629      0
