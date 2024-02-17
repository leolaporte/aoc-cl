;;;; Day12.lisp
;;;; 2023 AOC Day 12 solution
;;;; Leo Laporte
;;;; 25 January - 16 February 2023

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia :alexandria))

(defpackage :day12
  (:use #:cl #:iterate)  ; use iter instead of LOOP
  (:local-nicknames
   (:re :cl-ppcre)                      ; regular expressions
   (:tr :trivia)                        ; pattern matching
   (:al :alexandria)                    ; common utilities
   (:5a :fiveam)))                      ; testing

(in-package :day12)
(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_12/input.txt"
  "Downloaded from the AoC problem set")

#| ----------------------------------------------------------------------------
--- Day 12: Hot Springs ---
--- Part One ---

"For each row, the condition records show every spring and whether it
is operational (.) or damaged (#). This is the part of the condition
records that is itself damaged; for some springs, it is simply
unknown (?) whether the spring is operational or damaged.

However, the engineer that produced the condition records also
duplicated some of this information in a different format! After the
list of springs for a given row, the size of each contiguous group of
damaged springs is listed in the order those groups appear in the
row. This list always accounts for every damaged spring, and number
each is the entire size of its contiguous group (that is, groups are
always separated by at least one operational spring: #### would always
be 4, never 2,2).

For each row, count all of the different arrangements of operational
and broken springs that meet the given criteria. What is the sum of
those counts?"

LEO'S NOTES: The structure of the program is easy, it's writing the
code to count the possible arrangements that's tough.

Let's start with the easy part: parsing the input. Each line needs to
be separated into two segments I'll call springs and groupings. I'll put
a meaningless . char at the end of each condition list to make the
check for the final group easier. (Otherwise I get an error comparing
# to nil.)

Thanks to Peter Norvig (whose solution I adapted) and Xavdid (who
helped me understand what is actually happening) I have a recursive
solution to part one.

To speed things up for part two - which grows exponentially - I'll use
memoization. There's a library for that but I wanted to write my
own. My cache is very simple. I just keep a hash table with the keys
being the function's arguments and the values the associated
results. If I've already solved for that argument, just return the
hash value, otherwise solve it and save the result to the hash.

I'm told this is "top down dynamic programming." Well la-di-da.
----------------------------------------------------------------------------|#

(defparameter *test-data*
  '("???.### 1,1,3"
    ".??..??...?##. 1,1,3"
    "?#?#?#?#?#?#?#? 1,3,1,6"
    "????.#...#... 4,1,1"
    "????.######..#####. 1,6,5"
    "?###???????? 3,2,1"))

(defun parse-input (los)
  "given a list of strings describing a set of springs and their
groupings, return a tuple containing a spring string and a list of
integers representing valid groupings of damaged springs"
  (let ((spring-list '()))

    (dolist (l los)                     ; go line by line
      (let ((splits (re:split " " l)))  ; split line in two: springs and groupings

        (push (cons
               (concatenate 'string (first splits) ".")  ; add . for convenience
               (mapcar #'parse-integer                   ; and a list of ints
                       (re:all-matches-as-strings "\\d+"
                                                  (second splits))))
              spring-list)))            ; push each tuple into a list

    (reverse spring-list)))             ; and return it (after reversing)

(5a:test parse-input-t
  (5a:is (equal (first (parse-input *test-data*))
                (cons "???.###." '(1 1 3)))))

;; use memoization to speed up this process (especially in part 2) by
;; keeping a hash of all the solved spring sub-sequences and their
;; results - each call to memoize checks to see if I've seen this
;; argument before and only performs the calculation if it's new
(defun memoize (fn)
  (let ((cache (make-hash-table :test 'equal))) ; add each sub-result into hash
    #'(lambda (&rest args)
        (or (gethash args cache) ; have I already solved this? yes!
            (setf (gethash args cache) (apply fn args)))))) ; no solve and cache

(defun max-damage (springs)
  "returns the largest number of possible damaged springs in a list of
 springs"
  (+ (count #\# springs) (count #\? springs)))

(5a:test max-damage-test
  (5a:is (= 6 (max-damage "??##...##"))))

;; Use recursion to split the parsing into small chunks, memoizing the
;; results
(defparameter *cc* ; cached count
  (memoize ; cache each substring result
   (lambda (tuple)
     (let ((springs (car tuple))     ; the list of spring chars
           (groups (cdr tuple)))     ; the groupings as a list of int

       (cond ((null groups)          ; we've done all the groups
              (if (find #\# springs) ; if leftover #s
                  0                  ; then fail
                  1))                ; no leftovers - a win!

             ;; insufficient springs remain to do the job
             ((< (max-damage springs) (apply #'+ groups)) 0)

             (t ; there are still groups and springs left to check
              (+
               ;; does the first chunk of springs satisfy the first group?
               (if (or
                    ;; the first group of springs cannot be #
                    (not (= (max-damage (subseq springs 0 (first groups)))
                            (first groups)))
                    ;; the group is followed by #\# (breaking it)
                    (char= #\# (char (subseq springs (first groups)) 0)))
                   0   ; then it's a fail

                   ;; else we've got the first group so chop
                   ;; off that group (and following spring
                   ;; which must be .) and continue
                   (funcall *cc* (cons (subseq springs (1+ (first groups)))
                                       (rest groups))))

               ;;  we don't have a satisfactory group, so what do we have?
               (if (char= #\# (char springs 0)) ; do we have a bad group?
                   0                            ; in which case, fail
                   ;; not damaged, so it must be operational, drop and recurse
                   (funcall *cc* (cons (subseq springs 1) groups))))))))))

(5a:test funcall-*cc*-test
  (let ((d (parse-input *test-data*)))
    (5a:is (= (funcall *cc* (first d)) 1))
    (5a:is (= (funcall *cc* (second d)) 4))
    (5a:is (= (funcall *cc* (third d)) 1))
    (5a:is (= (funcall *cc* (fourth d)) 1))
    (5a:is (= (funcall *cc* (fifth d)) 4))
    (5a:is (= (funcall *cc* (sixth d)) 10))))

(Defun Day12 (los)
  "given a list of strings reflecting each sprint condition return the
sum of possible arrangements of operational and broken springs"
  (iter (for l in los)
    (summing (funcall *cc* l))))

(5a:test Day12-test
  (5a:is (= (Day12 (parse-input *test-data*)) 21)))

#| ----------------------------------------------------------------------------
--- Part Two ---

Let's get exponential!!

"unfold the records, on each row, replace the list of spring springs
with five copies of itself (separated by ?) and replace the list of
contiguous groups of damaged springs with five copies of
itself (separated by ,)."

I was prepared for this. I'll unfold the original parsed lines and
then let's see if my simple memoization is up to the task.

Harumph - at least it completes but the time (78 seconds) is
disappointing.  Maybe I should use a memoize library instead of my
roll-your-own hash table. ... So I've tested three different
memoization libraries for common lisp and none of them does better -
some much worse.

There's some optimization that's eluding me because I know people are
doing part 2 much faster even with the much slower Python.

OK I figured it out. I was representing the spring string as a list of
chars. Strings are vectors and much faster - so I'm leaving it as a
string (which is a very minor modification as it turns out). And I
suspect vectors work better as keys for the memoize hash.
----------------------------------------------------------------------------|#

(defun unfold-input (los)
  "given a list of lists containing spring springs and groupings return
 an unfolded list with each condition string repeated five times with a
 ? in between each, and return a list of groupings repeating the
 original group five times."
  (let ((unfolded '()))

    (dolist (l los)
      (let* ((splits (re:split " " l))
             (springs (first splits))
             (rules (mapcar #'parse-integer
                            (re:all-matches-as-strings "\\d+"
                                                       (second splits)))))
        (push
         (cons
          (let ((folded (loop repeat 5 collect (concatenate 'string springs "?") into sequences
                              finally (return (apply #'concatenate 'string sequences)))))

            ;; replace the last ? with .
            (concatenate 'string (subseq folded 0 (1- (length folded))) "."))

          (iter (repeat 5) (appending rules)))
         unfolded)))

    (reverse unfolded)))

(5a:test funcall-*cc*-test-2
  (let ((d (unfold-input *test-data*)))
    (5a:is (= (funcall *cc* (first d)) 1))
    (5a:is (= (funcall *cc* (second d)) 16384))
    (5a:is (= (funcall *cc* (third d)) 1))
    (5a:is (= (funcall *cc* (fourth d)) 16))
    (5a:is (= (funcall *cc* (fifth d)) 2500))
    (5a:is (= (funcall *cc* (sixth d)) 506250))))

(5a:test Day12-2-test
  (5a:is (= (Day12 (unfold-input *test-data*)) 525152)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 12 Part 1 is ~a"
              (day12 (parse-input (uiop:read-file-lines *data-file*)))))

(time (format t "The answer to AOC 2023 Day 12 Part 2 is ~a"
              (day12 (unfold-input (uiop:read-file-lines *data-file*)))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 12 Part 1 is 6488
;; Evaluation took:
;; 0.007 seconds of real time
;; 0.006673 seconds of total run time (0.006230 user, 0.000443 system)
;; 100.00% CPU
;; 4,228,144 bytes consed

;; The answer to AOC 2023 Day 12 Part 2 is 815364548481
;; Evaluation took:
;; 0.165 seconds of real time
;; 0.166004 seconds of total run time (0.162056 user, 0.003948 system)
;; [ Real times consist of 0.010 seconds GC time, and 0.155 seconds non-GC time. ]
;; [ Run times consist of 0.010 seconds GC time, and 0.157 seconds non-GC time. ]
;; 100.61% CPU
;; 104,884,816 bytes consed
