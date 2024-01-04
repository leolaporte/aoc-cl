;;;; Day07.lisp
;;;; 2023 AOC Day 07 solution
;;;; Leo Laporte
;;;; 3-4 Jan 2024

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :split-sequence :trivia))

(defpackage :day07
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)
   (:5a :fiveam)
   (:ss :split-sequence)
   (:tr :trivia)))

(in-package :day07)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_07/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 7: Camel Cards ---
--- Part One ---

"In Camel Cards, you get a list of hands, and your goal is to order
them based on the strength of each hand. A hand consists of five cards
labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative
strength of each card follows this order, where A is the highest and 2
is the lowest.

Every hand is exactly one type. From strongest to weakest, they are:

Five of a kind
Four of a kind
Full house
Three of a kind
Two pair
One pair
High card

If two hands have the same type, a second ordering rule takes
effect. Start by comparing the first card in each hand. If these cards
are different, the hand with the stronger first card is considered
stronger.

Now, you can determine the total winnings of this set of hands by
adding up the result of multiplying each hand's bid with its rank.

Find the rank of every hand in your set. What are the total winnings?"

LEO'S NOTES: There are 1000 hands in the input data, so I probably
need to store the data efficiently. I'll also have to rank the hands.

So two data structures a hash-table of hands=>bids for fast lookups
and a vector of hands for sorting hand rank.

1. Read the input data into an unordered hash-table for quick look up
of bid value. Key: Hand Value: Bid

2. Use the hash-table to  create a vector of hands.

3. Write a sort key function that can order two hands according to the
rules.

4. Sort the vector using that key.

5. Then traverse the vector from lowest hand to highest, looking each
hand up in the hash table to find the bid value and multiply it by the
index+1 of that hand.

All the real work is in the SORT predicate.

----------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"))

#| ---------------------------- Parsing code ---------------------------------|#

(defun pht (hash)
  "little utility for printing the contents of a hash"
  (loop for k being the hash-keys in hash using (hash-value v)
        do (format t "~A => ~A~&" k v)))

(defun make-hands-hash (los)
  "given a list of strings containing the hand and a number string
separated by a space, return a hash table of all lines with the hand
string being the key and the number string as an integer for the bid
value."
  (let ((hands (make-hash-table :test 'equalp :size (length los))))
    (dolist (hand los)
      (let ((hb (ss:split-sequence #\Space hand)))
        (setf (gethash (first hb) hands) (parse-integer (second hb)))))
    hands))

(defun make-hands-vec (hands-hash)
  "given a hands hash create a vector of hands"
  (let ((hands-vector (make-array (hash-table-count hands-hash)
                                  :adjustable t :fill-pointer 0)))
    (loop for h being the hash-keys in hands-hash
          do (vector-push h hands-vector))
    hands-vector))

#| ------------------------------- Working code ----------------------------- |#

(defun score-hand (hand)
  "given a string of five characters (a hand) return the hand score: 7
for five of a kind, 6 for four of a kind, 5 for full house ... 0 for
highcard, etc."
  (let* ((total (apply #'+ (loop for c in (coerce hand 'list)
                                 collect (count c hand)))))

    (cond ((= total 25) 7) ; five of a kind
          ((= total 17) 6) ; four of a kind
          ((= total 13) 5) ; full house
          ((= total 11) 4) ; three of a kind
          ((= total 9) 3)  ; two pair
          ((= total 7) 2)  ; pair
          (t 0))))         ; high card

(5a:test score-hand-test
  (5a:is (equal (score-hand "KKKKK") 7))
  (5a:is (equal (score-hand "KKTKK") 6))
  (5a:is (equal (score-hand "K2K2K") 5))
  (5a:is (equal (score-hand "KKK12") 4))
  (5a:is (equal (score-hand "11233") 3))
  (5a:is (equal (score-hand "KK123") 2))
  (5a:is (equal (score-hand "12345") 0)))

(defun lower-hand-p (x y)
  "returns true if x is a lower hand than y by card rank and order -
applied only if both hands have an equal score"
  (labels ((encode (hand)
             (setf hand (substitute #\E #\A hand))
             (setf hand (substitute #\D #\K hand))
             (setf hand (substitute #\C #\Q hand))
             (setf hand (substitute #\B #\J hand))
             (setf hand (substitute #\A #\T hand))
             hand))
    (string< (encode x) (encode y))))

(5a:test lower-hand-p-test
  (5a:is-true (lower-hand-p "K2344" "A2345"))
  (5a:is-true (lower-hand-p "Q432A" "KA234"))
  (5a:is-true (lower-hand-p "KQJT4" "KQJT5"))
  (5a:is-false (lower-hand-p "23456" "23455"))
  (5a:is-false (lower-hand-p "QKJ12" "JKJ12"))
  (5a:is-false (lower-hand-p "12J34" "12T45")))

(defun hand< (x y)
  "given two hands, x y, return true if x is lower than y. This is where
the magic happens"
  (let ((x-score (score-hand x))
        (y-score (score-hand y)))

    (if (or (zerop (+ x-score y-score)) ; both high-card hands
            (= x-score y-score))        ; or equal hands
        (lower-hand-p x y)              ; so rank 'em
        (< x-score y-score))))          ; otherwise compare 'em

(5a:test hand<-test
  (5a:is-true (hand< "KK123" "KKK12"))
  (5a:is-true (hand< "12345" "1234T"))
  (5a:is-true (hand< "QQQ99" "KKK99"))
  (5a:is-true (hand< "QKQKT" "QKQKJ")))

(defun Day07-1 (los)
  (let* ((hands-hash (make-hands-hash los))       ; turn hands into hash
         (hands-vec (make-hands-vec hands-hash))) ; and vector

    (sort hands-vec #'hand<) ; sort the hands from lowest to highest

    (loop for i below (length hands-vec) ; for hands from lowest to highest
          summing                  ; adding each score (score = bid * rank)
          (* (gethash (aref hands-vec i) hands-hash) ; the bid
             (1+ i)))))                              ; the rank

(5a:test Day07-1-test
  (5a:is (= (Day07-1 *test-data*) 6440)))

#| -----------------------------------------------------------------------------
--- Part Two ---

New Joker rule:
Now, J cards are wild! (but they still rank lower than 2)

LEO'S NOTE: I'll have to change how the vector is sorted. So hand2<
which calls lower-hand2-p and score-hand2. Otherwise the main routine
stays the same.

------------------------------------------------------------------------------|#

(defun score-hand2 (hand)
  "given a string of five characters (a hand) return the hand
score (Jokers are wild so use them to maximize the score): 7 for five
of a kind, 6 for four of a kind, 5 for full house ... 0 for highcard,
etc.. It feels like there's something I could do to make it simpler
but it runs fairly quickly so..."
  (let* ((jokers (count #\J hand))      ; count the jokers
         (base-hand (remove #\J hand))  ; then remove them
         (score (apply #'+ (loop for c in (coerce base-hand 'list) ; score
                                 collect (count c base-hand)))))   ; what's left

    (tr:match jokers ; match the number of jokers

      ((or 5 4) 7)               ; five of a kind "JJJJJ" or "JJJJ1"

      (3 (if (= score 4) 7 6))   ; "JJJ22" or "JJJ11"

      (2 (cond ((= score 9) 7)   ; "JJ333"
               ((= score 5) 6)   ; "JJ221"
               ((= score 3) 4))) ; "JJ111

      (1 (cond ((= score 16) 7)  ; "J4444
               ((= score 10) 6)  ; "J3331"
               ((= score 8) 5)   ; "J2222"
               ((= score 6) 4)   ; "J2211"
               ((= score 4) 2)   ; "J1111"
               (t (error "What??"))))

      (otherwise
       (cond ((= score 25) 7) ; five of a kind
             ((= score 17) 6) ; four of a kind
             ((= score 13) 5) ; full house
             ((= score 11) 4) ; three of a kind
             ((= score 9) 3)  ; two pair
             ((= score 7) 2)  ; pair
             (t 0))))))       ; high card

(5a:test score-hand2-test
  (5a:is (equal (score-hand2 "KKJKK") 7))
  (5a:is (equal (score-hand2 "KKTKJ") 6))
  (5a:is (equal (score-hand2 "K2K2J") 5))
  (5a:is (equal (score-hand2 "KKJ12") 4))
  (5a:is (equal (score-hand2 "1J233") 4))
  (5a:is (equal (score-hand2 "KJ123") 2))
  (5a:is (equal (score-hand2 "12345") 0)))

(defun lower-hand2-p (x y)
  "returns true if x is a lower hand than y by card rank and order -
applies only if the hands have equal score"
  (labels ((encode (hand)
             (setf hand (substitute #\E #\A hand))
             (setf hand (substitute #\D #\K hand))
             (setf hand (substitute #\C #\Q hand))
             (setf hand (substitute #\1 #\J hand)) ; joker is lowest worth
             (setf hand (substitute #\A #\T hand))
             hand))
    (string< (encode x) (encode y))))

(5a:test lower-hand2-p-test
  (5a:is-true (lower-hand2-p "KQJT4" "KQ2T5"))
  (5a:is-true (lower-hand2-p "JKJ12" "2KJ12"))
  (5a:is-true (lower-hand2-p "12J34" "12T45")))

(defun hand2< (x y)
  "given two hands, x y, return true if x is lower than y. This is where
the magic happens"
  (let ((x-score (score-hand2 x))
        (y-score (score-hand2 y)))

    (if (or (zerop (+ x-score y-score)) ; both high-card hands
            (= x-score y-score))        ; or equal hands
        (lower-hand2-p x y)             ; so rank 'em
        (< x-score y-score))))          ; otherwise compare 'em

(5a:test hand2<-test
  (5a:is-true (hand2< "KK123" "KKK12"))
  (5a:is-true (hand2< "12345" "1234T"))
  (5a:is-true (hand2< "QQQ99" "KKK99"))
  (5a:is-true (hand2< "QKQKT" "QKQKJ")))

(defun Day07-2 (los)
  (let* ((hands-hash (make-hands-hash los))       ; turn hands into hash
         (hands-vec (make-hands-vec hands-hash))) ; and vector

    (sort hands-vec #'hand2<) ; sort the hands using the new rules

    (loop for i below (length hands-vec) ; for hands from lowest to highest
          summing                  ; adding each score (score = bid * rank)
          (* (gethash (aref hands-vec i) hands-hash) ; the bid
             (1+ i)))))                              ; the rank

(5a:test Day07-2-test
  (5a:is (= (Day07-2 *test-data*) 5905)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 07 Part 1 is ~a"
              (day07-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 07 Part 2 is ~a"
              (day07-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 07 Part 1 is 247823654
;; Evaluation took:
;; 0.015 seconds of real time
;; 0.015376 seconds of total run time (0.014441 user, 0.000935 system)
;; 100.00% CPU
;; 9,696,992 bytes consed

;; The answer to AOC 2023 Day 07 Part 2 is 245461700
;; Evaluation took:
;; 0.017 seconds of real time
;; 0.017782 seconds of total run time (0.016893 user, 0.000889 system)
;; 105.88% CPU
;; 10,679,552 bytes consed
