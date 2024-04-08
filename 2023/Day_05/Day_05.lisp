;;;; Day05.lisp
;;;; 2023 AOC Day 05 solution
;;;; Leo Laporte
;;;; 16 December 2023 - 3 January 2024
;;;; (optimizing Part 2 was a b***h)

;; -----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; -----------------------------------------------------------------------------

(ql:quickload '(:fiveam :cl-ppcre :trivia))

(defpackage :day05
  (:use #:cl)
  (:local-nicknames
   (:re :cl-ppcre)     ; regular expression library
   (:5a :fiveam)       ; unit testing
   (:tr :trivia)))     ; pattern matching

(in-package :day05)

(setf 5a:*run-test-when-defined* t)                    ; test as we go
(declaim (optimize (debug 3)))                         ; max debugging info
;; (declaim (optimize (speed 3) (debug 0) (safety 0))) ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_05/input.txt"
  "Downloaded from the AoC problem set")

#| -----------------------------------------------------------------------------
--- Day 5: If You Give A Seed A Fertilizer ---
--- Part One ---

"The almanac (your puzzle input) lists all of the seeds that need to
be planted. It also lists what type of soil to use with each kind of
seed, what type of fertilizer to use with each kind of soil, what type
of water to use with each kind of fertilizer, and so on. Every type of
seed, soil, fertilizer and so on is identified with a number, but
numbers are reused by each category - that is, soil 123 and fertilizer
123 aren't necessarily related to each other.

What is the lowest location number that corresponds to any of the
initial seed numbers?"

LEO'S NOTE: Parse the almanac into eight lists, one of seeds, and seven maps
consisting of triplets in the form '(dest start, source start, range):
soil, fert, water, light, temp, humid, and loc (this is where most of
the work will be).

Create a function that takes two parameters: map and seed
(transform map seed)

Transform searches for seed in the map, if seed is within the range of
a map, transform it, otherwise return seed.

Do this for each seed and return the lowest loc.
----------------------------------------------------------------------------- |#

(defparameter *test-data*
  '("seeds: 79 14 55 13"
    ""
    "Seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23" ; 77-99
    "81 45 19" ; 45-63
    "68 64 13" ; 64-76
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"))

(defun split-list-if (test list &aux (start list) (end list))
  "splits a list where test is true"
  (loop while (and end (setq start (member-if-not test end)))
        collect (ldiff start (setq end (member-if test start)))))

(defun make-seeds (seed-map)
  "given a seed map as a string, return a list of integers"
  (mapcar #'parse-integer (rest (uiop:split-string seed-map))))

(defparameter *digits* (re:create-scanner "\\d+"))

(defun make-map (list-of-strings)
  "given a specific map (soil, fertilizer, etc.) as a list of strings,
return a list of triplets representing (dest start, source start,
range) as a list of integer"
  (loop for triplet in (rest list-of-strings) ; drop seeds
        collect
        (mapcar #'parse-integer  ;  convert to integers
                (re:all-matches-as-strings *digits* triplet)))) ; get the 3 nums

(5a:test make-map-test
  (5a:is (equal (make-map
                 '("light-to-temperature map:" "45 77 23" "81 45 19" "68 64 13"))
                '((45 77 23) (81 45 19) (68 64 13)))))

(defun parse-maps (list-of-strings)
  "given a list of strings that describe a list of seeds and seed maps return a
list of maps"
  (let ((maps (rest (split-list-if
                     #'(lambda (str) (string= str "")) list-of-strings))))
    (loop for map in maps collecting (make-map map))))

(defun transform (map seed)
  "given a list of tripets representing a resource map (dest start range)
and a seed, transform the seed by finding the right triplet and adding
the difference between the start and seed to dest - if no such triplet
exists, return the original seed value"
  (let ((triplet
          ;; find a triplet for which the seed is in range
          (find-if
           #'(lambda (source)
               (<= (second source) seed (+ (second source) (third source))))
           map)))
    (if triplet
        (+ (first triplet) (- seed (second triplet))) ; return transformed seed
        seed)))                                   ; otherwise, just return seed

(5a:test transform-test
  (let ((soil '((50 98 2) (52 50 48))))
    (5a:is (equal (transform soil 79) 81))
    (5a:is (equal (transform soil 14) 14))
    (5a:is (equal (transform soil 55) 57))
    (5a:is (equal (transform soil 13) 13))))

(defun transform-all (list-of-maps seed)
  "given a seed and a list of maps to traverse through in order return
the final seed"
  (dolist (map list-of-maps)
    (setf seed (transform map seed)))
  seed)

(defun Day05-1 (los)
  "given a list of strings representing a list of seeds and transformation maps
return the closest seed location"
  (let ((maps (parse-maps los))
        (seeds (make-seeds (first los))))
    (loop for s in seeds
          minimize (transform-all maps s))))

(5a:test Day05-1-test
  (5a:is (= (Day05-1 *test-data*) 35)))

#| -----------------------------------------------------------------------------
--- Part Two ---

"It looks like the seeds: line actually describes ranges of seed
numbers. The values on the initial seeds: line come in pairs. Within
each pair, the first value is the start of the range and the second
value is the length of the range."

LEO'S NOTES: Hmm. Not much of a change. I just have to modify how I
get the seed list. I'll factor that out of part one as MAKE-SEEDS and
write a new function for part 2, NEW-MAKE-SEEDS.

Oh. It is a LOT of seeds. Well the first part was very fast let's see
how it handles a MUCH longer list of seeds!

...

Emacs crashes just generating the seed list. I need a better way of
looking at seeds. Honestly I don't see how I can NOT expand the range
- I have to look at every seed for the answer. The bottleneck seems to
be in NEW-MAKE-SEEDS - let me see if I can speed that up. Would a
vector be faster than a list? Almost certainly.

...

OK but I exhausted the heap! So we'll have to do this on the fly. I'll
generate each seed range one at a time, collect the minimum seed from
each range, then get the minimum of that.

This just takes way way way too long. There must be an easier way.

So the big insight is that I DON'T have to check every seed. Clearly I
can't. I can check seed RANGES against map RANGES. The transform will
have to accept ranges of values in both maps and seeds, and each
transform will output a transformed range of seeds. Basically I am no
longer working with individual values, but always with ranges. That's
the optimization.

One more mistake I made. I assumed that once I matched any portion of
a seed to a map that I was done. In fact, I'm not. I have to keep
processing the unchanged portion of the range until there are no more
overlaps. Fixed that and it sails through in 0 seconds. (Well .000772
to be exact).
------------------------------------------------------------------------------|#

(defun overlap-p (source seed)
  "Given a map transformation triplet (dest start, source start, length)
and a list representing a seed range '(seed start, length), returns
NIL if seed overlaps none of the maps. Otherwise returns the type of
overlap: below above same src-contains seed-contains"
  (let* ((src-start (second source))
         (src-end (1- (+ src-start (third source))))
         (seed-start (first seed))
         (seed-end (1- (+ seed-start (second seed)))))
    (cond
      ;; start and end match
      ((and (= src-start seed-start)
            (= src-end seed-end)) 'same)
      ;; seed overlaps from below
      ((and (< seed-start src-start)
            (<= src-start seed-end src-end)) 'below)
      ;; seed overlaps from above
      ((and  (> seed-end src-end)
             (<= src-start seed-start src-end)) 'above)
      ;; source contains seed
      ((and (<= src-start seed-start src-end)
            (<= src-start seed-end src-end)) 'src-contains)
      ;; seed contains source
      ((and (< seed-start src-start)
            (> seed-end src-end)) 'seed-contains)

      (t nil)))) ; no overlap, return FALSE

(5a:test overlap-p-test
  ;; map range is 100-149                                         ; seed:
  (5a:is (equal (overlap-p '(0 100 50) '(99 2)) 'below))          ; 99-100
  (5a:is (equal (overlap-p '(0 100 50) '(110 100)) 'above))       ; 110-209
  (5a:is (equal (overlap-p '(0 100 50) '(101 5)) 'src-contains))  ; 101-105
  (5a:is (equal (overlap-p '(0 100 50) '(99 60)) 'seed-contains)) ; 99-158
  (5a:is (equal (Overlap-p '(0 100 50) '(100 50)) 'same))         ; 100-149
  (5a:is (equal (overlap-p '(0 100 50) '(150 1)) nil)))           ; 150

(defun transform-seed (map-range seed)
  "given a triplet '(dest source len) representing a source and
destination range from a map for soil, fert, etc., and a seed-range --
a list of two integers: start and length. Where seed and source
overlap the values are mapped to dest, otherwise they're returned
unchanged. Returns two values:

1. a contiguous range of transformed locations
2. 0, 1, or 2 ranges in the seed that were not modified

In other words two lists:  changed range and unchanged ranges"

  (let ((seed-start (first seed))
        (seed-len (second seed))
        (dst-start (first map-range))
        (src-start (second map-range))
        (src-len (third map-range)))

    (tr:match (overlap-p map-range seed) ; overlaps how?

      ('above
       ;; seed range overlaps from above - 2 ranges result
       ;; Src    xxxxxxxxxxxxx
       ;;        a   b       c         d
       ;; Seed       |-------/---------|
       ;; Ranges      changed/unchanged
       (let* ((a2b (- seed-start src-start)) ; segment lengths
              (b2c (- src-len a2b))
              (c2d (- seed-len b2c)))
         (values
          (list (+ dst-start a2b) b2c)            ; changed (b->c)
          (list (list (+ seed-start b2c) c2d))))) ; unchanged (c->d)

      ('below
       ;; seed range overlaps from below - 2 ranges result
       ;; Src             xxxxxxxxxxxxxxx
       ;;        a        b       c
       ;; Seed   |--------/-------/
       ;; Ranges unchanged/changed
       (let* ((a2b (- src-start seed-start)) ; segment lengths
              (b2c (- seed-len a2b)))
         (values
          (list dst-start b2c)            ; changed (b->c)
          (list (list seed-start a2b))))) ; unchanged (a->b)

      ('seed-contains
       ;; Seed extends on both sides of source - 3 ranges result
       ;; Src              xxxxxxxxx
       ;;       a          b        c         d
       ;; Seed  |----------/--------/---------|
       ;;        unchanged  changed  unchanged
       (let* ((a2b (- src-start seed-start)) ; seg lengths
              (b2c src-len)
              (c2d (- seed-len b2c a2b)))
         (values
          (list dst-start b2c)              ; changed
          (list
           (list seed-start a2b)             ; unchanged
           (list (+ src-start b2c) c2d)))))  ; unchanged

      ('src-contains
       ;; Source range contains seed-range - 1 range result
       ;; Src    xxxxxxxxxxxxxxxxx
       ;; Seed      |-------|
       ;; Range     /changed/
       (values
        (list (+ dst-start (- seed-start src-start)) seed-len)
        nil))

      ('same
       ;; Src and seed are the same - 1 range result
       ;; Src    xxxxxxxxx
       ;; Seed.  |-------|
       ;; Ranges /changed/
       (values
        (list dst-start seed-len)
        nil))

      ;; no overlap - return just the seed
      (otherwise (values seed nil)))))

(defun map-seed (map seed)
  "given a list of map triplets (a complete map) and a single seed,
return a list of fully processed seed->dest ranges"
  (do ((changed '())             ; stack of transformed ranges
       (unchanged (list seed)))  ; stack of yet to be transformed ranges

      ((null unchanged) changed) ; no more to do? return transformed ranges

    ;; loop through unprocessed ranges
    (let* ((next (pop unchanged))
           (match (find-if #'(lambda (m) (overlap-p m next)) map)))
      (if (null match)        ; any valid map range?
          (push next changed) ; no? then push the unchanged range to changed
          ;; yes? then map this range
          (multiple-value-bind (c u) (transform-seed match next)
            (push c changed)                  ; push changed range
            (loop for s in u                  ; unroll and
                  do (push s unchanged))))))) ; push unchanged

(defun map-seeds (map seeds)
  "given a single map and a list of seeds, return a list of all the
transformed ranges (in other words: run all seeds through a single map
transforming every range that matches any source range in the map by
mapping it to dest until no more changes are possible)"
  (cond ((null seeds) nil)
        (t (append (map-seed map (first seeds))
                   (map-seeds map (rest seeds))))))

(5a:test map-seeds-test
  (5a:is (equal (map-seeds (list '(0 100 10) '(100 200 10))
                           (list '(300 10) '(100 1) '(199 2)))
                (list '(300 10) '(0 1) '(199 1) '(100 1)))))

(defun seed->location (maps seeds)
  "given a list of maps to traverse and a list containing a starting
seed, walk the seed through the maps until we have collected a list of
all completely transformed ranges then return the lowest location
value"
  (dolist (m maps)
    (setf seeds (map-seeds m seeds))) ; run the seed through the maps
  (loop for (loc len) in seeds minimize loc)) ; return the lowest location

(5a:test seed->location-test
  (5a:is (equal (seed->location (parse-maps *test-data*) '((82 11))) 46)))

(defun Day05-2 (los)
  "given a list of strings representing a list of seeds and
transformation maps return the closest seed location"
  (let ((maps (parse-maps los))
        (seeds (make-seeds (first los))))

    (loop for (start length) on seeds by #'cddr ; go seed pair by pair
          minimize (seed->location maps (list (list start length))))))

(5a:test Day05-2-test
  (5a:is (equal (Day05-2 *test-data*) 46)))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 05 Part 1 is ~a"
              (day05-1 (uiop:read-file-lines *data-file*))))

(time (format t "The answer to AOC 2023 Day 05 Part 2 is ~a"
              (day05-2 (uiop:read-file-lines *data-file*))))

;; -----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; -----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 05 Part 1 is 457535844
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000400 seconds of total run time (0.000346 user, 0.000054 system)
;; 100.00% CPU
;; 196,496 bytes consed

;; The answer to AOC 2023 Day 05 Part 2 is 41222968
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000772 seconds of total run time (0.000744 user, 0.000028 system)
;; 100.00% CPU
;; 196,480 bytes consed
