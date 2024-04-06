;;;; Day20.lisp
;;;; 2023 AOC Day 20 solution
;;;; Leo Laporte
;;;; Started: 31 March 2024
;;;; Done: 6 April 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:serapeum :alexandria :fiveam :iterate
                :cl-ppcre :str :trivia :trivia.ppcre)) ; useful libraries
(use-package :iterate) ; use iter instead of LOOP

(defpackage :day20
  (:use  #:cl :iterate)
  (:local-nicknames              ; not all of these are used every day
   (:sr :serapeum)               ; misc utilities
   (:ax :alexandria)             ; ditto
   (:re :cl-ppcre)               ; regex
   (:tr :trivia)                 ; pattern matching
   (:tp :trivia.ppcre)           ; regex in pattern matching
   (:5a :fiveam)))               ; testing framework

(in-package :day20)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/AOC/2023/Day_20/input.txt"
  "Downloaded from the AoC problem set")

(defparameter *data-file2* "~/cl/AOC/2023/Day_20/input2.txt"
  "ChocolateMilkMinisip's AoC problem set")


#| ----------------------------------------------------------------------------
                  --- Day 20: Pulse Propagation ---
                           --- Part One ---

"Here at Desert Machine Headquarters, there is a module with a single
button on it called, aptly, the button module. When you push the
button, a single low pulse is sent directly to the broadcaster module.

After pushing the button, you must wait until all pulses have been
delivered and fully handled before pushing it again. Never push the
button if modules are still processing pulses.

Pulses are always processed in the order they are sent. So, if a pulse
is sent to modules a, b, and c, and then module a processes its pulse
and sends more pulses, the pulses sent to modules b and c would have
to be handled first.

To get the cables warmed up, the Elves have pushed the button 1000
times. What do you get if you multiply the total number of low pulses
sent by the total number of high pulses sent?"

LEO'S NOTES: This seems like a good opportunity to play with object
oriented programing in Common Lisp, aka CLOS.

I'll define a superclass MODULE with the sub-classes FLIP-FLOP,
CONVEYOR, and BROADCASTER. And a generic function SEND-PULSE which
will have different methods for each class. (Oh I'm also adding a
sub-class DEVNULL for receiver modules that don't do anything.)

Each pulse send will be handled according to the rules for each kind
of module which will output a list of the next pulses to send
as (list (list to from type)). I'll collect all the NEXTs before I
process them to preserve the order of operation.

I.E. On each button push, I'll accumulate a list of next pulses in a
list called NEXT until all the sends are processed, then I'll make a
copy of the NEXT list into WORK, clear the NEXT list, and process
WORK, populating the new NEXT list. When, after processing the entire
WORK list there are no more NEXTs, the round is done and the next
button push will be processed. This will happen PUSHES times.

I'll use a :before method to keep track of the high and low pulses I'm
sending. (And an :after method to print each pulse sent for
debugging.) Pulse counts will be stored in shared slots that are part
of the superclass MODULE. When I've completed 1000 pushes I'll return
the product of all the 'HIGH and 'LOW pulses sent.

---------------------------------------------------------------------------- |#

(defparameter *example1*
  '("broadcaster -> a, b, c"
    "%a -> b"
    "%b -> c"
    "%c -> inv"
    "&inv -> a"))

(defparameter *example2*
  '("broadcaster -> a"
    "%a -> inv, con"
    "&inv -> b"
    "%b -> con"
    "&con -> output"))

(defparameter *debug* t) ; display each pulse send when true

;; ----------------------------------------------------------------------
;; CLASSES
;; ----------------------------------------------------------------------

(defclass module ()
  ((name
    :initarg :name
    :type string
    :reader name)
   (dests
    :initarg :dests
    :type list
    :initform '()
    :accessor dests
    :documentation "output modules connected to this module")
   (low-pulse-count
    :initform 0
    :type fixnum
    :accessor low-count
    :allocation :class
    :documentation "a shared slot that tallies all low pulses sent")
   (high-pulse-count
    :initform 0
    :type fixnum
    :accessor high-count
    :allocation :class
    :documentation "a shared slot that tallies all high pulses sent"))
  (:documentation "The superclass for all the modules in the circuit"))

(defclass flip-flop (module)
  ((state
    :initform 'OFF  ; starts 'OFF
    :type symbol
    :accessor state)) ; reads and using setf writes STATE
  (:documentation "Flip-flop modules are either on or off; they
 are initially off. If a flip-flop module receives a high pulse, it is
 ignored and nothing happens. However, if a flip-flop module receives
 a low pulse, it flips between on and off. If it was off, it turns on
 and sends a high pulse. If it was on, it turns off and sends a low
 pulse."))

(defun make-flip-flop (name dests)
  "flip-flip module constructor"
  (make-instance 'flip-flop :name name :dests dests))

(defclass conjunction (module)
  ((history
    :initform '() ; a list of pulse types received
    :type alist
    :accessor history))
  (:documentation "Conjunction modules remember the type of the
most recent pulse received from each of their connected input
modules; they initially default to remembering a low pulse for each
input. When a pulse is received, the conjunction module first updates
its memory for that input. Then, if it remembers high pulses for all
inputs, it sends a low pulse; otherwise, it sends a high pulse."))

(defun make-conjunction (name dests)
  "conjunction module constructor"
  (make-instance 'conjunction :name name :dests dests))

(defclass broadcast (module)
  () ; no slots
  (:documentation "There is a single broadcast module (named
 broadcaster). When it receives a pulse, it sends the same pulse to
 all of its destination modules."))

(defun make-broadcast (name dests)
  "broadcast module constructor"
  (make-instance 'broadcast :name name :dests dests))

(defclass devnull (module)
  ()
  (:documentation "A bit bucket that receives pulses but does nothing with them"))

(defun make-devnull (name dests)
  (make-instance 'devnull :name name :dests dests))

;; ----------------------------------------------------------------------
;; METHODS
;; ----------------------------------------------------------------------

(defgeneric send-pulse (source type mod)
  (:documentation "sends a 'HIGH or 'LOW TYPE pulse to a module named
 MOD from module SOURCE - the module will process the pulse according
 to its rules and output a list of next pulses to send as (list (list
 source pulse-type destination))"))

(defmethod send-pulse :before (source type (mod module))
  "before we process the pulse, make a record of it"
  (if (equal type 'LOW)
      (incf (low-count mod))   ; count incoming low
      (incf (high-count mod)))) ; count incoming high

(defmethod send-pulse :after (source type (mod module))
  "if *debug* is true sends a line showing what happened after each send-pulse"
  (when *debug*
    (format t "~%~A -~A-> ~A" source (string-downcase type) (name mod))))

(defmethod send-pulse (source type (mod flip-flop))
  "a pulse sent to a flip-flop module outputs a list of next pulses to
send"
  (declare (ignore source))

  (when (equal type 'LOW)             ; ignore 'HIGH pulses
    (cond ((equal (state mod) 'OFF)
           (setf (state mod) 'ON)
           (iter (for m in (dests mod))
             (collect (list (name mod) 'HIGH m))))

          ((equal (state mod) 'ON)
           (setf (state mod) 'OFF)
           (iter (for m in (dests mod))
             (collect (list (name mod) 'LOW m)))))))

(defmethod send-pulse (source type (mod broadcast))
  "trigger the broadcaster module to create a list of TYPE pulses to send
to all its dests as (list (cons mod type))"
  (declare (ignore source))
  ;; queue up pulses
  (iter (for m in (dests mod))
    (collect (list (name mod) type m))))

(defmethod send-pulse (source type (mod conjunction))
  "send a pulse to a conjunction module, which in-turn outputs a list of
all the modules to send pulses to as (list (cons mod type))"
  ;; track incoming pulse source and type
  (setf (history mod) ; remove previously received pulse from mod
        (remove-if #'(lambda (h) (equalp (car h) source)) (history mod)))
  (setf (history mod) ; replace it with the latest received pulse type
        (append (history mod) (list (cons source type)))) ; add new type

  ;; decide which pulse type to send
  (let ((pulse-type (if (every #'(lambda (x) (eql (cdr x) 'HIGH))
                               (history mod)) ; all 'HIGH?
                        ;; then
                        'LOW
                        ;; else
                        'HIGH)))

    ;; make a list of packets to send out
    (iter (for m in (dests mod))
      (collect (list (name mod) pulse-type m)))))

(defmethod send-pulse (source type (mod devnull))
  "the bit bucket - drops the pulse"
  (declare (ignore source)) (declare (ignore type))
  (declare (ignore mod))
  ())

(defmethod print-object ((mod module) stream)
  "overrides the standard print-object to print more info in a human
  readable form"
  (format stream "~%Type: ~S~&State: ~S~&Dests: ~S~&"
          (type-of mod)
          (tr:match (type-of mod)
            ('FLIP-FLOP (state mod))
            ('CONJUNCTION (history mod))
            ('BROADCAST 'NA)
            ('DEV-NULL 'NA))
          (dests mod))
  mod)

;; ----------------------------------------------------------------------
;; PARSER
;; ----------------------------------------------------------------------

(defparameter *module-regex* (re:create-scanner "(.+) -> (.+)"))

(defun find-feeders (endpoint modules)
  "given an endpoint (like a CONJUNCTION module) returns a list of
modules that send to it"
  (iter (for (key val) in-hashtable modules)
    (when (member endpoint (dests val) :test 'equal)
      (collect key))))

(defun parse-modules (los)
  "given a list of strings describing a series of communication modules,
create a hash-table with the keys being the module name as a string
and a value being a struct of module type"
  (let ((modules (make-hash-table :test 'equal :size (length los))))

    (setf modules
          (iter (for module in los)
            (re:register-groups-bind (mod dest)
                (*module-regex* module) ; split into two parts

              (setf dest (re:split ", " dest)) ; make list of dests

              (tr:match (subseq mod 0 1)
                ("b" (setf (gethash mod modules)
                           (make-broadcast mod dest)))

                ("%" (setf (gethash (subseq mod 1) modules)
                           (make-flip-flop (subseq mod 1) dest)))

                ("&" (setf (gethash (subseq mod 1) modules)
                           (make-conjunction (subseq mod 1) dest)))

                (otherwise (error "What the hell is this? ~A" mod)))

              (finally (return modules)))))

    ;; create a dev null entry for pulses to nowhere name it "rx" for
    ;; part 2 (in example 2 it's output but the name is irrelevent as
    ;; long as the button push code refers to it as "rx"
    (setf (gethash "rx" modules)
          (make-devnull "rx" nil))

    ;; now initialize the history in all CONJUNCTION modules
    (iter (for (key val) in-hashtable modules)
      (when (equal (type-of val) 'CONJUNCTION)
        (setf (history val)
              (iter (for d in (find-feeders key modules))
                (collect (cons d 'LOW))))))

    modules))

(defun push-button (modules)
  "push button sending 'LOW to broadcaster, modifies modules in place to
reflect new state of modules after all the cascading pulses are done"
  (let ((next '())
        (work '()))

    (setf next
          (append
           (send-pulse
            "button" 'LOW (gethash "broadcaster" modules))
           next)) ; save output to NEXT list

    ;; process all NEXT commands
    (iter (while next)

      ;; copy next to work then clear next
      (setf work (copy-list next))
      (setf next '())

      ;; now process all the queued jobs in work
      (iter (for job in work)
        (let ((sender (first job))
              (type (second job))
              (module (gethash (third job) modules))) ; get actual module

          (when (null module)                     ; catcher not pitcher
            (setf module (gethash "rx" modules))) ; dest is bit-bucket

          (setf next
                (append
                 (send-pulse sender type module)
                 next)))))))

(defun day20-1 (los pushes)
  "given a list of strings describing a module based circuit, returns the
product of all high and low pulses sent after PUSHES button
presses (see the AoC page for a full explanation of what this does)"
  (let ((modules (parse-modules los)))

    ;; clear high and low counts
    (setf (low-count (gethash "broadcaster" modules)) 0)
    (setf (high-count (gethash "broadcaster" modules)) 0)

    ;; all set - let's get pushing
    (iter (for push from 1 to pushes)
      (push-button modules)
      (finally (return (* (high-count (gethash "broadcaster" modules))
                          (low-count (gethash "broadcaster" modules))))))))

(5a:test day20-1-test
  (setf *debug* nil)
  (5a:is (= 32 (day20-1 *example1* 1)))
  (5a:is (= 16 (day20-1 *example2* 1)))
  (5a:is (= 32000000 (day20-1 *example1* 1000)))
  (5a:is (= 11687500 (day20-1 *example2* 1000))))

#| ----------------------------------------------------------------------------
--- Part Two ---

"Reset all modules to their default states. Waiting for all pulses to
be fully handled after each button press, what is the fewest number of
button presses required to deliver a single low pulse to the module
named rx?"

LEO'S NOTES: Phew. I thought he was going to ask for a trillion pulses
here. Noting that the pattern repeats in Example 2, I figured it would
be a matter of either memoizing the results or finding the repeat as
in an earlier day. This is a bit different. I just have to count the
pulses until I get to the first instance of sending a single low pulse
to rx. (in part 1 rx is a devnull). I wonder why Eric di`d not provide
an example for testing this part. That's the first time this year.

OK I see the problem. The number of button pushes is QUITE high. I
gave up at 1 million. (Which took 10 seconds). So I do have to find
the repeating pattern. I guess. Let me think.

I think I need to work backwards. First what condition has to happen
to get a 'LOW to "rx"?

Very specifically to MY particular problem set, "rx" is fed by a
CONJUNCTION "dh" alone. To get "dh" to send a 'LOW we have to get its
feeders (four CONJUNCTIONS tr xm dr nh) to send 'HIGH to 'dh'. Each of
these has exactly one feeder, hd tn vc and jx, each a conjunction. So
if each of (hd tr vc jx) sends 'LOW they'll trigger a cascade that
ends in LOW for rx. hd tn vc and jx have multiple feeders each so we
can't wait for all those to go LOW so the trail goes cold here. The
chokepoints we're looking for then are hd tn vc and jx each sending a
LOW. Or conversely tr xm dr and nh each receiving a LOW so they'll
send HIGH to dh so it sends a LOW to rx. So the solution involves
finding how many pushes it takes to get LOW to tr xm dr and nh
respectively then finding the LCM of those pushes.

hd -> LOW -> tr -> HIGH \
tn -> LOW -> xm -> HIGH  \
..........................dh -> LOW -> rx
vc -> LOW -> dr -> HIGH  /
jx -> LOW -> nh -> HIGH /

That solves it for my particular problem set, but I want to make a
more general solution. I think it's likely that everybody's problem
set ends in four conjunctions which feed a single conjunction which
feeds rx. So I can make more general code to find that chokepoint. I
guess I can assume that the problem sets have all the same structure -
only the names have been changed to annoy the solvers.

---------------------------------------------------------------------------- |#

(defun find-chokepoint (modules)
  "find the upstream feeders that send the appropriate pulses to start
the chain to send a low to rx. I'll go as far upstream as I can
without exceeding four 'CONJUNCTIONS. This makes the extreme
assumption that all the problem sets have the same final structure as
mine."
  (let ((current nil)
        (next (list "rx")))

    (iter (while (<= (length next) 4))
      (setf current next)
      (setf next
            (iter (for mod in current)
              (appending (find-feeders mod modules))))

      (finally (return current)))))

(5a:test find-chokepoint-test
  (5a:is (equal (find-chokepoint
                 (parse-modules (uiop:read-file-lines *data-file*)))
                (list "hd" "tn" "vc" "jx"))))

(defun reset-modules (modules)
  "given a hash table of modules - reset all FLIP-FLOP STATEs to 'OFF and CONJUNCTION HISTORYs to 'LOW. I need this because each time we push the button the modules hash table is changed. For each iteration of PULSE-UNTIL I need to be able to start with a clean slate. Resets the hash table in place"
  (flet ((reset (mod)
           (cond ((equal (type-of mod) 'FLIP-FLOP)
                  (setf (state mod) 'OFF))

                 ((equal (type-of mod) 'CONJUNCTION)
                  (setf (history mod)
                        (mapcar #'(lambda (m) (setf m (cons (car m) 'LOW)))
                                (history mod)))))
           mod))

    (iter (for (name mod) in-hashtable modules)
      (setf (gethash name modules)
            (reset mod)))))

(defun pulse-until (source pulse-type modules)
  "resets modules to original state, then pulses until the SOURCE module
sends the given PULSE-TYPE, return the number of button pushes
required"
  (let ((next '())
        (work '()))

    (reset-modules modules) ; clear modules

    ;; all set - let's get pushing
    (iter (for pushes from 1) ; to infinity and beyond!

      ;; push button sending 'LOW to broadcaster to kick off
      (setf next
            (append
             (send-pulse
              "button" 'LOW (gethash "broadcaster" modules))
             next)) ; save output to NEXT list

      ;; process all NEXT commands
      (iter (while next)
        ;; (format t "~%Next: ~A" next)
        ;; copy next to work then clear next
        (setf work (copy-list next))
        (setf next '())

        ;; now process all the queued jobs in work
        (iter (for job in work)
          (let ((sender (first job))
                (type (second job))
                (module (gethash (third job) modules))) ; get actual module

            (when (null module)                     ; catcher not pitcher
              (setf module (gethash "rx" modules))) ; dest is bit-bucket

            (if (and (eql pulse-type type)
                     (equal source sender))
                ;; satifies starting condition
                (return-from pulse-until pushes)
                ;; else keep going
                (setf next
                      (append
                       (send-pulse sender type module)
                       next)))))))))

(5a:test pulse-until-test
  (let ((mods (parse-modules *example2*)))
    (5a:is (= 2 (pulse-until "a" 'LOW mods)))
    (5a:is (= 2 (pulse-until  "inv" 'HIGH mods)))))

(defun day20-2 (los)
  (let* ((modules (parse-modules los))
         (feeders (find-chokepoint modules)))

    (apply #'lcm
           (iter (for module-name in feeders)
             (collect (pulse-until module-name 'LOW modules))))))

(5a:test day20-2-test
  (5a:is (= 207652583562007 (day20-2 (uiop:read-file-lines *data-file*))))
  (5a:is (= 224602953547789 (day20-2 (uiop:read-file-lines *data-file2*)))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 20 Part 1 is ~a"
              (day20-1 (uiop:read-file-lines *data-file*) 1000)))

(time (format t "The answer to AOC 2023 Day 20 Part 2 is ~a"
              (day20-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to AOC 2023 Day 20 Part 1 is 763500168
;; Evaluation took:
;; 0.007 seconds of real time
;; 0.007648 seconds of total run time (0.007515 user, 0.000133 system)
;; 114.29% CPU
;; 8,058,000 bytes consed

;; The answer to AOC 2023 Day 20 Part 2 is 207652583562007
;; Evaluation took:
;; 0.103 seconds of real time
;; 0.103532 seconds of total run time (0.102807 user, 0.000725 system)
;; [ Real times consist of 0.003 seconds GC time, and 0.100 seconds non-GC time. ]
;; [ Run times consist of 0.003 seconds GC time, and 0.101 seconds non-GC time. ]
;; 100.97% CPU
;; 121,528,608 bytes consed
