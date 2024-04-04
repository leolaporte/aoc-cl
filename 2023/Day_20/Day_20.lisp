;;;; Day20.lisp
;;;; 2023 AOC Day 20 solution
;;;; Leo Laporte
;;;; Started: 31 March 2024

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

(defgeneric send-pulse (mod source type)
  (:documentation "sends a 'HIGH or 'LOW TYPE pulse to a module named
 MOD from module SOURCE - the module will process the pulse according
 to its rules and output a list of next pulses to send as (list (list
 to from pulse-type))"))

(defmethod send-pulse :before ((mod module) source type)
  "before we process the pulse, make a record of it"
  (if (equal type 'LOW)
      (incf (low-count mod))   ; count incoming low
      (incf (high-count mod)))) ; count incoming high

(defmethod send-pulse :after ((mod module) source type)
  "after the pulse is sent display what happened for debugging"
  (format t "~%~A -~A-> ~A" source (string-downcase type) (name mod))
  )

(defmethod send-pulse ((mod flip-flop) source type)
  "a pulse sent to a flip-flop module outputs a list of next pulses to
send"
  (declare (ignore source))

  (when (equal type 'LOW)             ; ignore 'HIGH pulses
    (cond ((equal (state mod) 'OFF)
           (setf (state mod) 'ON)
           (iter (for m in (dests mod))
             (collect (list m (name mod) 'HIGH))))

          ((equal (state mod) 'ON)
           (setf (state mod) 'OFF)
           (iter (for m in (dests mod))
             (collect (list m (name mod) 'LOW)))))))

(defmethod send-pulse ((mod broadcast) source type)
  "trigger the broadcaster module to create a list of TYPE pulses to send
to all its dests as (list (cons mod type))"
  (declare (ignore source))
  ;; queue up pulses
  (iter (for m in (dests mod))
    (collect (list m (name mod) type))))

(defmethod send-pulse ((mod conjunction) source type)
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
      (collect (list m (name mod) pulse-type)))))

(defmethod send-pulse ((mod devnull) source type)
  "the bit bucket - drops the pulse"
  (declare (ignore source)) (declare (ignore type))
  ())

;; (defmethod print-object ((mod module) stream)
;;   "overrides the standard print-object to print more info in a human
;; readable form"
;;   (format stream "#<~S D>"
;;           (type-of mod))
;;   mod)

;; (defmethod describe ((mod module))
;;   "overides the standard describe to deliver more info in a human
;;  readable form"
;;   (format t "~&~S is a module of type ~S named ~A."
;;           mod
;;           (type-of mod)
;;           (name mod))
;;   mod)

;; ----------------------------------------------------------------------
;; PARSER
;; ----------------------------------------------------------------------

(defparameter *module-regex* (re:create-scanner "(.+) -> (.+)"))

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

    ;; create a dev null entry for pulses to nowhere
    (setf (gethash "rx" modules) ; call it RX for part two
          (make-devnull "rx" nil))

    ;; now initialize the history in all CONJUNCTION modules
    (iter (for (key module) in-hashtable modules)
      (when (typep module 'CONJUNCTION)
        (setf (gethash key modules)
              (iter (for (k v) in-hashtable modules)
                (when (member key (dests v) :test 'equalp)
                  (push (cons k 'LOW) (history module)))
                (finally (return module))))))

    modules))

(defun pht (hash)
  "utility to print hashes"
  (iter (for (key val) in-hashtable hash)
    (format t "~%~S => ~S" key val)))

(defun day20-1 (los pushes)
  "given a list of strings describing a module based circuit, returns the
product of all high and low pulses sent after PUSHES button
presses (see the AoC page for a full explanation of what this does)"
  (let ((modules (parse-modules los))
        (next '())
        (work '()))

    ;; clear high and low counts
    (setf (low-count (gethash "broadcaster" modules)) 0)
    (setf (high-count (gethash "broadcaster" modules)) 0)

    ;; all set - let's get pushing
    (iter (for ps below pushes)

      ;; push button sending 'LOW to broadcaster to kick off
      (setf next
            (append
             (send-pulse
              (gethash "broadcaster" modules) "button" 'LOW)
             next)) ; save output to NEXT list

      ;; process all NEXT commands
      (iter (while next)

        ;; copy next to work then clear next
        (setf work (copy-list next))
        (setf next '())

        ;; now process all the queued jobs in work
        (iter (for job in work)

          (let ((mod (gethash (first job) modules)))
            (when (null mod) ; check for receiver modules
              (setf mod (gethash "rx" modules)))

            (setf next (append
                        (send-pulse mod (second job) (third job))
                        next)))))

      (finally (return (* (high-count (gethash "broadcaster" modules))
                          (low-count (gethash "broadcaster" modules))))))))

(5a:test day20-1-test
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
in an earlier day. This is a bit easier. I just have to count the
pulses until I get to the first instance of sending a single low pulse
to rx. (in part 1 rx is a devnull).

OK I see the problem. The number of button pushes is QUITE high. I
gave up at 1 million. (Which took 10 seconds). So I do have to find
the repeating pattern. I guess. Let me think.

---------------------------------------------------------------------------- |#


(defun day20-2 (los pushes)
  "given a list of strings describing a module based circuit, returns the
product of all high and low pulses sent after PUSHES button
presses (see the AoC page for a full explanation of what this does)"
  (let ((modules (parse-modules los))
        (next '())
        (work '()))

    ;; clear high and low counts
    (setf (low-count (gethash "broadcaster" modules)) 0)
    (setf (high-count (gethash "broadcaster" modules)) 0)

    ;; all set - let's get pushing
    (iter (for ps below pushes)

      ;; push button sending 'LOW to broadcaster to kick off
      (setf next
            (append
             (send-pulse
              (gethash "broadcaster" modules) "button" 'LOW)
             next)) ; save output to NEXT list

      ;; process all NEXT commands
      (iter (while next)

        ;; (format t "~%Next: ~A" next)
        ;; copy next to work then clear next
        (setf work (copy-list next))
        (setf next '())

        ;; now process all the queued jobs in work
        (iter (for job in work)
          (let ((mod (gethash (first job) modules)))

            (when (null mod)
              (setf mod (gethash "rx" modules))
              (format t "~%rx is receiving ~A" (third job)))

            (setf next (append
                        (send-pulse mod (second job) (third job))
                        next)))))

      (finally (return (* (high-count (gethash "broadcaster" modules))
                          (low-count (gethash "broadcaster" modules))))))))

;; now solve the puzzle!
(time (format t "The answer to AOC 2023 Day 20 Part 1 is ~a"
              (day20-1 (uiop:read-file-lines *data-file*) 1000)))

(time (format t "The answer to AOC 2023 Day 20 Part 2 is ~a"
              (day20-2 (uiop:read-file-lines *data-file*) 1000)))


;; ----------------------------------------------------------------------------
;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
;; ----------------------------------------------------------------------------
