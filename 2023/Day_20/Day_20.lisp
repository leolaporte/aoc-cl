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
CONVEYOR, and BROADCASTER. And a generic function PULSE-MODULE which
will have different methods for each class.

Each pulse send will be handled according to the rules for each kind
of module which will output a list of the next pulses to send
as (list (list to from type)). I need to track FROM for the Conveyor
modules. (NB I'm not sure yet how to set up the Conveyor history)

On each button push, I'll accumulate a list of next pulses in a list
called NEXT, counting all the 'HIGH and 'LOW sends as I do, until all
the sends are processed, then I'll make a copy of the NEXT list into
WORK, clear the NEXT list, and process WORK, populating the new NEXT
list. When, after processing the entire WORK list there are no more
NEXTs, the round is done and the next button push will be
processed. This will happen PUSHES times. Then I'll return the product
of all the 'HIGH and 'LOW pulses sent.

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
    :reader name)
   (dests
    :initarg :dests
    :type list
    :initform '()
    :accessor dests
    :documentation "output modules connected to this module"))
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


;; ----------------------------------------------------------------------
;; METHODS
;; ----------------------------------------------------------------------

(defgeneric pulse-module (mod source type)
  (:documentation "sends a 'HIGH or 'LOW TYPE pulse to a module named
 MOD from module SOURCE - the module will process the pulse according
 to its rules and output a list of next pulses to send as (list (list
 to from pulse-type))"))

(defmethod pulse-module ((mod flip-flop) source type)
  "a pulse sent to a flip-flop module outputs a list of next pulses to
send"
  (declare (ignore source))
  (when (equal type 'LOW) ; ignore 'HIGH pulses
    (cond ((equal (state mod) 'OFF)
           (setf (state mod) 'ON)
           (iter (for m in (dests mod))
             (collect (list m mod 'HIGH))))

          ((equal (state mod) 'ON)
           (setf (state mod) 'OFF)
           (iter (for m in (dests mod))
             (collect (list m mod 'LOW))))

          (t (error "unknown flip-flop state: ~A" (state mod))))))

(defmethod pulse-module ((mod broadcast) source type)
  "trigger the broadcaster module to create a list of TYPE pulses to send
to all its dests as (list (cons mod type))"
  (declare (ignore source))
  (iter (for m in (dests mod))
    (collect (list m mod type))))

(defmethod pulse-module ((mod conjunction) source type)
  "send a pulse to a conjunction module, which in-turn outputs a list of
all the modules to send pulses to as (list (cons mod type))"

  ;; track incoming pulse source and type
  (remove source (history mod) :key #'car :test 'equalp)
  (push (cons source type) (history mod))

  (let ((pulse-type (if (every #'(lambda (x) (eql (cdr x) 'HIGH))
                               (history mod)) ; all 'HIGH?
                        ;; then
                        'LOW
                        ;; else
                        'HIGH)))

    (iter (for m in (dests mod))
      (collect (list m mod pulse-type)))))


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

                (otherwise (error "malformed module ~A" module)))
              (finally (return modules)))))

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
  (let ((modules (parse-modules los))
        (low-pulses 0)
        (high-pulses 0)
        (next '())
        (work '()))

    (iter (for p below pushes) ; do 1000 times

      ;; push button sending 'LOW to broadcaster to kick off
      (push
       (pulse-module (gethash "broadcaster" modules) "button" 'LOW)
       next) ; save output to NEXT list

      ;; process all NEXT commands
      (iter (while next)
        (incf low-pulses (count 'LOW next :key #'third))
        (incf high-pulses (count 'HIGH next :key #'third))
        (setf work next) ; copy the next list
        (setf next '())  ; then clear it for the next round
        (iter (for command in work)
          (push
           (pulse-module (gethash (first command) modules)
                         (second command) (third command))
           next)))
      (finally (return (* high-pulses low-pulses))))))

(5a:test day20-1-test
  (5a:is (= 32000000 (day20-1 *example1* 1000)))
  (5a:is (= 11687500 (day20-1 *example2* 1000))))


#| ----------------------------------------------------------------------------
--- Part Two//kkkjjkhgfdr54j ---

---------------------------------------------------------------------------- |#

  ;; now solve the puzzle!
  ;; (time (format t "The answer to AOC 2023 Day 20 Part 1 is ~a"
  ;;	      (day20-1 (uiop:read-file-lines *data-file*) 1000)))

  ;; (time (format t "The answer to AOC 2023 Day 20 Part 2 is ~a"
  ;;	      (day20-2 (uiop:read-file-lines *data-file*))))

  ;; ----------------------------------------------------------------------------
  ;; Timings with SBCL on M3-Max MacBook Pro with 64GB RAM
  ;; ----------------------------------------------------------------------------
