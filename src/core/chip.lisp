(in-package :lisch8/core)

(defconstant +initial-address+ #x200
  "Program counter's initial value")
(defconstant +opcode-size+ 2) ; An opcode is 2 bytes large
(defconstant +mem-size+ 4096)
(defconstant +font-sprite-size+ 5)

;;; Chip8 font
(defconstant +font+
  #(#xF0 #x90 #x90 #x90 #xF0
    #x20 #x60 #x20 #x20 #x70
    #xF0 #x10 #xF0 #x80 #xF0
    #xF0 #x10 #xF0 #x10 #xF0
    #x90 #x90 #xF0 #x10 #x10
    #xF0 #x80 #xF0 #x10 #xF0
    #xF0 #x80 #xF0 #x90 #xF0
    #xF0 #x10 #x20 #x40 #x40
    #xF0 #x90 #xF0 #x90 #xF0
    #xF0 #x90 #xF0 #x10 #xF0
    #xF0 #x90 #xF0 #x90 #x90
    #xE0 #x90 #xE0 #x90 #xE0
    #xF0 #x80 #x80 #x80 #xF0
    #xE0 #x90 #x90 #x90 #xE0
    #xF0 #x80 #xF0 #x80 #xF0
    #xF0 #x80 #xF0 #x80 #x80))

;;; Main data structure
(defclass chip ()
  ((v-registers
    :type (simple-array int-8 (16))
    :initform
    (make-array 16
     :element-type 'int-8)
    :reader v-regs)
   (i-register
    :type int-16
    :initform 0
    :accessor i-reg)
  (delay-timer
   :type int-8
   :initform 0
   :accessor delayt)
  (sound-register
   :type int-8
   :initform 0
   :accessor soundt)
  (step-counter?
   :initform t
   :accessor step-counter?
   :documentation
   "If nil, prevents the program counter
    from being incremented during a cycle")
  (counter
   :type int-16
   :initform +initial-address+
   :accessor counter)
  (stack
   :initform
   (make-array 16
    :element-type 'int-16
    :fill-pointer 0)
   :reader stack)
  (memory
   :initform
   (make-array +mem-size+
     :element-type 'int-8)
   :reader memory)
  (screen
   :initform (make-instance 'screen)
   :reader screen)
  (keyboard
   :initform
   (make-array 16
    :element-type 'bit)
   :reader keyboard))) 

(defmethod initialize-instance :after ((this chip) &key)
  (memory-copy +font+ (memory this) #.(length +font+)))

;; Core functions
(defun load-rom (chip pathname)
  (with-open-file (fd pathname :element-type 'unsigned-byte)
    (read-sequence (memory chip) fd :start #x200)
    chip))
		     
(defun fetch (chip)
  (with-chip-registers chip
    (let ((b1 (@MEM @PC))
	  (b2 (@MEM (1+ @PC))))
      (concat-bytes b1 b2))))

(defun print-debug-info (chip op asm)
  (format t
    "OP: ~x Counter: ~x Delay: ~x~%V-REGS:~a ~%I-REG:~x I-MEM: ~a~%~a~%~%"
    op
    (counter chip)
    (delayt chip)
    (v-regs chip)
    (i-reg chip)
    (loop for i from (i-reg chip) to (+ #xF (i-reg chip))
	  collect (aref (memory chip) i))
    asm))
	     
(defun execute (chip opcode)
  (destructuring-bind (ins vars) (retrieve-instruction opcode)
    (print-debug-info chip opcode (disassembly ins vars))
    (when (null (car vars)) (setf vars nil))
    (apply (func ins) (cons chip vars))))
 
(defun cycle (chip)
  (let ((opcode (fetch chip)))
    (execute chip opcode)
    (if (step-counter? chip) 
      (incf (counter chip) 2)
      (setf (step-counter? chip) t))))

(defun run (chip)
  (loop (cycle chip) (sleep 1)))

(defconstant +timer-decrease-rate+ (float (/ 1 60)))

(defmacro decrease-timer (timer)
  `(unless (zerop ,timer) (decf ,timer)))

(defun run+clock (chip clockspeed)
  (with-accessors ((dt delayt) (st soundt) (screen screen)) chip
    (iter
      (:with timer := (make-clock))
      (:with chip-clock := (make-clock))
      (when (> (funcall timer) +timer-decrease-rate+)
        (decrease-timer dt)
	(decrease-timer dt)
        (funcall timer :reset))
      (when (> (funcall chip-clock) clockspeed)
	(cycle chip)
	(funcall timer :reset))
      (when (refresh? (screen chip))
	(send *output-channel* (screen chip) :blockp t)))))
	
