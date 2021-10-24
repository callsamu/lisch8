(ql:quickload :rutils)
(ql:quickload :chanl)

(defpackage :lisch8/core
  (:use :common-lisp :rutils :chanl)
  (:export
   :chip :screen
   :run+clock :run
   :load-rom
   :*output-channel*
   :width :height :memory))

(in-package :lisch8/core)

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

;;; Constants
(defconstant +initial-address+ #x200
  "Program counter's initial value")
(defconstant +font-sprite-size+ 5)
(defconstant +mem-size+ 4096)
(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)
(defconstant +opcode-size+ 2) ; An opcode is 2 bytes large

;;; Types
(deftype int-8 ()  '(unsigned-byte 8))
(deftype int-16 ()  '(unsigned-byte 16))

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
   
(defclass screen ()
  ((width
    :initform +screen-width+
    :reader width)
   (height
    :initform +screen-height+
    :reader height)
   (refresh?
    :initform t
    :accessor refresh?)
   (memory
    :initform nil
    :reader memory)))

(defmethod initialize-instance :after ((this screen) &key)
  (with-slots (width height memory) this
    (setf memory (make-array (* width height) :element-type 'bit))))

;;; Channels
(defparameter *output-channel* (make-instance 'channel))

;;; Globals
(defparameter *chip*
  (make-instance 'chip))

(defparameter *cpu-function-table*
  (make-hash-table :size 37))

;; General Helper functions
(defun hexadecimal-string-p (hs)
  (and (stringp hs)
       (every (lambda (c) (digit-char-p c 16)) hs)))

(defun make-clock ()
  (let ((time (get-internal-real-time)))
    (lambda (&optional reset)
      (if reset
	  (setf time (get-internal-real-time))
	  (float (/ (- (get-internal-real-time) time)
		    internal-time-units-per-second))))))

(declaim (inline map-if))
(defun map-if (pred func seq)
  (loop for i in seq
	if (funcall pred i)
	   collect (funcall func i)
	else collect i))

(declaim (inline get-bit))
(defun get-bit (bit-pos num)
  (ldb (byte 1 bit-pos) num))

(declaim (inline concat-bytes))
(defun concat-bytes (b1 b2)
  (+ (ash b1 8) b2))

(declaim (inline charlist->string))
(defun charlist->string (charlist)
  (coerce charlist 'string))

(declaim (inline byte-add))
(defun byte-add (a b)
  "Addition between eight bit integers with
   with the possibility of overflow"
  (let ((sum (+ a b)))
    (values (mod sum 256)
	    (if (> sum 255) 1 0))))

(declaim (inline byte-sub))
(defun byte-sub (a b)
  "Subtraction between eight bit integers with
   with the possibility of overflow"
  (values (mod (- a b) 256)
	  (if (> b a) 1 0)))

(defun byte-lshift (a)
  "Left bitwise shift which also
   returns the most significant bit."
  (values (mod (ash a 1) 256)
	  (get-bit 7 a)))
	    
(defun byte-rshift (a)
  "Right bitwise shift which also
   returns the least significant bit."
  (values (ash a -1)
	  (get-bit 0 a)))

(declaim (inline random-byte))
(defun random-byte ()
  "Generates a random byte."
  (the int-8 (random 256)))

(declaim (inline list-n-digits))
(defun list-n-digits (n num)
  (when (not (integerp n))
    (error "Number of digits to be listed must be an integer"))
  (labels ((rec (src acc count)
	     (if (zerop count)
		 acc
		 (rec (floor src 10)
		      (cons (mod src 10)
			    acc)
		      (1- count)))))
    (rec num nil n)))

(declaim (inline memory-copy))
(defun memory-copy (src dest bytes &key (src-offset 0) (dest-offset 0))
  "Copies *bytes* elements from *src* starting at *src-offset* to
   *dest*, starting at *dest-offset*"
  (loop for i from src-offset to (+ (1- bytes) src-offset)
	for j from dest-offset
	do (setf (aref dest j)
		 (aref src i))
	finally (return dest)))
	   

(defmacro xorf (place value)
  `(setf ,place (logxor ,place ,value)))

;; For screen drawing
(declaim (inline collisionp))
(defun collisionp (bit result)
  (logand (lognot result) bit))

(declaim (inline draw-bit))
(defun draw-bit (index bit screen)
  (unless (null index)
    (collisionp bit (xorf (aref (memory screen) index) bit))))

(defun draw-byte (x y byte screen)
  (flet ((coords (x y)
	   (+ x (* y (width screen)))))
    (let ((x (mod x (width screen)))
	  (y (mod y (height screen))))
      (rutils:iter
        (:with start := (coords x y))
        (:with end := (coords (width screen) y))
        (:with flag := 0)
        (:for i :from 7 :downto 0)
        (:for p :from start :to end)
        (:for bit := (get-bit i byte))
        (:for collision := (draw-bit p bit screen))
        (when (= 1 collision) (setf flag 1))
        (:finally (return-from draw-byte flag))))))

(defun draw-sprite (x y sprite screen)
  (setf (refresh? screen) t)
  (rutils:iter
    (:for b :in sprite)
    (:for i :from y :to (height screen))
    (:with flag := 0)
    (setf flag (logior flag (draw-byte x i b screen)))
    (:finally (return flag))))

;; Keyboard Input
(declaim (inline key-pressed-p))
(defun key-pressed-p (keynum keyboard)
  (not (zerop (aref keyboard keynum))))

;; Assembly Related
(defclass instruction ()
  ((assembly
    :initarg :asm
    :initform
    (error
     ":asm parameter must be supplied to
      instance of class instruction")
    :type list
    :reader assembly
    :documentation
     "Sexp assembly representation
      of the instruction.")
   (function
    :initarg :func
    :initform 
    (error
     ":func parameter must be supplied to
      instance of class instruction")
    :type function
    :reader func
    :documentation
    "Function object which
     actually implements
     the instruction")))

(defmethod initialize-instance :after ((this instruction) &key)
  (with-slots ((asm assembly) (func function)) this
    (setf asm (sanitize-sexp-asm asm))))

(defgeneric disassembly (instruction &optional variables)
  (:documentation
    "Disassembly method for any object which can
     refer to an instruction value"))

(defmethod disassembly ((opcode integer) &optional v)
  (declare (ignorable v))
  (destructuring-bind (instruction vars)
      (retrieve-instruction opcode)
    (disassembly instruction vars)))

(defmethod disassembly ((this instruction) &optional vars)
  (format-sexp-asm (assembly this) vars))

(defun sanitize-sexp-asm (asm)
  (labels ((first-symbol-char (s)
	     (char (symbol-name s) 0))
           (variablep (s)
	     (let ((fcs (first-symbol-char s)))
	       (or (eql fcs #\@)
		   (eql fcs #\$)))))
    (assert (and (not (variablep (first asm)))
                 (every #'symbolp asm)))
    (map-if #'variablep #'first-symbol-char asm)))

(defun format-sexp-asm (asm vars)
  (flet ((format-var (prefix)
	   (let ((directive (if (char= prefix #\@)
				"~c~1,'0x"
				"~c~4,'0x")))
	     (intern (format nil directive prefix (pop vars)) :keyword))))
  (map-if #'characterp #'format-var asm)))

;; Instruction Table
(defconstant +mask0+ #xFFFF)
(defconstant +mask1+ #xF0FF)
(defconstant +mask2+ #xF00F)
(defconstant +mask3+ #xF000)

(defmacro with-byte-chunks (large-byte chunks &body body)
   "Binds a variable number of symbols to a specified
    to a form which retrieves a specified chunk of the
    parameter <large-byte>." 
  `(let ,(loop for (name size off) in chunks
	   collect `(,name (ldb (byte ,size ,off)
	     		        ,large-byte)))
      ,@body))
 
(declaim (inline classify-opcode))
(defun classify-opcode (op)
  "Returns the opcode variables and a
   constant value for masking them."
  (with-byte-chunks op
      ((id 4 12)
       (x 4 8)
       (y 4 4)
       (n 4 0)
       (kk 8 0)
       (nnn 12 0))
    (ecase id
      (#x0               (list +mask0+ nil))
      (#xD               (list +mask3+ x y n))
      ((#xE #xF)         (list +mask1+ x))
      ((#x5 #x8 #x9)     (list +mask2+ x y))
      ((#x1 #x2 #xA #xB) (list +mask3+ nnn))
      ((#x3 #x4 #x6 #x7 #xC) (list +mask3+ x kk)))))

(let ((table (make-hash-table :test 'eq :size 47)))
  (defun add-instruction (opcode instruction)
    "Adds an <instruction> with key <opcode> to the table"
    (setf (gethash opcode table) instruction))

  (defun retrieve-instruction (opcode)
    "Looks for the opcode ANDed with one the masks
     returned by the classification function.
     If the lookup result is nil, an error is signaled,
     otherwise, it is added to a pair with the instruction 
     variables and returned it."
    (destructuring-bind (mask &rest vars)
        (classify-opcode opcode)
      (if-it (gethash (logand mask opcode) table)
	(list it vars) 
	(error "Instruction not found: opcode ~4,'0x. is invalid" opcode)))))

;;; Instruction DSL

;; Those macros/functions are responsible for implementing a small DSL
;; for defining the instruction set. 
(defun reduce-opcode-tokens (op-tokens)
  "Receives a list of strings as input, substitutes each
   character of strings which don't contain hexnumbers for
   zeroes, concatenates all strings into a single one and
   and parse it as a integer.
   (\"12A\" \"var\") => #x12A000"
    (-> op-tokens
        (map-if (lambda (_)
		  (not (hexadecimal-string-p _)))
		(lambda (_)
		  (make-string (length _) :initial-element #\0))
		%)
	(apply #'concatenate 'string %)
	(parse-integer % :radix 16)))
	
(defun parse-opcode-string (string)
  "Parses an opcode string, which contains 
   symbols and hexnumbers, returning a list
   that holds its numeric representation, 
   produced by #reduce-opcode-tokens# and a list
   of the symbols.
   Ex: \"5 x kk\" => (#x5000 (X KK))"
  (let ((splitted (split #\SPACE string)))
    (list (reduce-opcode-tokens splitted)
          (mapcar #'mksym (remove-if #'hexadecimal-string-p splitted)))))

(defun |#[]-opcode-string-reader| (stream char1 char2)
 "A read macro for representing opcodes containing variables.
  Ex: #[1 nnn] => (#x1000 (NNN)) 
      #[00EE]  => (#x00EE NIL)
      #[5 x y 9] => (#x5009 (X Y))
  As you can see, it separates the variables from the
  numbers and replaces them for zeroes. The full number
  is stored on the left of a pair, while the symbols stay
  on a list, on the right side.
  Though this could have been done using normal macros, doing
  it so in read-time seemed easier and more reasonable."
  (declare (ignorable char1 char2))
    (parse-opcode-string
      (charlist->string
       (iter (:for c := (read-char stream nil))
         (:until (eql c #\]))
         (:collect c)))))

(set-dispatch-macro-character #\# #\[
  (function |#[]-opcode-string-reader|))

(defmacro let-places (places &body body)
  "<places> holds a serie of setfable expressions,
   and for each p in <places>, two functions p and 
   (setf p) are declared in local scope."
  (flet ((setfable (f)
	   (with-gensyms (newval)
	     (destructuring-bind (name args &rest body) f
	       `((setf ,name) (,newval ,@args) (setf ,@body ,newval))))))
  `(labels ,(loop for f in places
		  collect f
		  collect (setfable f))
     ,@body)))

 
(defmacro with-chip-registers (chip-obj &body body)
  "Wraps the body in both a macrolet and symbol-macrolet
   which define bindings for accessing the chip object's main slots.
   Each binding is prefixed by a @. Some, like @V, are actually setfable
   functions which return the element at position x of the slot, which must
   be an array."
 `(with-accessors ((@I i-reg)
	           (@DT delayt)
	           (@ST soundt) 
		   (@PC counter)
		   (@V v-regs)
		   (@MEM memory)
		   (@STACK stack)
		   (@SCREEN screen)
		   (@KBD keyboard)
		   (step-counter? step-counter?))
       ,chip-obj
    (let-places ((@V (x) (aref @V x))
	         (@MEM (x) (aref @MEM x)))
      ,@body)))

(defmacro definstruction (name opcode &body body)
  (destructuring-bind (key vars) opcode
    (with-gensyms (chip)
      `(add-instruction
	 ,key
         (make-instance 'instruction
           :asm ',name
           :func (lambda (,chip ,@vars)
	 	  (with-chip-registers ,chip ,@body)))))))

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
  (let ((*chip* chip))
    (loop (cycle chip) (sleep 1))))

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
		       
;; CHIP-8 instructions
;; See http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;; For a full documentation of those.

(definstruction (:SYS) #[0 nnn]
  (print "Unsuported instruction: SYS"))

(definstruction (:CLS) #[00E0]
  (iter (:for i :below (length (memory @SCREEN)))
    (setf (aref (memory @SCREEN) i) 0)))

(defmacro jump-to (address)
  "Set counter to address and prevent it
   from being incremented during cycle"
  `(setf @PC ,address
         step-counter? nil))

(definstruction (:RET) #[00EE]
  (setf @PC (vector-pop @STACK)))

(definstruction (:JMP :$addr) #[1 nnn]
  (jump-to nnn))

(definstruction (:CALL :$addr) #[2 nnn]
  (vector-push @PC @STACK)
  (jump-to nnn))

(defmacro branch-if (pred)
  `(when ,pred (incf @PC 2)))

(definstruction (:SE :@x :$byte) #[3 x kk]
  (branch-if (= (@V x) kk)))

(definstruction (:SNE :@x :$byte) #[4 x kk]
  (branch-if (/= (@V x) kk)))

(definstruction (:SE :@x :@y) #[5 x y 0]
  (branch-if (= (@V x) (@V y))))

(definstruction (:SNE :@x :@y) #[9 x y 0]
  (branch-if (/= (@V x) (@V y))))

(definstruction (:SKP :@x) #[E x 9E]
  (branch-if (key-pressed-p (@V x) @KBD)))

(definstruction (:SKNP :@x) #[E x A1]
  (branch-if (not (key-pressed-p (@V x) @KBD))))

(definstruction (:LD :@x :$byte) #[6 x kk]
  (setf (@V x) kk))

(definstruction (:ADD :@x :$byte) #[7 x kk]
  (setf (@V x) (byte-add (@V x) kk)))

(definstruction (:LD :@x :@y) #[8 x y 0]
  (setf (@V x) (@V y)))

(definstruction (:OR :@x :@y) #[8 x y 1]
  (setf (@V x) (logior (@V x) (@V y))))

(definstruction (:AND :@x :@y) #[8 x y 2]
  (setf (@V x) (logand (@V x) (@V y))))

(definstruction (:XOR :@x :@y) #[8 x y 3]
  (setf (@V x) (logxor (@V x) (@V y))))

(defmacro do-arithmetic (operation x &rest ys)
  "Due to free variable injections, should 
   only be used inside with-chip forms.
   Apply the operation, set the first
   set the flag register to the flag value
   and return the result value."
  (with-gensyms (result flag)
    `(multiple-value-bind (,result ,flag)
	                  (,operation ,x ,@ys)
       (setf (@V #xF) ,flag)
       ,result)))

(definstruction (:ADD :@x :@y) #[8 x y 4]
  (setf (@V x) (do-arithmetic byte-add (@V x) (@V y))))

(definstruction (:SUB :@x :@y) #[8 x y 5]
  (setf (@V x) (do-arithmetic byte-sub (@V x) (@V y))))

(definstruction (:SHR :@x :@y) #[8 x y 6]
  (setf (@V x) (do-arithmetic byte-rshift (@V x))))

(definstruction (:SUBN :@x :@y) #[8 x y 7]
  (setf (@V x) (do-arithmetic byte-sub (@V y) (@V x))))

(definstruction (:SHL :@x :@y) #[8 x y E]
  (setf (@V x) (do-arithmetic byte-lshift (@V x))))

(definstruction (:LD :I :$byte) #[A nnn]
  (setf @I nnn))

(definstruction (:JP :V0 :$addr) #[B nnn]
  (setf @PC (+ nnn (@V 0))))

(definstruction (:RND :@x :$byte) #[C x kk]
  (setf (@V x) (logand (random-byte) kk)))

(definstruction (:DRW :@x :@y :$nibble) #[D x y n]
  (let ((sprite (iter (:for i :from @I :to (+ @I n -1))
                  (:collect (@MEM i)))))
    (setf (@V #xF)
	  (draw-sprite (@V x) (@V y) sprite @SCREEN))))

(definstruction (:LD :@x :DT) #[F x 07]
  (setf (@V x) @DT))

(definstruction (:LD :@x :K) #[F x 0A]
  (flet ((find-pressed-key (kbd)
	   (iter (:for i to #xF)
	     (when (key-pressed-p i kbd)
	       (return i)))))
    (if-it (find-pressed-key @KBD)
           (setf (@V x) it)
	   (decf @PC 2))))

(definstruction (:LD :DT :@x) #[F x 15]
  (setf @DT (@V x)))

(definstruction (:LD :ST :@x) #[F x 18]
  (setf @ST (@V x)))

(definstruction (:ADD :I :@x) #[F x 1E]
  (setf @I (+ (@V x) @I)))

(definstruction (:LD :F :@x) #[F x 29]
  (setf @I (* (@V x) +font-sprite-size+)))

(definstruction (:LD :B :@x) #[F x 33]
  (loop for d in (list-n-digits 3 (@V x))
	for a from @I
	do (setf (@MEM a) d)))

(definstruction (:LD :[I] :@[0..x]) #[F x 55]
  (memory-copy @V @MEM (1+ x) :dest-offset @I))

(definstruction (:LD :@[0..x] :[I]) #[F x 65]
  (memory-copy @MEM @V (1+ x) :src-offset @I))

