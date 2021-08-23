(ql:quickload :rutils)

(defpackage :lisch8/core
  (:use :common-lisp :rutils))

(in-package :lisch8/core)

;;; Constants
(defconstant +mem-size+ 4096)
(defconstant +opcode-size+ 2) ; An opcode is 2 bytes large

;;; Types
(deftype int-8 ()  '(unsigned-byte 8))
(deftype int-16 ()  '(unsigned-byte 16))

;;; Data Structures
(defstruct chip
  (Vr (make-array 16 :element-type 'int-8)
   :type (simple-array int-8 (16)))
  (Ir 0 :type int-16)
  (delayr 0 :type int-8)
  (soundr 0 :type int-8)
  (counter 0 :type int-16)
  (stack (make-array 16 :element-type 'int-16
			:fill-pointer 0))
  (memory (make-array +mem-size+ :element-type 'int-8)))

(defstruct instruction asm func)

;;; Globals
(defparameter *chip*
  (make-chip))

(defparameter *cpu-function-table*
  (make-hash-table :size 37))

;;; General Helper functions
(declaim (inline hexadecimal-string-p))
(defun hexadecimal-string-p (hs)
  (and (stringp hs)
       (every (lambda (c) (digit-char-p c 16)) hs)))

(declaim (inline map-if))
(defun map-if (pred func seq)
  (loop for i in seq
	if (funcall pred i)
	   collect (funcall func i)
	else collect i))

(declaim (inline get-bit))
(defun get-bit (bit-pos num)
  (ldb (byte 1 bit-pos) num))

(declaim (inline charlist->string))
(defun charlist->string (charlist)
  (coerce charlist 'string))

(declaim (inline 8_+))
(defun 8_+ (a b)
  "Addition between eight bit integers with
   with the possibility of overflow"
  (let ((sum (+ a b)))
    (values (mod sum 256)
	    (if (> sum 255) 1 0))))

(declaim (inline 8_-))
(defun 8_- (a b)
  "Subtraction between eight bit integers with
   with the possibility of overflow"
  (values (mod (- a b) 256)
	  (if (> b a) 1 0)))

(defun 8_<< (a)
  "Left bitwise shift which also
   returns the most significant bit."
  (values (ash a 1)
	  (get-bit 0 a)))
	    
(defun 8_>> (a)
  "Right bitwise shift which also
   returns the least significant bit."
  (values (ash a -1)
	  (get-bit 7 a)))

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
		 (aref src i))))

;;; Instruction DSL
;; Those macros/functions are responsible for implementing a small DSL
;; for defining the instruction set. 
(defun reduce-op-tokens (op-tokens)
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
   produced by #reduce-op-tokens# and a list
   of the symbols.
   Ex: \"5 x kk\" => (#x5000 (X KK))"
  (let ((splitted (split #\SPACE string)))
    (list (reduce-op-tokens splitted)
          (mapcar #'mksym (remove-if #'hexadecimal-string-p splitted))))))

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
  (once-only ((echip chip-obj))
   `(with-accessors ((@I chip-ir)
	             (@DT chip-delayr)
		     (@ST chip-soundr) 
		     (@PC chip-counter)
		     (@V chip-vr)
		     (@MEM chip-memory)
		     (@STACK chip-stack))
                     ,echip
      (let-places ((@V (x) (aref @V x))
		   (@MEM (x) (aref @MEM x)))
        ,@body))))
		       

(defmacro definstruction (name opcode &body body)
 "Definstruction creates an anonymous function whose
  parameter list is composed of the symbols contained in <opcode>
  and wraps its body in a with-chip-registers macro. The
  new function is then stored in the #func# slot of the 
  #instruction# struct along with <name> on the asm slot.
    This new instruction is then stored in *cpu-function-table*
  with a key defined by the result of a reduce on <opcode>
  where each number or symbol is treated as digits of a
  single hexadecimal value, and each character of a symbol
  count as a zero. For instance, (1 2 nnn) would become #x12000.
    The <name> parameter must be a sexp representation of the
  instruction's assembly code. See #make-asm-sexp# for more details."
  `(flet ((self ,(second opcode)
            (with-chip-registers *chip* ,@body)))
     (setf (gethash ,(first opcode) *cpu-function-table*)
	   (make-instruction :asm ',(sanitize-sexp-asm name) :func #'self)))))

;; Functions for disassembling

(defun sanitize-sexp-asm (asm)
  (labels ((first-symbol-char (s)
	     (char (symbol-name s) 0))
           (variablep (s)
	     (let ((fcs (first-symbol-char s)))
	       (or (eql fcs #\@) (eql fcs #\$)))))
    (unless (and (every #'symbolp asm)
	         (not (variablep (first asm))))
      (error "Invalid sexp-asm."))
    (map-if #'variablep #'first-symbol-char asm)))

(defun format-sexp-asm (asm vars)
  (map-if #'characterp
	  (lambda (x)
	    (intern (format nil "~c~4,'0x" x (pop vars)) :keyword))
	  asm))

(defun load-rom (chip pathname)
  (with-chip-registers chip
    (with-open-file (fd pathname :element-type 'unsigned-byte)
      (loop for a = (read-byte fd nil)
	    for i from 200
	    while a do (setf (@MEM i) a)))))
  
(defun run (chip)
  (with-chip-registers chip
    (setf @PC 200)
    (loop ())))
		       
;; CHIP-8 instructions
;; See http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;; For a full documentation of those.

(definstruction (:CLS) #[00E0]
  (print "CLS not implemented."))

(definstruction (:RET) #[00EE]
  (setf @PC (vector-pop @STACK)))

(definstruction (:LD :$addr) #[1 nnn]
  (setf @PC nnn))

(definstruction (:CALL :$addr) #[2 nnn]
  (vector-push @PC @STACK)
  (setf @PC nnn))

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

(definstruction SNE-y #[E x 9E]
  ;;(key-pressed-p (@V x)))
  (print "key-pressed-p not implemented."))

(definstruction SNE-y #[#xE x #x9E]
  ;;(not (key-pressed-p (@V x))))
  (print "key-pressed-p not implemented.")))

(definstruction (:LD :@x :$byte) #[6 x byte]
  (setf (@V x) byte))

(definstruction (:ADD :@x :$byte) #[7 x kk]
  (incf (@V x) kk))

(definstruction (:LD :@x :y) #[8 x y 0]
  (setf (@V x) (@V y)))

(definstruction (:OR :@x :@y) #[8 x y 1]
  (setf (@V x) (logior (@V x) (@V y))))

(definstruction (:AND :@x :@y) #[8 x y 2]
  (setf (@V x) (logand (@V x) (@V y))))

(definstruction (:XOR :@x :@y) #[8 x y 3]
  (setf (@V x) (logxor (@V x) (@V y))))

(macrolet ((defarithmetical (name args body)
	     `(definstruction ,name ,args
	        (multiple-value-bind (value flag) ,body
		  (setf (@V ,(cadr body)) value)
		  (setf @VF flag)))))

  (defarithmetical ADD-y #[8 x y 4]
    (8_+ (x y))

  (defarithmetical SUB-y #[8 x y 5]
    (8_- x y))

  (defarithmetical SHR-y #[8 x y 6]
    (8_>> x y))

  (defarithmetical SUBNy #[8 x y 7]
    ((lambda (x y) (8_- y x)) x y))

  (defarithmetical SHL-y #[8 x y #xE]
    (8_<< x y))))

(definstruction (:LD :I :$byte) #[A nnn]
  (setf @I nnn))

(definstruction (:JP :V0 :$addr) #[B nnn]
  (setf @I (+ nnn (@V 0))))

(definstruction (:RND :@x :$byte) #[C x kk]
  (setf (@V x) (logand (random-byte) kk)))

(definstruction (:DRW :@x :@y :$nibble) #[D x y n]
  (print "DRW is not implemented"))

(definstruction (:LD :@x :DT) #[F x 07]
  (setf (@V x) @DT))

(definstruction RD-key #[F x 0A]
  ;; (setf (@V x) (read-key)))
  (print "Read-key not implemented."))

(definstruction LD-delay (#xF 4b!x #x15)
  (setf @DT (@V x)))

(definstruction LD-sound (#xF 4b!x #x18)
  (setf @ST (@V x)))

(definstruction ADD-I (#xF 4b!x #x1E)
  (setf @I (+ (@V x) @I)))

(definstruction RD-font (#xF 4b!x #x29))
  ;;Not yet implemented.

(definstruction RD-BCD (#xF x #x33)
  (loop for d in (digits 3 (@V x))
	for a from @I
	(setf (@MEM a) d)))

(definstruction LD-[i] (#xF x #x55)
  (memory-copy @V @MEM x :dest-offset @I))

(definstruction RD-[i] (#xF x #x65)
  (memory-copy @MEM @V x :src-offset @I))




(defmacro with-byte-chunks (large-byte chunks &body body)
   "Binds a variable number of symbols to a specified\
    to a form which retrieves a specified chunk of the\
    parameter <large-byte>." 
  `(symbol-macrolet
       ,(loop for (name size off) in chunks
	      collect `(,name (ldb (byte ,size ,off)
				   ,large-byte)))
     ,@body))

(defconstant +mask0+ #xFFFF)
(defconstant +mask1+ #xFF0F)
(defconstant +mask2+ #xF00F)
(defconstant +mask3+ #xF000)

(declaim (inline classify-opcode))
(defun classify-opcode (op)
  "Returns the opcode variables and a\
   constant value for masking them."
  (with-byte-chunks op
      ((id 4 12)
       (x 4 8)
       (y 4 4)
       (n 4 0)
       (kk 8 0)
       (nnn 12 0))
    (case id
      (#x0               (list +mask0+ nil))
      (#xC               (list +mask3+ x kk n))
      ((#xD #xF)         (list +mask1+ x))
      ((#x5 #x8 #x9)     (list +mask2+ x y))
      ((#x1 #x2 #xA #xB) (list +mask3+ nnn))
      ((#x3 #x4 #x6 #x7) (list +mask3+ x kk)))))

(defun disasm (op)
  (destructuring-bind (mask &rest args) (classify-opcode op)
    (format-sexp-asm (instruction-asm (gethash (logand mask op) *cpu-function-table*)) args)))

    

;(defun process-opcode (chip op)
;  "Call the opcode associated function along
;   with it's arguments."
;  (destructuring-bind (mask &rest args) (classify-opcode op)
;    (apply (gethash (logand op mask) *cpu-function-table*) (cons op args))))
		    
