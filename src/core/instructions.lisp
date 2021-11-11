(in-package :lisch8/core)

;;; Assembly Representation

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

(defreadtable :instructions
  (:merge :standard)
  (:dispatch-macro-char #\# #\[
		   (function |#[]-opcode-string-reader|)))

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
