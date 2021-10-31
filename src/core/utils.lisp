(in-package :lisch8/core) 

(deftype int-8 ()  '(unsigned-byte 8))
(deftype int-16 ()  '(unsigned-byte 16))

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
