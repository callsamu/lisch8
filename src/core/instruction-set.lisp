(in-package :lisch8/core)

(in-readtable :instructions)

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

