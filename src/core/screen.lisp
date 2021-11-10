(in-package :lisch8/core)

;; For screen drawing

(defconstant +screen-width+ 64)
(defconstant +screen-height+ 32)

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
  (with-slots (width height memory)
      this
    (setf memory (make-array (* width height)
			     :element-type 'bit))))

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

