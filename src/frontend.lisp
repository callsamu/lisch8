(ql:quickload '(bodge-host bodge-ui bodge-ui/renderer))

(defpackage :lisch8/bodge
  (:use
   :cl
   :lisch8/core
   :bodge-ui
   :bodge-ui.renderer
   :bodge-math))

(in-package :lisch8/bodge)

(defparameter *window-width* (* 26 64))
(defparameter *window-height* (* 26 32))

(defparameter *output-channel*
  (make-instance 'chanl:channel))

(defparameter *input-channel*
  (make-instance 'chanl:channel))

(defclass emulator (bodge-host:window)
  ((context :initform nil)
   (renderer :initform nil)
   (enabled? :initform t :accessor enabled?)
   (mouse-actions :initform (list))
   (cursor :initform (bodge-math:vec2)))
  (:default-initargs
   :opengl-version '(3 3)
   :title "Lisch8"
   :width *window-width*
   :height *window-height*
   :autoscaled nil))

(defparameter *vertex-shader* "
   #version 330 core
   layout (location = 0) in vec3 aPos;
   layout (location = 1) in vec2 aTexCoord;
   out vec2 TexCoord;
   void main()
   {
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
      TexCoord = aTexCoord;
   }")

(defparameter *fragment-shader* "
   #version 330 core
   in vec2 TexCoord;
   uniform sampler2D Tex;
   out vec4 FragColor;
   void main()
   {
      float texred = texture(Tex, TexCoord).x * 256;
      FragColor = vec4(texred, texred, texred, 1.0) * vec4(0.0, 1.0, 0.0, 1.0);
   }")

(defvar *quad*
  #(-0.8 -0.8 0.0 0.0 1.0
    -0.8  0.8 0.0 0.0 0.0
    +0.8 -0.8 0.0 1.0 1.0
    -0.8  0.8 0.0 0.0 0.0
    +0.8 -0.8 0.0 1.0 1.0
    +0.8 +0.8 0.0 1.0 0.0))

(defun make-texture (width height data mode)
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:texture-parameter texture :texture-wrap-s :repeat)
    (gl:texture-parameter texture :texture-wrap-t :repeat)
    (gl:texture-parameter texture :texture-min-filter :nearest-mipmap-linear)
    (gl:texture-parameter texture :texture-mag-filter :nearest)
    (gl:tex-image-2d :texture-2d 0 mode width height 0 mode :unsigned-byte data)
    (gl:generate-mipmap :texture-2d)
    texture))
    
(defun build-shaders (vertexsrc fragsrc)
  (let ((vertex (gl:create-shader :vertex-shader))
	(fragment (gl:create-shader :fragment-shader))
	(program (gl:create-program)))
    (gl:shader-source vertex vertexsrc)
    (gl:shader-source fragment fragsrc)
    (gl:compile-shader vertex)
    (gl:compile-shader fragment)
    (gl:attach-shader program vertex)
    (gl:attach-shader program fragment)
    (gl:link-program program)
    program))

(defun make-vertex-array (vertexdata)
  (let ((buffer (gl:gen-buffer))
	(vtx-array (gl:gen-vertex-array)))
    (gl:bind-vertex-array vtx-array)
    (gl:bind-buffer :array-buffer buffer)
    (gl:buffer-data :array-buffer :static-draw vertexdata)
    (gl:vertex-attrib-pointer 0 3 :float :false 20 0)
    (gl:vertex-attrib-pointer 1 2 :float :false 20 12)
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    vtx-array))

(defmacro with-uniform ((var name shader) &body body)
  `(let ((,var (gl:get-uniform-location ,shader ,name)))
     ,@body))

(defun array->glarray (array)
  (loop with length = (length array)
	with glarray = (gl:alloc-gl-array :float length)
	finally (return glarray)
	for i to (1- length)
	do (setf (gl:glaref glarray i)
		 (coerce (aref array i)
			 'single-float))))

(defun setup-rendering-context (application)
  (bodge-host:bind-main-rendering-context application)
  (glad:init))

(defun initialize-ui (application)
  (with-slots (context renderer) application
    (setf renderer (make-nuklear-renderer *window-width* *window-height*)
          context (make-ui renderer :input-source application))))

(defun release-ui (application)
  (with-slots (context renderer) application
    (bodge-memory:dispose context)
    (destroy-nuklear-renderer renderer)))

(declaim (inline render-example-ui))
(defun render-example-ui (app)
  (with-slots (context) app
    (compose-ui context)))

(defun screen->texture (screen)
  (with-slots (width height memory) screen
    (make-texture width height memory :red)))

(defun run-rendering-loop (application)
  (let* ((vertex-data (array->glarray *quad*))
	 (vertex (make-vertex-array vertex-data))
	 (shader (build-shaders *vertex-shader* *fragment-shader*))
	 (texture (make-texture 64 32 (make-array #.(* 64 32) :element-type 'bit) :red)))
    (gl:clear-color 0.0 0.0 0.0 0.1)
    (do ()
	((enabled? application))
      (let ((refresh (chanl:recv *output-channel* :blockp nil)))
	(when refresh
	  (setf texture (screen->texture refresh))))
      (gl:bind-texture :texture-2d texture)
      (gl:clear-color 0.0 0.5 0.0 0.1)
      (gl:clear :color-buffer-bit)
      (gl:use-program shader)
      (gl:bind-vertex-array vertex)
      (gl:draw-arrays :triangles 0 6)
      (gl:bind-vertex-array 0)
      (render-example-ui application)
      (bodge-host:swap-buffers application))))

(defun start-rendering-thread (application)
  (with-slots (context renderer enabled?) application
    (bodge-concurrency:in-new-thread ("rendering-thread")
      (unwind-protect
           (progn
             (setup-rendering-context application)
             (initialize-ui application)
             (run-rendering-loop application)
             (release-ui application))
        (bodge-host:close-window application)))))

(defmethod bodge-host:on-mouse-action ((this emulator) button action)
  (with-slots (mouse-actions) this
    (setf mouse-actions
	  (nconc mouse-actions
		 (list (cons button action))))))

(defmethod bodge-host:on-cursor-movement ((this emulator) x y)
  (with-slots (cursor) this
    (setf (x cursor) x
          (y cursor) y)))

(defmethod bodge-host:on-init ((this emulator))
  (with-slots (context renderer enabled?) this
    (setf enabled? t)
    (start-rendering-thread this)))

(defmethod bodge-host:on-hide ((this emulator))
  (setf (enabled? this) nil))

(defmethod bodge-host:on-destroy ((this emulator))
  (setf (enabled? this) nil))

(defmethod next-mouse-interaction ((this emulator))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))

(defmethod last-cursor-position ((this emulator) &optional result-vec2)
  (with-slots (cursor) this
    (if result-vec2
        (progn
          (setf (x result-vec2) (x cursor)
                (y result-vec2) (y cursor))
          result-vec2)
        cursor)))

(defmethod bodge-host:on-key-action ((this emulator) key state)
  (format t "~a | ~a ~%" key state))

(defun run (rom)
  (bodge-host:open-window (make-instance 'emulator)))

