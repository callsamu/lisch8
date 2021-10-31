(defpackage :lisch8/core
  (:use :common-lisp :rutils :chanl)
  (:export
   :chip :screen
   :run+clock :run
   :load-rom
   :*output-channel*
   :width :height :memory))
