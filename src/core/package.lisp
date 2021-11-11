(defpackage :lisch8/core
  (:use
   :common-lisp
   :rutils
   :chanl
   :named-readtables)
  (:export
   :chip
   :run+clock
   :run
   :load-rom
   :*output-channel*
   :screen
   :width
   :height
   :memory))
