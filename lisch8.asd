(defsystem :lispychip-8
  :name "Lisch8"
  :description "Simple Chip-8 emulator in Common Lisp"
  :serial t
  :components
  (:module "src"
   :serial t
   :components
   (:module "core"
    :serial t
    :components ((:file "package")
		 (:file "utils")
		 (:file "screen")
		 (:file "instructions")
		 (:file "chip")
		 (:file "instruction-set"))))
  :depends-on (:rutils))

  
