(defsystem :lispychip-8
  :name "Lisp Chip8"
  :description "Chip8 emulator in Common Lisp"
  :serial t
  :components
  (:module "src"
   :serial t
   :components (:file "core"))
  :depends-on (:rutils))

  
