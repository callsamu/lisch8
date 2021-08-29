(defsystem :lispychip-8
  :name "Lisch8"
  :description "Simple Chip-8 emulator in Common Lisp"
  :serial t
  :components
  (:module "src"
   :serial t
   :components (:file "core"))
  :depends-on (:rutils))

  
