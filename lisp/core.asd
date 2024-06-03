(pushnew :core *features*)
(defsystem :core
  :depends-on (:std :log :io :obj :net :cry :parse :syn :dat)
  :components ((:file "core"))
  :build-pathname "core")
