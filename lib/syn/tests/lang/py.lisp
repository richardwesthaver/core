;;; py.lisp --- Python Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(in-suite :syn)
(in-readtable :std)
(defparameter *py-src* #"if __name__ == "__main__":
    from pip._internal.cli.main import main as _main

    sys.exit(_main())"#)

#+todo (deftest py-src () (istype 'sb-alien::alien-value (parse-lang-string :python *py-src*)))
