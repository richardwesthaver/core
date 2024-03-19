(define-alien-callable lisp-version c-string ()
  (make-alien-string (lisp-implementation-version)))

(sb-ext:save-lisp-and-die "alien.core" :callable-exports '("lisp_version"))
                          
