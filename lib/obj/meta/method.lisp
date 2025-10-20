;;; method.lisp --- Meta-Methods (Combinations)

;; Method Combinations

;;; Code:
(in-package :obj/meta/method)
;;; From ARNESI - Messing with the MOP

;; https://bese.common-lisp.dev/docs/arnesi/html/Messing_0020with_0020the_0020MOP.html#wrapping-standard_0020method_0020combination

(define-method-combination wrapping-standard
    (&key (around-order :most-specific-first)
          (before-order :most-specific-first)
          (primary-order :most-specific-first)
          (after-order :most-specific-last)
          (wrapping-order :most-specific-last)
          (wrap-around-order :most-specific-last))
  ((wrap-around (:wrap-around))
   (around (:around))
   (before (:before))
   (wrapping (:wrapping))
   (primary () :required t)
   (after (:after)))
  "Same semantics as standard method combination but allows
\"wrapping\" methods. Ordering of methods:

 (wrap-around
   (around
     (before)
     (wrapping
       (primary))
     (after)))

:warp-around, :around, :wrapping and :primary methods call the
next least/most specific method via call-next-method (as in
standard method combination).

The various WHATEVER-order keyword arguments set the order in
which the methods are called and be set to either
:most-specific-last or :most-specific-first."
  (labels ((effective-order (methods order)
             (ecase order
               (:most-specific-first methods)
               (:most-specific-last (reverse methods))))
           (call-methods (methods)
             (mapcar (lambda (meth) `(call-method ,meth))
                     methods)))
    (let* (;; reorder the methods based on the -order arguments
           (wrap-around (effective-order wrap-around wrap-around-order))
           (around (effective-order around around-order))
           (wrapping (effective-order wrapping wrapping-order))
           (before (effective-order before before-order))
           (primary (effective-order primary primary-order))
           (after (effective-order after after-order))
           ;; inital value of the effective call is a call its primary
           ;; method(s)
           (form (case (length primary)
                   (1 `(call-method ,(first primary)))
                   (t `(call-method ,(first primary) ,(rest primary))))))
      (when wrapping
        ;; wrap form in call to the wrapping methods
        (setf form `(call-method ,(first wrapping)
                                 (,@(rest wrapping) (make-method ,form)))))
      (when before
        ;; wrap FORM in calls to its before methods
        (setf form `(progn
                      ,@(call-methods before)
                      ,form)))
      (when after
        ;; wrap FORM in calls to its after methods
        (setf form `(multiple-value-prog1
                        ,form
                      ,@(call-methods after))))
      (when around
        ;; wrap FORM in calls to its around methods
        (setf form `(call-method ,(first around)
                                 (,@(rest around)
                                    (make-method ,form)))))
      (when wrap-around
        (setf form `(call-method ,(first wrap-around)
                                 (,@(rest wrap-around)
                                    (make-method ,form)))))
      form)))
