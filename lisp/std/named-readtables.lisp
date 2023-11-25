;;; named-readtables.lisp --- named-readtables

;; The standard readtable is controlled by the Lisp implementation and
;; generally shouldn't be touched. There can be problems with
;; 'stacking' multiple read-macros as can be seen in this SO post:
;; https://stackoverflow.com/questions/73346051/how-can-i-modify-the-and-readtable-macros-in-lisp

;; Instead, if you really want to change standard readtable behavior,
;; it is better to define your own readtables and be aware of the
;; context in which they are enabled. For example, loading a system
;; definition before enabling the readtable may cause divergent
;; behavior (using standard) versus your source code (custom).

;;; Code:
(uiop:define-package :std/named-readtables
  (:use :cl)
  (:export
   #:defreadtable
   #:in-readtable
   #:make-readtable
   #:merge-readtables-into
   #:find-readtable
   #:ensure-readtable
   #:rename-readtable
   #:readtable-name
   #:register-readtable
   #:unregister-readtable
   #:copy-named-readtable
   #:list-all-named-readtables
   ;; Types
   #:named-readtable-designator
   ;; Conditions
   #:readtable-error
   #:reader-macro-conflict
   #:readtable-does-already-exist
   #:readtable-does-not-exist
   #:parse-body))

(in-package :std/named-readtables)
(pushnew :named-readtables *features*)

(defmacro without-package-lock ((&rest package-names) &body body)
  `(sb-ext:with-unlocked-packages (,@package-names) ,@body))

;;; Taken from SWANK (which is Public Domain.)

(defmacro destructure-case (value &body patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
        (operands (gensym "rand-"))
        (tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
            (,operator (car ,tmp))
            (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect
                   (if (eq pattern t)
                       `(t ,@body)
                       (destructuring-bind (op &rest rands) pattern
                         `(,op (destructuring-bind ,rands ,operands
                                 ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

;;; Taken from Alexandria (which is Public Domain, or BSD.)

(define-condition simple-style-warning (simple-warning style-warning)
  ())

(defun simple-style-warn (format-control &rest format-args)
  (warn 'simple-style-warning
	 :format-control format-control
	 :format-arguments format-args))

(define-condition simple-program-error (simple-error program-error)
  ())

(defun simple-program-error (message &rest args)
  (error 'simple-program-error
         :format-control message
         :format-arguments args))

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list
designated by LIST."
  (if (listp list)
      list
      (list list)))

(declaim (inline ensure-function))	; to propagate return type.
(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defun parse-ordinary-lambda-list (lambda-list)
  "Parses an ordinary lambda-list, returning as multiple values:

 1. Required parameters.
 2. Optional parameter specifications, normalized into form (NAME INIT SUPPLIEDP)
    where SUPPLIEDP is NIL if not present.
 3. Name of the rest parameter, or NIL.
 4. Keyword parameter specifications, normalized into form ((KEYWORD-NAME NAME) INIT SUPPLIEDP)
    where SUPPLIEDP is NIL if not present.
 5. Boolean indicating &ALLOW-OTHER-KEYS presence.
 6. &AUX parameter specifications, normalized into form (NAME INIT).

Signals a PROGRAM-ERROR is the lambda-list is malformed."
  (let ((state :required)
        (allow-other-keys nil)
        (auxp nil)
        (required nil)
        (optional nil)
        (rest nil)
        (keys nil)
        (aux nil))
    (labels ((simple-program-error (format-string &rest format-args)
               (error 'simple-program-error
                      :format-control format-string
                      :format-arguments format-args))
             (fail (elt)
               (simple-program-error "Misplaced ~S in ordinary lambda-list:~%  ~S"
                                     elt lambda-list))
             (check-variable (elt what)
               (unless (and (symbolp elt) (not (constantp elt)))
                 (simple-program-error "Invalid ~A ~S in ordinary lambda-list:~%  ~S"
                                       what elt lambda-list)))
             (check-spec (spec what)
               (destructuring-bind (init suppliedp) spec
                 (declare (ignore init))
                 (check-variable suppliedp what)))
             (make-keyword (name)
               "Interns the string designated by NAME in the KEYWORD package."
               (intern (string name) :keyword)))
      (dolist (elt lambda-list)
        (case elt
          (&optional
           (if (eq state :required)
               (setf state elt)
               (fail elt)))
          (&rest
           (if (member state '(:required &optional))
               (setf state elt)
               (progn
                 (break "state=~S" state)
                 (fail elt))))
          (&key
           (if (member state '(:required &optional :after-rest))
               (setf state elt)
               (fail elt)))
          (&allow-other-keys
           (if (eq state '&key)
               (setf allow-other-keys t
                     state elt)
               (fail elt)))
          (&aux
           (cond ((eq state '&rest)
                  (fail elt))
                 (auxp
                  (simple-program-error "Multiple ~S in ordinary lambda-list:~%  ~S"
                                        elt lambda-list))
                 (t
                  (setf auxp t
                        state elt))
                 ))
          (otherwise
           (when (member elt '#.(set-difference lambda-list-keywords
                                                '(&optional &rest &key &allow-other-keys &aux)))
             (simple-program-error
              "Bad lambda-list keyword ~S in ordinary lambda-list:~%  ~S"
              elt lambda-list))
           (case state
             (:required
              (check-variable elt "required parameter")
              (push elt required))
             (&optional
              (cond ((consp elt)
                     (destructuring-bind (name &rest tail) elt
                       (check-variable name "optional parameter")
                       (if (cdr tail)
                           (check-spec tail "optional-supplied-p parameter")
                           (setf elt (append elt '(nil))))))
                    (t
                     (check-variable elt "optional parameter")
                     (setf elt (cons elt '(nil nil)))))
              (push elt optional))
             (&rest
              (check-variable elt "rest parameter")
              (setf rest elt
                    state :after-rest))
             (&key
              (cond ((consp elt)
                     (destructuring-bind (var-or-kv &rest tail) elt
                       (cond ((consp var-or-kv)
                              (destructuring-bind (keyword var) var-or-kv
                                (unless (symbolp keyword)
                                  (simple-program-error "Invalid keyword name ~S in ordinary ~
                                                         lambda-list:~%  ~S"
                                                        keyword lambda-list))
                                (check-variable var "keyword parameter")))
                             (t
                              (check-variable var-or-kv "keyword parameter")
                              (setf var-or-kv (list (make-keyword var-or-kv) var-or-kv))))
                       (if (cdr tail)
                           (check-spec tail "keyword-supplied-p parameter")
                           (setf tail (append tail '(nil))))
                       (setf elt (cons var-or-kv tail))))
                    (t
                     (check-variable elt "keyword parameter")
                     (setf elt (list (list (make-keyword elt) elt) nil nil))))
              (push elt keys))
             (&aux
              (if (consp elt)
                  (destructuring-bind (var &optional init) elt
                    (declare (ignore init))
                    (check-variable var "&aux parameter"))
                  (check-variable elt "&aux parameter"))
              (push elt aux))
             (t
              (simple-program-error "Invalid ordinary lambda-list:~%  ~S" lambda-list)))))))
    (values (nreverse required) (nreverse optional) rest (nreverse keys)
            allow-other-keys (nreverse aux)))))

(defmacro define-api (name lambda-list type-list &body body)
  (flet ((parse-type-list (type-list)
           (let ((pos (position '=> type-list)))
             (assert pos () "You forgot to specify return type (`=>' missing.)")
             (values (subseq type-list 0 pos)
                     `(values ,@(nthcdr (1+ pos) type-list) &optional)))))
    (multiple-value-bind (body decls docstring)
        (parse-body body :documentation t :whole `(define-api ,name))
      (multiple-value-bind (arg-typespec value-typespec)
          (parse-type-list type-list)
        (multiple-value-bind (reqs opts rest keys)
            (parse-ordinary-lambda-list lambda-list)
          (declare (ignorable reqs opts rest keys))
          `(progn
             (declaim (ftype (function ,arg-typespec ,value-typespec) ,name))
             (locally
                 ;; Muffle the annoying "&OPTIONAL and &KEY found in
                 ;; the same lambda list" style-warning
                 #+sbcl (declare (sb-ext:muffle-conditions style-warning))
               (defun ,name ,lambda-list
                 ,docstring
                 ,@decls
                 (locally
                     #+sbcl (declare (sb-ext:unmuffle-conditions style-warning))
                     ;; SBCL will interpret the ftype declaration as
                     ;; assertion and will insert type checks for us.
                     ,@body)))))))))

(defmacro define-cruft (name lambda-list &body (docstring . alternatives))
  (assert (typep docstring 'string) (docstring) "Docstring missing!")
  (assert (not (null alternatives)))
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,docstring ,(first alternatives))))

(eval-when (:compile-toplevel :execute)
  #+sbcl (when (find-symbol "ASSERT-NOT-STANDARD-READTABLE"
                            (find-package "SB-IMPL"))
           (pushnew :sbcl+safe-standard-readtable *features*)))



;;;; Mapping between a readtable object and its readtable-name.

(defvar *readtable-names* (make-hash-table :test 'eq))

(define-cruft %associate-readtable-with-name (name readtable)
  "Associate READTABLE with NAME for READTABLE-NAME to work."
  #+ :common-lisp (setf (gethash readtable *readtable-names*) name))

(define-cruft %unassociate-readtable-from-name (name readtable)
  "Remove the association between READTABLE and NAME."
  #+ :common-lisp (progn (assert (eq name (gethash readtable *readtable-names*)))
                         (remhash readtable *readtable-names*)))

(define-cruft %readtable-name (readtable)
  "Return the name associated with READTABLE."
  #+ :common-lisp (values (gethash readtable *readtable-names*)))

(define-cruft %list-all-readtable-names ()
  "Return a list of all available readtable names."
  #+ :common-lisp (list* :standard :current :modern
                         (loop for name being each hash-value of *readtable-names*
                               collect name)))

;;;; Mapping READTABLE objects to docstrings.

(defvar *readtable-to-docstring* (make-hash-table :test 'eq))

(defun %associate-docstring-with-readtable (readtable docstring)
  (setf (gethash readtable *readtable-to-docstring*) docstring))

(defun %unassociate-docstring-from-readtable (readtable)
  (prog1 (gethash readtable *readtable-to-docstring*)
    (remhash readtable *readtable-to-docstring*)))

;;;; Specialized DOCUMENTATION for named readtables.

;;; Lispworks, at least, forbids defining methods on DOCUMENTATION.
;;; Wrapping these forms with WITHOUT-PACKAGE-LOCK (as for PRINT-OBJECT,
;;; see below) allows this to compile on Lispworks.

(without-package-lock (:common-lisp #+lispworks :implementation)

  (defmethod documentation ((name symbol) (doc-type (eql 'readtable)))
    (let ((readtable (find-readtable name)))
      (and readtable (gethash readtable *readtable-to-docstring*))))

  (defmethod documentation ((readtable readtable) (doc-type (eql 'readtable)))
    (gethash readtable *readtable-to-docstring*))

  (defmethod (setf documentation) (docstring (name symbol)
                                             (doc-type (eql 'readtable)))
    (let ((readtable (find-readtable name)))
      (unless readtable
        (error 'readtable-does-not-exist :readtable-name name))
      (setf (gethash readtable *readtable-to-docstring*) docstring)))

  (defmethod (setf documentation) (docstring (readtable readtable)
                                             (doc-type (eql 'readtable)))
    (setf (gethash readtable *readtable-to-docstring*) docstring)))


;;;; Mapping between a readtable-name and the actual readtable object.

;;; On Allegro we reuse their named-readtable support so we work
;;; nicely on their infrastructure.

(defvar *named-readtables* (make-hash-table :test 'eq))

(define-cruft %associate-name-with-readtable (name readtable)
  "Associate NAME with READTABLE for FIND-READTABLE to work."
  #+ :common-lisp (setf (gethash name *named-readtables*) readtable))

(define-cruft %unassociate-name-from-readtable (name readtable)
  "Remove the association between NAME and READTABLE"
  #+ :common-lisp (progn (assert (eq readtable (gethash name *named-readtables*)))
                         (remhash name *named-readtables*)))

(define-cruft %find-readtable (name)
  "Return the readtable named NAME."
  #+ :common-lisp (values (gethash name *named-readtables* nil)))


;;;; Reader-macro related predicates

;;; CLISP creates new function objects for standard reader macros on
;;; each readtable copy.
(define-cruft function= (fn1 fn2)
  "Are reader-macro function-designators FN1 and FN2 the same?"
  (let ((fn1 (ensure-function fn1))
        (fn2 (ensure-function fn2)))
    (or (eq fn1 fn2)
        ;; After SBCL 1.1.18, for dispatch macro characters
        ;; GET-MACRO-CHARACTER returns closures whose name is:
        ;;
        ;; (LAMBDA (STREAM CHAR) :IN SB-IMPL::%MAKE-DISPATCH-MACRO-CHAR)
        ;;
        ;; Treat all these closures equivalent.
        (flet ((internal-dispatch-macro-closure-name-p (name)
                 (find "SB-IMPL::%MAKE-DISPATCH-MACRO-CHAR" name
                       :key #'prin1-to-string :test #'string-equal)))
          (let ((n1 (sb-impl::%fun-name fn1))
                (n2 (sb-impl::%fun-name fn2)))
            (and (listp n1) (listp n2)
                 (internal-dispatch-macro-closure-name-p n1)
                 (internal-dispatch-macro-closure-name-p n2))))))
  #+ :common-lisp
  (eq (ensure-function fn1) (ensure-function fn2)))

(define-cruft dispatch-macro-char-p (char rt)
  "Is CHAR a dispatch macro character in RT?"
  #+ :common-lisp
  (handler-case (locally
                  (get-dispatch-macro-character char #\x rt)
                  t)
    (error () nil)))

;; (defun macro-char-p (char rt)
;;   (let ((reader-fn (%get-macro-character char rt)))
;;     (and reader-fn t)))

;; (defun standard-macro-char-p (char rt)
;;   (multiple-value-bind (rt-fn rt-flag) (get-macro-character char rt)
;;     (multiple-value-bind (std-fn std-flag) (get-macro-character char *standard-readtable*)
;;       (and (eq rt-fn std-fn)
;; 	   (eq rt-flag std-flag)))))

;; (defun standard-dispatch-macro-char-p (disp-char sub-char rt)
;;   (flet ((non-terminating-p (ch rt) (nth-value 1 (get-macro-character ch rt))))
;;     (and (eq (non-terminating-p disp-char rt)
;; 	     (non-terminating-p disp-char *standard-readtable*))
;; 	 (eq (get-dispatch-macro-character disp-char sub-char rt)
;; 	     (get-dispatch-macro-character disp-char sub-char *standard-readtable*)))))


;;;; Readtables Iterators

(defmacro with-readtable-iterator ((name readtable) &body body)
  (let ((it (gensym)))
    `(let ((,it (%make-readtable-iterator ,readtable)))
       (macrolet ((,name () `(funcall ,',it)))
         ,@body))))

(defun funcall-or (package-and-name-list &rest args)
  (loop for (package name) in package-and-name-list
        do (let ((symbol (find-symbol (string name) package)))
             (when symbol
               (return-from funcall-or (apply symbol args))))))

(defun %make-readtable-iterator (readtable)
  (let ((char-macro-array (funcall-or '((sb-impl base-char-macro-array)
                                        (sb-impl character-macro-array))
                                      readtable))
        (char-macro-ht (funcall-or '((sb-impl extended-char-table)
                                     (sb-impl character-macro-hash-table))
                                   readtable))
        (dispatch-tables (sb-impl::dispatch-tables readtable))
        (char-code 0))
    (with-hash-table-iterator (ht-iterator char-macro-ht)
      (labels ((grovel-base-chars ()
                 (if (>= char-code sb-int:base-char-code-limit)
                     (grovel-unicode-chars)
                     (let ((reader-fn (svref char-macro-array char-code))
                           (char (code-char (shiftf char-code (1+ char-code)))))
                       (if reader-fn
                           (yield char)
                           (grovel-base-chars)))))
               (grovel-unicode-chars ()
                 (multiple-value-bind (more? char) (ht-iterator)
                   (if (not more?)
                       (values nil nil nil nil nil)
                       (yield char))))
               (yield (char)
                 (let ((disp-fn (get-macro-character char readtable))
                       (disp-ht))
                   (cond
                     ((setq disp-ht (cdr (assoc char dispatch-tables)))
                      (let ((sub-char-alist))
                        (maphash (lambda (k v)
                                   (push (cons k v) sub-char-alist))
                                 disp-ht)
                        (values t char disp-fn t sub-char-alist)))
                     (t
                      (values t char disp-fn nil nil))))))
        #'grovel-base-chars))))

(defmacro do-readtable ((entry-designator readtable &optional result)
                        &body body)
  "Iterate through a readtable's macro characters, and dispatch macro characters."
  (destructuring-bind (char &optional reader-fn non-terminating-p disp? table)
      (if (symbolp entry-designator)
          (list entry-designator)
          entry-designator)
    (let ((iter (gensym "ITER+"))
          (more? (gensym "MORE?+"))
          (rt (gensym "READTABLE+")))
      `(let ((,rt ,readtable))
         (with-readtable-iterator (,iter ,rt)
           (loop
             (multiple-value-bind (,more?
                                   ,char
                                   ,@(when reader-fn (list reader-fn))
                                   ,@(when disp? (list disp?))
                                   ,@(when table (list table)))
                 (,iter)
               (unless ,more? (return ,result))
               (let ,(when non-terminating-p
                       ;; FIXME: N-T-P should be incorporated in iterators.
                       `((,non-terminating-p
                          (nth-value 1 (get-macro-character ,char ,rt)))))
                 ,@body))))))))

;;;; Misc

;;; This should return an implementation's actual standard readtable
;;; object only if the implementation makes the effort to guard against
;;; modification of that object. Otherwise it should better return a
;;; copy.
(define-cruft %standard-readtable ()
  "Return the standard readtable."
  #+ :sbcl+safe-standard-readtable sb-impl::*standard-readtable*
  #+ :common-lisp                  (copy-readtable nil))

;;; On SBCL, SET-SYNTAX-FROM-CHAR does not get rid of a
;;; readtable's dispatch table properly.
;;; Same goes for Allegro but that does not seem to provide a
;;; setter for their readtable's dispatch tables. Hence this ugly
;;; workaround.
(define-cruft %clear-readtable (readtable)
  "Make all macro characters in READTABLE be constituents."
  (prog1 readtable
    (do-readtable (char readtable)
      (set-syntax-from-char char #\A readtable))
    (setf (sb-impl::dispatch-tables readtable) nil))
  #+ :common-lisp
  (do-readtable (char readtable readtable)
    (set-syntax-from-char char #\A readtable)))

(define-cruft %get-dispatch-macro-character (char subchar rt)
  "Ensure ANSI behaviour for GET-DISPATCH-MACRO-CHARACTER."
  #+ :common-lisp (get-dispatch-macro-character char subchar rt))

(define-cruft %get-macro-character (char rt)
  "Ensure ANSI behaviour for GET-MACRO-CHARACTER."
  #+ :common-lisp (get-macro-character char rt))


;;;; Specialized PRINT-OBJECT for named readtables.

;;; As per #19 in CLHS 11.1.2.1.2 defining a method for PRINT-OBJECT
;;; that specializes on READTABLE is actually forbidden. It's quite
;;; likely to work (modulo package-locks) on most implementations,
;;; though.

(without-package-lock (:common-lisp)
  (defmethod print-object :around ((rt readtable) stream)
    (let ((name (readtable-name rt)))
      (if name
          (print-unreadable-object (rt stream :type nil :identity t)
            (format stream "~A ~S" :named-readtable name))
          (call-next-method)))))

;;;
;;;  ``This is enough of a foothold to implement a more elaborate
;;;    facility for using readtables in a localized way.''
;;;
;;;                               (X3J13 Cleanup Issue IN-SYNTAX)
;;;

;;;;;; DEFREADTABLE &c.
(defmacro defreadtable (name &body options)
  "Define a new named readtable, whose name is given by the symbol NAME.
  Or, if a readtable is already registered under that name, redefine
  that one.

  The readtable can be populated using the following OPTIONS:

  - If the first element of OPTIONS is a string then it is associated
    with the readtable as in `(SETF (DOCUMENTATION NAME 'READTABLE)
    DOCSTRING)`.

  - `(:MERGE READTABLE-DESIGNATORS+)`

      Merge the macro character definitions from the readtables
      designated into the new readtable being defined as per
      MERGE-READTABLES-INTO. The copied options are
      :DISPATCH-MACRO-CHAR, :MACRO-CHAR and :SYNTAX-FROM, but not
      READTABLE-CASE.

      If no :MERGE clause is given, an empty readtable is used. See
      MAKE-READTABLE.

  - `(:FUSE READTABLE-DESIGNATORS+)`

      Like :MERGE except:

      Error conditions of type READER-MACRO-CONFLICT that are signaled
      during the merge operation will be silently _continued_. It
      follows that reader macros in earlier entries will be
      overwritten by later ones. For backward compatibility, :FUZE is
      accepted as an alias of :FUSE.

  - `(:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION)`

      Define a new sub character `SUB-CHAR` for the dispatching macro
      character `MACRO-CHAR`, per SET-DISPATCH-MACRO-CHARACTER. You
      probably have to define `MACRO-CHAR` as a dispatching macro
      character by the following option first.

  - `(:MACRO-CHAR MACRO-CHAR FUNCTION [NON-TERMINATING-P])`

      Define a new macro character in the readtable, per
      SET-MACRO-CHARACTER. If [FUNCTION][argument] is the keyword
      :DISPATCH, `MACRO-CHAR` is made a dispatching macro character,
      per MAKE-DISPATCH-MACRO-CHARACTER.

  - `(:SYNTAX-FROM FROM-READTABLE-DESIGNATOR FROM-CHAR TO-CHAR)`

      Set the character syntax of TO-CHAR in the readtable being
      defined to the same syntax as FROM-CHAR as per
      SET-SYNTAX-FROM-CHAR.

  - `(:CASE CASE-MODE)`

      Defines the _case sensitivity mode_ of the resulting readtable.

  Any number of option clauses may appear. The options are grouped by
  their type, but in each group the order the options appeared
  textually is preserved. The following groups exist and are executed
  in the following order: :MERGE and :FUSE (one group), :CASE,
  :MACRO-CHAR and :DISPATCH-MACRO-CHAR (one group), finally
  :SYNTAX-FROM.

  Notes:

  The readtable is defined at load-time. If you want to have it
  available at compilation time -- say to use its reader-macros in the
  same file as its definition -- you have to wrap the DEFREADTABLE
  form in an explicit EVAL-WHEN.

  On redefinition, the target readtable is made empty first before
  it's refilled according to the clauses.

  NIL, :STANDARD, :COMMON-LISP, :MODERN, and :CURRENT are
  preregistered readtable names."
  (check-type name symbol)
  (when (reserved-readtable-name-p name)
    (error "~A is the designator for a predefined readtable. ~
            Not acceptable as a user-specified readtable name." name))
  (flet ((process-option (option var)
           (destructure-case option
             ((:merge &rest readtable-designators)
	      `(merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                     readtable-designators)))
             ((:fuse &rest readtable-designators)
	      `(handler-bind ((reader-macro-conflict #'continue))
                 (merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                       readtable-designators))))
             ;; alias for :FUSE
             ((:fuze &rest readtable-designators)
	      `(handler-bind ((reader-macro-conflict #'continue))
                 (merge-readtables-into ,var ,@(mapcar #'(lambda (x) `',x)
                                                       readtable-designators))))
             ((:dispatch-macro-char disp-char sub-char function)
              `(set-dispatch-macro-character ,disp-char ,sub-char
                                             ,function ,var))
             ((:macro-char char function &optional non-terminating-p)
	      (if (eq function :dispatch)
		  `(make-dispatch-macro-character ,char ,non-terminating-p ,var)
		  `(set-macro-character ,char ,function
                                        ,non-terminating-p ,var)))
	     ((:syntax-from from-rt-designator from-char to-char)
	      `(set-syntax-from-char ,to-char ,from-char
				     ,var (find-readtable ,from-rt-designator)))
	     ((:case mode)
	      `(setf (readtable-case ,var) ,mode))))
	 (remove-clauses (clauses options)
	   (setq clauses (if (listp clauses) clauses (list clauses)))
	   (remove-if-not #'(lambda (x) (member x clauses))
			  options :key #'first)))
    (let* ((docstring (when (stringp (first options))
                        (pop options)))
           (merge-clauses (remove-clauses '(:merge :fuze :fuse) options))
	   (case-clauses (remove-clauses :case  options))
	   (macro-clauses (remove-clauses '(:macro-char :dispatch-macro-char)
                                          options))
	   (syntax-clauses (remove-clauses :syntax-from options))
	   (other-clauses
             (set-difference options
                             (append merge-clauses case-clauses
                                     macro-clauses syntax-clauses))))
      (cond
	((not (null other-clauses))
	 (error "Bogus DEFREADTABLE clauses: ~/PPRINT-LINEAR/" other-clauses))
	(t
	 `(eval-when (:load-toplevel :execute)
            ;; The (FIND-READTABLE ...) is important for proper
            ;; redefinition semantics, as redefining has to modify the
            ;; already existing readtable object.
            (let ((readtable (find-readtable ',name)))
              (cond ((not readtable)
                     (setq readtable (make-readtable ',name)))
                    (t
                     (setq readtable (%clear-readtable readtable))
                     (simple-style-warn
                      "Overwriting already existing readtable ~S."
                      readtable)))
              (setf (documentation readtable 'readtable) ,docstring)
              ,@(loop for option in merge-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in case-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in macro-clauses
                      collect (process-option option 'readtable))
              ,@(loop for option in syntax-clauses
                      collect (process-option option 'readtable))
              readtable)))))))

(defmacro in-readtable (name)
  "Set *READTABLE* to the readtable referred to by the symbol NAME.
  Return the readtable."
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; NB. The :LOAD-TOPLEVEL is needed for cases like (DEFVAR *FOO*
     ;; (GET-MACRO-CHARACTER #\"))
     (setf *readtable* (ensure-readtable ',name))
     (when (find-package :swank)
       (%frob-swank-readtable-alist *package* *readtable*))
     *readtable*))

;;; KLUDGE: [interim solution]
;;;
;;;   We need support for this in Slime itself, because we want IN-READTABLE
;;;   to work on a per-file basis, and not on a per-package basis.
;;;
(defun %frob-swank-readtable-alist (package readtable)
  (let ((readtable-alist (find-symbol (string '#:*readtable-alist*)
				      (find-package :swank))))
    (when (boundp readtable-alist)
      (let ((new-item (cons (package-name package) readtable)))
        (setf (symbol-value readtable-alist)
              (cons
               new-item
               (remove new-item (symbol-value readtable-alist)
                       :test (lambda (entry1 entry2)
                               (string= (car entry1) (car entry2))))))))))

(deftype readtable-designator ()
  `(or null readtable))

(deftype named-readtable-designator ()
  "Either a symbol or a readtable itself."
  `(or readtable-designator symbol))

;;;;; Compiler macros

;;; Since the :STANDARD readtable is interned, and we can't enforce
;;; its immutability, we signal a style-warning for suspicious uses
;;; that may result in strange behaviour:

;;; Modifying the standard readtable would, obviously, lead to a
;;; propagation of this change to all places which use the :STANDARD
;;; readtable (and thus rendering this readtable to be non-standard,
;;; in fact.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-standard-readtable-expression-p (thing)
    (or (null thing)
        (eq thing :standard)
        (and (consp thing)
             (find thing
                   '((find-readtable nil)
                     (find-readtable :standard)
                     (ensure-readtable nil)
                     (ensure-readtable :standard))
                   :test #'equal))))

  (defun signal-suspicious-registration-warning (name-expr readtable-expr)
    (when (constant-standard-readtable-expression-p readtable-expr)
      (simple-style-warn
       "Caution: ~<You're trying to register the :STANDARD readtable ~
    under a new name ~S. As modification of the :STANDARD readtable ~
    is not permitted, subsequent modification of ~S won't be ~
    permitted either. You probably want to wrap COPY-READTABLE ~
    around~@:>~%             ~S"
       (list name-expr name-expr) readtable-expr))))

(define-compiler-macro register-readtable (&whole form name readtable)
  (signal-suspicious-registration-warning name readtable)
  form)

(define-compiler-macro ensure-readtable (&whole form name &optional
                                                (default nil default-p))
  (when default-p
    (signal-suspicious-registration-warning name default))
  form)

(declaim (special *standard-readtable* *empty-readtable*))

(define-api make-readtable
    (&optional (name nil name-supplied-p) &key merge)
    (&optional named-readtable-designator &key (:merge list) => readtable)
  "Creates and returns a new readtable under the specified
  NAME.

  MERGE takes a list of NAMED-READTABLE-DESIGNATORs and specifies the
  readtables the new readtable is created from. (See the :MERGE clause
  of DEFREADTABLE for details.)

  If MERGE is NIL, an empty readtable is used instead.

  If NAME is not given, an anonymous empty readtable is returned.

  Notes:

  An empty readtable is a readtable where each character's syntax is
  the same as in the _standard readtable_ except that each macro
  character has been made a constituent. Basically: whitespace stays
  whitespace, everything else is constituent."
  (cond ((not name-supplied-p)
         (copy-readtable *empty-readtable*))
        ((reserved-readtable-name-p name)
         (error "~A is the designator for a predefined readtable. ~
                Not acceptable as a user-specified readtable name." name))
        ((let ((rt (find-readtable name)))
           (and rt (prog1 nil
                     (cerror "Overwrite existing entry."
                             'readtable-does-already-exist :readtable-name name)
                     ;; Explicitly unregister to make sure that we do
                     ;; not hold on of any reference to RT.
                     (unregister-readtable rt)))))
        (t (let ((result (apply #'merge-readtables-into
                                ;; The first readtable specified in
                                ;; the :merge list is taken as the
                                ;; basis for all subsequent
                                ;; (destructive!) modifications (and
                                ;; hence it's copied.)
                                (copy-readtable (if merge
                                                    (ensure-readtable
                                                     (first merge))
                                                    *empty-readtable*))
                                (rest merge))))

             (register-readtable name result)))))

(define-api rename-readtable
    (old-name new-name)
    (named-readtable-designator symbol => readtable)
  "Replaces the associated name of the readtable designated by
  OLD-NAME with NEW-NAME. If a readtable is already registered under
  NEW-NAME, an error of type READTABLE-DOES-ALREADY-EXIST is
  signaled."
  (when (find-readtable new-name)
    (cerror "Overwrite existing entry."
            'readtable-does-already-exist :readtable-name new-name))
  (let* ((readtable (ensure-readtable old-name))
	 (readtable-name (readtable-name readtable)))
    ;; We use the internal functions directly to omit repeated
    ;; type-checking.
    (%unassociate-name-from-readtable readtable-name readtable)
    (%unassociate-readtable-from-name readtable-name readtable)
    (%associate-name-with-readtable new-name readtable)
    (%associate-readtable-with-name new-name readtable)
    (%associate-docstring-with-readtable
     readtable (%unassociate-docstring-from-readtable readtable))
    readtable))

(define-api merge-readtables-into
    (result-readtable &rest named-readtables)
    (named-readtable-designator &rest named-readtable-designator => readtable)
  "Copy macro character definitions of each readtable in
  NAMED-READTABLES into RESULT-READTABLE.

  If a macro character appears in more than one of the readtables,
  i.e. if a conflict is discovered during the merge, an error of type
  READER-MACRO-CONFLICT is signaled.

  The copied options are :DISPATCH-MACRO-CHAR, :MACRO-CHAR and
  :SYNTAX-FROM, but not READTABLE-CASE."
  (flet ((merge-into (to from)
	   (do-readtable ((char reader-fn non-terminating-p disp? table) from)
             (check-reader-macro-conflict from to char)
             (cond ((not disp?)
                    (set-macro-character char reader-fn non-terminating-p to))
                   (t
                    (ensure-dispatch-macro-character char non-terminating-p to)
                    (loop for (subchar . subfn) in table do
                      (check-reader-macro-conflict from to char subchar)
                      (set-dispatch-macro-character char subchar
                                                    subfn to)))))
	   to))
    (let ((result-table (ensure-readtable result-readtable)))
      (dolist (table (mapcar #'ensure-readtable named-readtables))
        (merge-into result-table table))
      result-table)))

(defun ensure-dispatch-macro-character (char &optional non-terminating-p
                                                       (readtable *readtable*))
  (if (dispatch-macro-char-p char readtable)
      t
      (make-dispatch-macro-character char non-terminating-p readtable)))

(define-api copy-named-readtable
    (named-readtable)
    (named-readtable-designator => readtable)
  "Like COPY-READTABLE but takes a NAMED-READTABLE-DESIGNATOR as argument."
  (copy-readtable (ensure-readtable named-readtable)))

(define-api list-all-named-readtables () (=> list)
  "Returns a list of all registered readtables. The returned list is
  guaranteed to be fresh, but may contain duplicates."
  (mapcar #'ensure-readtable (%list-all-readtable-names)))


(define-condition readtable-error (error) ())

(define-condition readtable-does-not-exist (readtable-error)
  ((readtable-name :initarg :readtable-name
	           :initform (required-argument)
	           :accessor missing-readtable-name
                   :type named-readtable-designator))
  (:report (lambda (condition stream)
             (format stream "A readtable named ~S does not exist."
                     (missing-readtable-name condition)))))

(define-condition readtable-does-already-exist (readtable-error)
  ((readtable-name :initarg :readtable-name
                   :initform (required-argument)
                   :accessor existing-readtable-name
                   :type named-readtable-designator))
  (:report (lambda (condition stream)
             (format stream "A readtable named ~S already exists."
                     (existing-readtable-name condition))))
  (:documentation "Continuable."))

(define-condition reader-macro-conflict (readtable-error)
  ((macro-char
    :initarg :macro-char
    :initform (required-argument)
    :accessor conflicting-macro-char
    :type character)
   (sub-char
    :initarg :sub-char
    :initform nil
    :accessor conflicting-dispatch-sub-char
    :type (or null character))
   (from-readtable
    :initarg :from-readtable
    :initform (required-argument)
    :accessor from-readtable
    :type readtable)
   (to-readtable
    :initarg :to-readtable
    :initform (required-argument)
    :accessor to-readtable
    :type readtable))
  (:report
   (lambda (condition stream)
     (format stream "~@<Reader macro conflict while trying to merge the ~
                    ~:[macro character~;dispatch macro characters~] ~
                    ~@C~@[ ~@C~] from ~A into ~A.~@:>"
             (conflicting-dispatch-sub-char condition)
             (conflicting-macro-char condition)
             (conflicting-dispatch-sub-char condition)
             (from-readtable condition)
             (to-readtable condition))))
  (:documentation "Continuable.

  This condition is signaled during the merge process if a reader
  macro (be it a macro character or the sub character of a dispatch
  macro character) is present in the both source and the target
  readtable and the two respective reader macro functions differ."))

(defun check-reader-macro-conflict (from to char &optional subchar)
  (flet ((conflictp (from-fn to-fn)
           (assert from-fn ()
                   "Bug in readtable iterators or concurrent access?")
           (and to-fn (not (function= to-fn from-fn)))))
    (when (if subchar
              (conflictp (%get-dispatch-macro-character char subchar from)
                         (%get-dispatch-macro-character char subchar to))
              (conflictp (%get-macro-character char from)
                         (%get-macro-character char to)))
      (cerror (format nil "Overwrite ~@C in ~A." char to)
              'reader-macro-conflict
              :from-readtable from
              :to-readtable to
              :macro-char char
              :sub-char subchar))))


;;; Although there is no way to get at the standard readtable in
;;; Common Lisp (cf. /standard readtable/, CLHS glossary), we make
;;; up the perception of its existence by interning a copy of it.
;;;
;;; We do this for reverse lookup (cf. READTABLE-NAME), i.e. for
;;;
;;;   (equal (readtable-name (find-readtable :standard)) "STANDARD")
;;;
;;; holding true.
;;;
;;; We, however, inherit the restriction that the :STANDARD
;;; readtable _must not be modified_ (cf. CLHS 2.1.1.2), although it'd
;;; technically be feasible (as *STANDARD-READTABLE* will contain a
;;; mutable copy of the implementation-internal standard readtable.)
;;; We cannot enforce this restriction without shadowing
;;; CL:SET-MACRO-CHARACTER and CL:SET-DISPATCH-MACRO-FUNCTION which
;;; is out of scope of this library, though. So we just threaten
;;; with nasal demons.
;;;
(defvar *standard-readtable*
  (%standard-readtable))

(defvar *empty-readtable*
  (%clear-readtable (copy-readtable nil)))

(defvar *case-preserving-standard-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :preserve)
    readtable))

(defparameter *reserved-readtable-names*
  '(nil :standard :common-lisp :modern :current))

(defun reserved-readtable-name-p (name)
  (and (member name *reserved-readtable-names*) t))

;;; In principle, we could DEFREADTABLE some of these. But we do
;;; reserved readtable lookup seperately, since we can't register a
;;; readtable for :CURRENT anyway.

(defun find-reserved-readtable (reserved-name)
  (cond ((eq reserved-name nil)          *standard-readtable*)
	((eq reserved-name :standard)    *standard-readtable*)
        ((eq reserved-name :common-lisp) *standard-readtable*)
        ((eq reserved-name :modern)      *case-preserving-standard-readtable*)
	((eq reserved-name :current)     *readtable*)
	(t (error "Bug: no such reserved readtable: ~S" reserved-name))))

(define-api find-readtable
    (name)
    (named-readtable-designator => (or readtable null))
  "Looks for the readtable specified by NAME and returns it if it is
  found. Returns NIL otherwise."
  (cond ((readtablep name) name)
        ((reserved-readtable-name-p name)
         (find-reserved-readtable name))
        ((%find-readtable name))))

;;; FIXME: This doesn't take a NAMED-READTABLE-DESIGNATOR, but only a
;;; STRING-DESIGNATOR. (When fixing, heed interplay with compiler
;;; macros below.)
(defsetf find-readtable register-readtable)

(define-api ensure-readtable
    (name &optional (default nil default-p))
    (named-readtable-designator &optional (or named-readtable-designator null)
      => readtable)
  "Looks up the readtable specified by NAME and returns it if it's found.
  If it is not found, it registers the readtable designated by DEFAULT
  under the name represented by NAME; or if no default argument is
  given, it signals an error of type READTABLE-DOES-NOT-EXIST
  instead."
  (cond ((find-readtable name))
        ((not default-p)
         (error 'readtable-does-not-exist :readtable-name name))
        (t (setf (find-readtable name) (ensure-readtable default)))))


(define-api register-readtable
    (name readtable)
    (symbol readtable => readtable)
  "Associate READTABLE with NAME. Returns the readtable."
  (assert (typep name '(not (satisfies reserved-readtable-name-p))))
  (%associate-readtable-with-name name readtable)
  (%associate-name-with-readtable name readtable)
  readtable)

(define-api unregister-readtable
    (named-readtable)
    (named-readtable-designator => boolean)
  "Remove the association of NAMED-READTABLE. Returns T if successfull,
  NIL otherwise."
  (let* ((readtable (find-readtable named-readtable))
	 (readtable-name (and readtable (readtable-name readtable))))
    (if (not readtable-name)
	nil
	(prog1 t
	  (check-type readtable-name
                      (not (satisfies reserved-readtable-name-p)))
          (%unassociate-readtable-from-name readtable-name readtable)
          (%unassociate-name-from-readtable readtable-name readtable)
          (%unassociate-docstring-from-readtable readtable)))))

(define-api readtable-name
    (named-readtable)
    (named-readtable-designator => symbol)
  "Returns the name of the readtable designated by NAMED-READTABLE,
  or NIL."
   (let ((readtable (ensure-readtable named-readtable)))
    (cond ((%readtable-name readtable))
          ((eq readtable *readtable*) :current)
	  ((eq readtable *standard-readtable*) :common-lisp)
          ((eq readtable *case-preserving-standard-readtable*) :modern)
	  (t nil))))

(provide :readtables)
