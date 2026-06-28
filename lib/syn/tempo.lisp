;;; tempo.lisp --- Lisp Template Interpreter

;; Based on Embedded Ruby templates (ERB)

;; [[https://github.com/ruby/erb][erb]]
;; [[https://gitlab.common-lisp.net/mraskin/cl-emb][cl-emb]]

;;; Commentary:

;; we intend to extend this to support different template DSLs with expansions
;; processed in CL and used by Emacs.

;; Eventually we want to replace `eglot--snippet-expansion-fn' with a remote
;; call to the functions supplied here.

;;; Code:
(in-package :syn/tempo)

(defvar *tempo-case-sensitive* nil)

(defvar *tempo-package* *package*)

(defvar *tempo-stream-redirection* "with-output-to-string (*standard-output*)")

(defvar *tempo-table* (make-hash-table :test #'equal)
  "Table mapping names to tempo-function instances.")

(defvar *tempo-start* "<%"
  "Start of script or expr. Remember that a following #\=
indicates an expr.")

(defvar *tempo-end* "%>"
  "End of script or expr.")

(defvar *escape-type* :raw
  "Default value for escaping @var output.")

(defparameter *tempo-variables*
  '(("escape" . "tempo:*escape-type*")
    ("case-sensitive" . "tempo:*tempo-case-sensitive*")))

(defparameter *tempo-parameters*
  '(("xml" . ":xml")
    ("html" . ":html")
    ("url" . ":url")
    ("uri" . ":uri")
    ("url-encode" . ":url-encode")
    ("raw" . ":raw")
    ("latex" . ":latex")
    ("t" . "t")
    ("nil" . "nil")))

#+todo
(defun tempo-stream-redirect (key &optional args)
  "Make a new string based on KEY and ARGS which may be bound to
*TEMPO-STREAM-REDIRECTION*.")

;; REVIEW 2026-03-23: cleanup
(defun set-tempo-variables (match &rest registers)
  "Parse parameter(s) of @set and set special variables
like e.g. *ESCAPE-TYPE*."
  (declare (ignore match))
  (let ((setf-pairs
          (let ((setf-list nil))
            (dolist (pair (cl-ppcre:split "\\s+" (first registers))
                          (when (first setf-list)
                            (format nil "~{ ~A~}" (reverse setf-list))))
              (destructuring-bind (left right)
                  (cl-ppcre:split "=" pair)
                (let ((place (rest (assoc left *tempo-variables* :test #'equalp)))
                      (value (rest (assoc right *tempo-parameters* :test #'equalp))))
                  (when (and place value)
                    (push (concatenate 'string place " " value) setf-list))))))))
    (if setf-pairs
        (format nil "(setf ~A)" setf-pairs)
        "")))

;; Code from Edi Weitz's [[http://weitz.de/tbnl/][TBNL]]
(defun escape-by-table (string replacements)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (let ((new (find char replacements 
                                :test 'equal
                                :key 'car)))
                 (if new 
                     (write-string (cdr new) out)
                     (write-char char out))
                 )))))

(defvar *latex-replacements*
  (mapcar 
   (lambda (x) `(,(character (car x)) . ,(cdr x)))
   `(
     ("#" . "\\#")
     ("$" . "\\$")
     ("%" . "\\%")
     ("&" . "\\&")
     ("_" . "\\_")
     ("{" . "\\{")
     ("}" . "\\}")
     ("<" . "{$<$}")
     (">" . "{$>$}")
     ("\\" . "{$\\backslash{}$}")
     ("|" . "{$\\vert{}$}")
     ("~" . "{\\,$\\tilde{}$\\,}")
     ("^" . "{\\,$\\hat{}$\\,}")
     (,(string #\Return) . "~\\\\")
     (,(string #\NewLine) . "~\\\\")
     ("\"" . "{'{}'}")
     (,(string (code-char 173)) . "\\-")   ; Soft hyphen
     (,(string (code-char 160)) . "~")     ; No-break space
     (,(string (code-char 8209)) . "-")    ; Non-breaking hyphen
     (,(string (code-char 8211)) . "--")   ; En-dash
     (,(string (code-char 8212)) . "---")  ; Em-dash
     (,(string (code-char 8470)) . "{\\textnumero}")))) ; Number sign

(defun escape-for-latex (string)
  (escape-by-table string *latex-replacements*))

(defun echo (string &key (escape *escape-type*))
  "Emit given STRING. Escape if wanted (global or via ESCAPE keyword).
STRING can be NIL."
  (let ((str (cond 
               ((stringp string) string)
               ((null string) "")
               ((functionp string) 
                (format nil "~a" (or (autofuncall string) "")))
               (t (format nil "~a" string))
               )))
    (case escape 
      ((:html :xml)
       (dat/xml::escape-for-html str))
      ((:latex)
       (escape-for-latex str))
      ((:url :uri :url-encode)
       (url:url-encode str))
      (t ; incl. :raw
       str))))

(defkernel tempo-function (ast kernel-object)
  ((path :initarg :path
         :accessor path)
   (time :initarg :time
         :accessor tempo-function-time)))

(defun tempo-function (path time function &optional form)
  "Constructor for class TEMPO-FUNCTION."
  (let ((fn (make-instance 'tempo-function
              :path path
              :time time
              :ast form)))
    (setf (kernel fn) function)
    fn))

(defun make-tempo-function (code)
  "Builds and compiles the tempo-function out of tempo code."
  (let ((form 
          `,(let ((*package* *tempo-package*))
              (read-from-string
               (format nil 
                       "(lambda (&key env generator name) (declare (ignorable env generator))
(let ((topenv env)
      (template-path-default (if (typep name 'pathname) name *default-pathname-defaults*)))
 (declare (ignorable topenv template-path-default))
 (~a ~@[~A~])))"
                       *tempo-stream-redirection*
                       (let ((str (make-tempo-body-string (expand-template-tags code))))
                         (unless (= 0 (length str))
                           str)))))))
    (values (compile nil form)
            ;; when debug..
            form)))

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-expand-template-tag (tag)
    "Returns a CL-PPCRE scanner which matches a template tag expanded by EXPAND-TEMPLATE-TAGS.
Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash tag scanner-hash)
        (setf (gethash tag scanner-hash)
              (ppcre:create-scanner tag))))
  (defun clear-expand-template-tag-hash ()
    "Removes all scanners for template tags from cache."
    (clrhash scanner-hash)))

(defparameter *template-tag-expand*
  `(("\\s*@if\\s+(\\S+)\\s*"      . " (cond ((tempo::autofuncall (tempo::getf-tempo \"\\1\")) ")
    ("\\s*@ifnotempty\\s+(\\S+)\\s*"      . " (cond ((let* ((value (tempo::autofuncall (tempo::getf-tempo \"\\1\")))) (or (numberp value) (> (length value) 0))) ")
    ("\\s*@ifequal\\s+(\\S+)\\s+(\\S+)\\s*"      . "  (cond ((equal (format nil \"~a\" (tempo::autofuncall (tempo::getf-tempo \"\\1\"))) (format nil \"~a\" (tempo::autofuncall (tempo::getf-tempo \"\\2\")))) ")
    ("\\s*@else\\s*"              . " ) (t ")
    ("\\s*@endif\\s*"             . " )) ")
    ("\\s*@unless\\s+(\\S+)\\s*"  . " (cond ((not (tempo::autofuncall (tempo::getf-tempo \"\\1\"))) ")
    ("\\s*@endunless\\s*"         . " )) ")
    ("=?\\s*@var\\s+(\\S+)\\s+-(\\S+)\\s+(\\S+)\\s*"
     . "= (tempo::echo (tempo::getf-tempo \"\\1\") :\\2 :\\3) ")
    ("=?\\s*@var\\s+(\\S+)\\s*"   . "= (tempo::echo (tempo::getf-tempo \"\\1\")) ")
    ("\\s*@repeat\\s+(\\d+)\\s*"  . " (dotimes (i \\1) ")
    ("\\s*@repeat\\s+(\\S+)\\s*"  . " (dotimes (i (or (tempo::autofuncall (tempo::getf-tempo \"\\1\")) 0)) ")
    ("\\s*@endrepeat\\s*"         . " ) ")
    ("\\s*@loop\\s+(\\S+)\\s*"    . " (dolist (env (tempo::autofuncall (tempo::getf-tempo \"\\1\"))) ")
    ("\\s*@endloop\\s*"           . " ) ")
    ("\\s*@genloop\\s+(\\S+)\\s*" . " (let ((env) 
                                            (%gen (funcall generator :\\1 
                                                           (tempo::getf-tempo \"\\1\"))))
                                           (loop
                                            (when (funcall %gen :test) (return))
                                            (setq env (funcall %gen :next))
                                            (progn ")
    ("\\s*@endgenloop\\s*"        . " ))) ")
    ("\\s*@with\\s+(\\S+)\\s*"    . " (let ((env (tempo::autofuncall (tempo::getf-tempo \"\\1\")))) ")
    ("\\s*@endwith\\s*"           . " ) ")
    ("\\s*@include\\s+(\\S+)\\s*" . "= (let ((tempo:*escape-type* tempo:*escape-type*))
                                            (tempo:execute-template (merge-pathnames \"\\1\" template-path-default) :env env :generator generator)) ")
    ("\\s*@includevar\\s+(\\S+)\\s*" . "= (let* ((tempo:*escape-type* tempo:*escape-type*) 
                                                  (parameter (tempo::autofuncall (tempo::getf-tempo \"\\1\"))))
                                                (unless parameter (error \"use of @includevar on undefined parameter ~s\" \"\\1\"))
                                                (tempo:execute-template (merge-pathnames parameter template-path-default) :env env :generator generator)) ")
    ("\\s*@call\\s+(\\S+)\\s*"    . "= (let ((tempo:*escape-type* tempo:*escape-type*))
                                            (tempo:execute-template \"\\1\" :env env :generator generator)) ")
    ("\\s*@insert\\s+(\\S+)\\s*"  . "= (std:read-file (merge-pathnames (tempo::autofuncall (tempo::getf-tempo \"\\1\")) template-path-default)) ")
    #+nil ("\\s*@set\\s+(.*?)\\s*"      . ,(function set-specials))
    ("#.*"                        . "")
    )
  "List of conses. FIRST is regex, REST replacement (STRING or FUNCTION).
Functions get called with two parameters: match and list of registers.")

(defun expand-template-tags (string)
  "Expand template-tags (@if, @else, ...) to Common Lisp.
Replacement and regex in *TEMPLATE-TAG-EXPAND*"
  (labels ((expand-tags (string &optional (expands *template-tag-expand*))
             (let ((regex (scanner-for-expand-template-tag
                           (concatenate 'string "(?is)"
                                        "^" (first (first expands)) "$")))
                   (replacement (rest (first expands))))
               (if (null (rest expands))
                   (ppcre:regex-replace-all regex string replacement :simple-calls t)
                   (expand-tags
                    (ppcre:regex-replace-all regex string replacement :simple-calls t)
                    (rest expands))))))
    (ppcre:regex-replace-all (format nil "(?is)(~A\\-?)(.+?)(\\-?~A)"
                                     (ppcre:quote-meta-chars *tempo-start*)
                                     (ppcre:quote-meta-chars *tempo-end*))
                             string
                             (lambda (match start-tag string end-tag)
                               (declare (ignore match))
                               (if (ppcre:scan "(?is)^#.+#$" string)
                                   ""
                                   (concatenate 'string
                                                start-tag
                                                (expand-tags string)
                                                end-tag)))
                             :simple-calls t)))

(defun string-right-trim-spaces-until-newline (string)
  (remove #\Newline (string-right-trim '(#\Space #\Tab) string)
          :from-end t
          :count 1))

;; Finds the next script or expr tag in TEMPO source.  Returns
;; nil if none are found, otherwise returns 3 values:
;;  1. The position of the first character of the start tag.
;;  2. The position of the contents of the tag.
;;  3. The type of tag (:script or :expr).
;;  4. Whether trim whitespaces before the start tag.
(defun next-code (string start)
  (let ((start-tag (search *tempo-start* string :start2 start)))
    (if (not start-tag)
        nil
        (let ((start-code (+ start-tag (length *tempo-start*))))
          (case (and (> (length string) start-code)
                     (char string start-code))
            (#\= (values start-tag (1+ start-code) :expr nil))
            (#\- (values start-tag (1+ start-code) :script t))
            (t (values start-tag start-code :script nil)))))))

;; Given a tag type (:script or :expr), returns a format
;; string to be used to generate source code from the contents of the
;; tag.
(defun tag-template (tag-type)
  (ecase tag-type
    ((:script) "~A")
    ((:expr) "(format t \"~~A\" ~A)")))

;; (i) Converts text outside <% ... %> tags into calls
;; to WRITE-STRING, (ii) Text inside <% ... %>
;; ("scripts") is straight lisp code, (iii) Text inside <%= ... %>
;; ("exprs") becomes the argument to (FORMAT t "~A" ...)
;; The markers <% and %> can be overridden by setting
;; *tempo-start* and *tempo-end*
(defun make-tempo-body-string (code &optional (start 0))
  "Takes a string containing an tempo code and returns a string
containing the lisp code that implements that tempo code."
  (unless (= (length code) 0)
    (multiple-value-bind (start-tag start-code tag-type trim-start-whitespaces)
        (next-code code start)
      (if (not start-tag)
          (format nil "~@[(write-string ~S)~]" (unless (= (length code) start) (subseq code start)))
          (let* ((end-code (search *tempo-end* code :start2 start-code))
                 (trim-end-whitespaces (char= (char code (1- end-code)) #\-))
                 (str (if trim-start-whitespaces
                          (string-right-trim-spaces-until-newline (subseq code start start-tag))
                          (subseq code start start-tag))))
            ;; (unless end-code (error "EOF reached in TEMPO inside open '~A' tag." *tempo-start*))
            (format nil "~@[(write-string ~S)~] ~A ~A"
                    (unless (= (length str) 0) str)
                    (format nil (tag-template tag-type)
                            (subseq code start-code (if trim-end-whitespaces
                                                        (1- end-code)
                                                        end-code)))
                    (make-tempo-body-string
                     code
                     (if trim-end-whitespaces
                         (let ((next-pos (cl-ppcre:scan "(?:\\S|\\n)" code :start (+ end-code (length *tempo-end*)))))
                           (cond
                             ((null next-pos) (length code))
                             ((char= (elt code next-pos) #\Newline)
                              (1+ next-pos))
                             (t next-pos)))
                         (+ end-code (length *tempo-end*))))))))))

(defgeneric getf* (thing key &optional default)
  (:documentation "Returns a value by a key"))

(defmethod getf* ((plist list) key &optional default)
  "Uses getf to get a value from a plist"
  (if *tempo-case-sensitive*
      (getf plist key default)
      (loop for (k v) on plist by #'cddr
            when (string-equal k key)
            do (return v)
            finally (return default))))

(defmethod getf* ((table hash-table) key &optional default)
  "Uses gethash to get a value from a hash-table"
  (gethash key table default))

(defmethod getf* ((object standard-object) key &optional default)
  "Uses slot-value to get a value from a standard object, where the slot name is derived from key"
  (let ((slot-name (intern (princ-to-string key)
                           (symbol-package (class-name (class-of object))))))
    (if (and (slot-exists-p object slot-name)
             (slot-boundp object slot-name))
        (slot-value object slot-name)
        default)))

(defmacro getf-tempo (key)
  "Search either plist TOPENV or ENV according to the search path in KEY. KEY
is a string."
  (let ((plist (if (char= (char key 0) #\/)
                   (find-symbol "TOPENV" tempo:*tempo-package*)
                   (find-symbol "ENV" tempo:*tempo-package*)))
        (path-parts (ppcre:split "/" key :sharedp t)))
    (labels ((dig-plist (plist keys)
               (if (null keys)
                   plist
                   (dig-plist
                    (if (zerop (length (first keys)))
                        plist
                        `(getf* ,plist ,(keywordicate (first keys))))
                    (rest keys)))))
      (dig-plist plist path-parts))))

(defgeneric execute-template (name &key env generator)
  (:documentation "Execute named tempo code. Returns a string. Keyword parameter ENV
to pass objects to the code. ENV must be a plist."))

(defgeneric register-template (name code)
  (:documentation "Register given tempo CODE as NAME."))

(defmethod execute-template ((name t) &key env generator)
  (funcall (get-tempo-function name) :env env :generator generator :name name))

(defmethod execute-template ((name tempo-function) &key env generator)
  (funcall name :env env :generator generator))

(defmethod execute-template ((name pathname) &key env generator)
  (let ((fun (or (get-tempo-function name)
                 (kernel (register-template name name)))))
    (funcall fun :env env :generator generator :name name)))

(defmethod register-template (name (code pathname))
  (multiple-value-bind (function form)
      (make-tempo-function (read-file code))
    (setf (gethash name *tempo-table*)
          (tempo-function code
                          (file-write-date code)
                          function
                          form))))

(defmethod register-templates (path)
  (with-readtable :tempo
    (doplist (k v) (read-lisp-file path)
      (let ((tmp (compile-and-eval v)))
        (setf (path tmp) path
              (ast tmp) v)
        (setf (gethash k *tempo-table*)
              tmp)))))

(defmethod register-template (name (code string))
  (multiple-value-bind (function form)
      (make-tempo-function code)
    (setf (gethash name *tempo-table*)
          (tempo-function nil
                          (get-universal-time)
                          function
                          form))))

(defmethod register-template (name (code function))
  (setf (gethash name *tempo-table*) (tempo-function nil (get-universal-time) code)))

(defun get-tempo-function (name)
  "Returns the named function implementing a registered tempo code.
Rebuilds it when text template was a file which has been modified."
  (let* ((tempo-function (gethash name *tempo-table*))
         (path (when tempo-function (path tempo-function))))
    (cond ((and (not (typep name 'pathname)) (null tempo-function))
           (error "Tempo function ~S not found." name))
          ((null tempo-function)
           (return-from get-tempo-function))
          ((and path (> (file-write-date path) (tempo-function-time tempo-function)))
           ;; Update when file is newer
           (multiple-value-bind (function form)
               (make-tempo-function (read-file path))
             (setf (tempo-function-time tempo-function) (file-write-date path)
                   (kernel tempo-function) function
                   (ast tempo-function) form))))
    (kernel tempo-function)))

(defmacro deftempo (name &rest body)
  `(register-template ',name ,@body))

;;; Init
(defmethod init ((self (eql :tempo)) &key (package *tempo-package*) (case-sensitive *tempo-case-sensitive*))
  (setf *tempo-package* package
        *tempo-case-sensitive* case-sensitive))

(defmethod clean ((self (eql :tempo)) &key)
  (clrhash *tempo-table*))

(defmethod reset ((self (eql :tempo)) &key)
  (setf *tempo-table* (make-hash-table :test #'equal)
        *tempo-package* (find-package :syn/tempo)
        *tempo-case-sensitive* nil))

;;; Readtable
(defun tempo-reader (stream subchar numarg)
  (declare (ignore subchar numarg))
  `(tempo-function (or *compile-file-truename* *load-truename*)
                   (get-universal-time)
                   (make-tempo-function 
                    ,(concatenate 
                      'string
                      (collecting
                        (loop for c = (read-char stream)
                              if (char= c #\%) 
                              do (let ((c1 (read-char stream)))
                                   (if (char= c1 #\#) 
                                       (loop-finish)
                                       (progn (collect c) (collect c1))))
                              else do (collect c)))))))

;; NOTE: returns a tempo function
;;  #% <% @if t %> (print :OK) <% @else %> (print :NOPE) <% @endif %> %#

(defreadtable :tempo
  (:merge :std)
  (:dispatch-macro-char #\# #\% #'tempo-reader))

(defun list-all-templates () (hash-table-list *tempo-table*))
