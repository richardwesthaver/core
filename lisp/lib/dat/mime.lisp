;;; mime.lisp --- MIME Database

;; 

;;; Code:
(in-package :dat/mime)

(defun read-mime-match-offset (offset)
  "Mime offsets are encoded as single int or range N:N. Returns an integer of a
cons of two ints."
  (let ((len (length offset)))
    (if (= 1 len)
        (parse-integer offset)
        (multiple-value-bind (int1 pos) (parse-integer offset :junk-allowed t)
          (if (>= pos len)
              int1
              (cons int1 (parse-integer offset :start (1+ pos))))))))

(defstruct mime-magic offset value type)

(defstruct mime-type type name superclasses glob magic)

(declaim (inline mime-type))
(defun mime-type (mime-type)
  (mime-type-type mime-type))

(defun load-mime-info (&optional (path #p"/usr/share/mime/packages/freedesktop.org.xml"))
  (let ((types (xmlrep-find-child-tags "mime-type"
                                       (xml-parse (with-open-file (file path)
                                                    (with-output-to-string (st)
                                                      (loop for l = (read-line file nil)
                                                            while l
                                                            do (std:println l st)))))))
        (mime-types))
    ;; assumes all children have a single attribute - TYPE
    (dolist (mime types mime-types)
      (let ((type (xmlrep-attrib-value "type" mime)))
        (push (make-mime-type :type type
                              :name (car (split-sequence #\/ type :count 1 :from-end t))
                              :superclasses
                              (mapcar (lambda (x) (xmlrep-attrib-value "type" x))
                                      (xmlrep-find-child-tags "sub-class-of" mime))
                              :glob
                              (mapcar (lambda (x) (xmlrep-attrib-value "pattern" x))
                                      (xmlrep-find-child-tags "glob" mime))
                              :magic
                              (loop for magic in (xmlrep-find-child-tags "magic" mime)
                                    while magic
                                    collect (loop for match in (xmlrep-find-child-tags "match" magic)
                                                  collect (make-mime-magic
                                                           :offset (read-mime-match-offset
                                                                    (xmlrep-attrib-value "offset" match))
                                                           :value (xmlrep-attrib-value "value" match)
                                                           :type (xmlrep-attrib-value "type" match)))))
            mime-types)))))

(defvar *mime-types* (load-mime-info))

(defvar *mime-database*
  (let ((tbl (make-hash-table :size (length *mime-types*) :test 'equal)))
    (dolist (mime *mime-types* tbl)
      (setf (gethash (mime-type mime) tbl) mime))))

(defvar *mime-db*
  (let ((tbl (make-hash-table :test 'equal))) ;; at least as large as *MIME-DATABASE*
    (dolist (mime *mime-types* tbl)
      (when-let ((patterns (mime-type-glob mime)))
        (dolist (p patterns)
          (when (wild-pathname-p p) ;; drop '.*'
            (setf p (subseq p 2)))
          (setf (gethash p tbl) (mime-type mime)))))))

(defun get-mime (value)
  "Return the name of a MIME-TYPE from *MIME-DB*. The resulting value is a string
which can be passed to MIME* to get the actual object from *MIME-DATABASE*."
  (gethash value *mime-db*))

(defun get-mime* (value)
  "Return a MIME-TYPE from *MIME-DATABASE*."
  (gethash value *mime-database*))

;; from TRIVIAL-MIMES
(defun mime-probe (pathname)
  "Attempts to get the mime-type through a call to the FILE shell utility.
If the file does not exist or the platform is not unix, NIL is returned."
  #+unix
  (when (probe-file pathname)
    (let ((output (uiop:run-program (list "file" #+darwin "-bI" #-darwin "-bi"
                                                 (uiop:native-namestring pathname))
                                    :output :string)))
      (with-output-to-string (mime)
        (loop for c across output
              for char = (char-downcase c)
              ;; Allowed characters as per RFC6383
              while (find char "abcdefghijklmnopqrstuvwxyz0123456789!#$&-^_.+/")
              do (write-char char mime)))))
  #-unix
  NIL)

(defun mime-lookup (path)
  (get-mime (pathname-type path)))

(defun mime (path &optional (default "application/octet-stream"))
  (or (mime-lookup path)
      (mime-probe path)
      default))

;; TODO 2024-06-11: from TRIVIAL-MIMES
(defun mime-equal (m1 m2)
  (or (equal "*" m1)
      (equal "*" m2)
      (equal "*/*" m1)
      (equal "*/*" m2)
      (destructuring-bind (type1 subtype1 &rest parameters1)
          (uiop:split-string m1 :separator '(#\/ #\;))
        (declare (ignorable parameters1))
        (destructuring-bind (type2 subtype2 &rest parameters2)
            (uiop:split-string m2 :separator '(#\/ #\;))
          (declare (ignorable parameters2))
          (cond
            ((or (equal "*" subtype1)
                 (equal "*" subtype2)
                 (equal "" subtype1)
                 (equal "" subtype2))
             (string-equal type1 type2))
            ((string-equal type1 type2)
             (string-equal subtype1 subtype2))
            (t nil))))))

(defmacro mime-case (file &body cases)
  "A case-like macro that works with MIME type of FILE.

Otherwise clause is the last clause that starts with T or OTHERWISE,.

Example:
\(mime-case #p\"~/CHANGES.txt\"
  ((\"application/json\" \"application/*\") \"Something opaque...\")
  (\"text/plain\" \"That's a plaintext file :D\")
  (t \"I don't know this type!\"))"
  (let ((mime (gensym "mime")))
    `(let ((,mime (mime ,file)))
       (cond
         ,@(loop for ((mimes . body) . rest) on cases
                 when (member mimes '(T OTHERWISE))
                   collect `(t ,@body) into clauses
                   and do (if rest
                              (warn "Clauses after T and OTHERWISE are not reachable.")
                              (return clauses))
                 collect `((member ,mime (list ,@(uiop:ensure-list mimes)) :test #'mime-equal)
                           ,@body)
                   into clauses
                 finally (return clauses))))))
