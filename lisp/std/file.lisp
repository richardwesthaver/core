;;; std/file.lisp --- Standard File Library

;;

;;; Code:
(in-package :std)

(declaim (inline octet-vector=/unsafe))
(defun octet-vector=/unsafe (v1 v2 start1 end1 start2 end2)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (compilation-speed 0))
           (type octet-vector v1 v2)
           (type array-index start1 start2)
           (type array-length end1 end2))
  (and (= (- end1 start1)
          (- end2 start2))
       (loop for i from start1 below end1
             for j from start2 below end2
             always (eql (aref v1 i) (aref v2 j)))))

(defun octet-vector= (v1 v2 &key (start1 0) end1
                                 (start2 0) end2)
  "Like `string=' for octet vectors."
  (declare (octet-vector v1 v2)
           (array-index start1 start2)
           ((or array-length null) end1 end2)
           (optimize speed))
  (let* ((len1 (length v1))
         (len2 (length v2))
         (end1 (or end1 len1))
         (end2 (or end2 len2)))
    (assert (<= start1 end1 len1))
    (assert (<= start2 end2 len2))
    (octet-vector=/unsafe v1 v2 start1 end1 start2 end2)))

(defun file-size-in-octets (file)
  (multiple-value-bind (path namestring)
      (etypecase file
        (string (values (pathname file)
                        file))
        (pathname (values file
                          (sb-ext:native-namestring file))))
    (declare (ignorable path namestring))
    (sb-posix:stat-size (sb-posix:stat path))))

(define-constant si-prefixes
  '((-30 "quecto" "q")
    (-27 "ronto"  "r")
    (-24 "yocto"  "y")
    (-21 "zepto"  "z")
    (-18 "atto"   "a")
    (-15 "femto"  "f")
    (-12 "pico"   "p")
    ( -9 "nano"   "n")
    ( -6 "micro"  "μ")
    ( -3 "milli"  "m")
    ( -2 "centi"  "c")
    ( -1 "deci"   "d")
    (  0 ""       "" )
    (  1 "deca"   "da")
    (  2 "hecto"  "h")
    (  3 "kilo"   "k")
    (  6 "mega"   "M")
    (  9 "giga"   "G")
    ( 12 "tera"   "T")
    ( 15 "peta"   "P")
    ( 18 "exa"    "E")
    ( 21 "zetta"  "Z")
    ( 24 "yotta"  "Y")
    ( 27 "ronna"  "R")
    ( 30 "quetta" "Q"))
  :test #'equalp
  :documentation "List as SI prefixes: power of ten, long form, short form.")

(define-constant si-prefixes-base-1000
  (loop for (pow long short) in si-prefixes
        unless (and (not (zerop pow))
                    (< (abs pow) 3))
          collect (list (truncate pow 3) long short))
  :test #'equalp
  :documentation "The SI prefixes as powers of 1000, with centi, deci, deca and hecto omitted.")

(define-constant iec-prefixes
  '(( 0 ""     "")
    (10 "kibi" "Ki")
    (20 "mebi" "Mi")
    (30 "gibi" "Gi")
    (40 "tebi" "Ti")
    (50 "pebi" "Pi")
    (60 "exbi" "Ei"))
  :test #'equalp
  :documentation "The IEC binary prefixes, as powers of 2.")

(eval-always
  (defun single (seq)
    "Is SEQ a sequence of one element?"
    (= (length seq) 1)))

(defmacro si-prefix-rec (n base prefixes)
  (cond ((null prefixes) (error "No prefixes!"))
        ((single prefixes)
         (destructuring-bind ((power long short)) prefixes
           `(values ,long ,short ,(expt base power))))
        (t
         ;; good enough
         (let* ((halfway (ceiling (length prefixes) 2))
                (lo (subseq prefixes 0 halfway))
                (hi (subseq prefixes halfway))
                (split (* (expt base (caar hi)))))
             `(if (< ,n ,split)
                  (si-prefix-rec ,n ,base ,lo)
                  (si-prefix-rec ,n ,base ,hi))))))

(defun si-prefix (n &key (base 1000))
  "Given a number, return the prefix of the nearest SI unit.

Three values are returned: the long form, the short form, and the
multiplying factor.

    (si-prefix 1001) => \"kilo\", \"k\", 1000d0

BASE can be 1000, 10, 1024, or 2. 1000 is the default, and prefixes
start at kilo and milli. Base 10 is mostly the same, except the
prefixes centi, deci, deca and hecto are also used. Base 1024 uses the
same prefixes as 1000, but with 1024 as the base, as in vulgar file
sizes. Base 2 uses the IEC binary prefixes."
  (if (zerop n) (values "" "" 1d0)
      (let ((n (abs (coerce n 'double-float))))
        (ecase base
          (2 (si-prefix-rec n 2d0 #.iec-prefixes))
          (10 (si-prefix-rec n 10d0 #.si-prefixes))
          (1000 (si-prefix-rec n 1000d0 #.si-prefixes-base-1000))
          (1024 (si-prefix-rec n 1024d0 #.si-prefixes-base-1000))))))

(defun human-size-formatter (size &key (flavor :si)
                                       (space (eql flavor :si)))
  "Auxiliary function for formatting quantities human-readably.
Returns two values: a format control and a list of arguments.

This can be used to integrate the human-readable printing of
quantities into larger format control strings using the recursive
processing format directive (~?):

    (multiple-value-bind (control args)
        (human-size-formatter size)
      (format t \"~?\" control args))"
  (let ((size (coerce size 'double-float))
        ;; Avoid printing exponent markers.
        (*read-default-float-format* 'double-float)
        (base (ecase flavor
                (:file 1024)
                (:si   1000)
                (:iec  2))))
    (multiple-value-bind (long short factor)
        (si-prefix size :base base)
      (declare (ignore long))
      (let* ((size (/ size factor))
             (int (round size))
             (size
               (if (> (abs (- size int))
                      0.05d0)
                   size
                   int)))
        (values (formatter "~:[~d~;~,1f~]~:[~; ~]~a")
                (list (floatp size) size space short))))))

(defun format-human-size (stream size
                          &key (flavor :si)
                               (space (eql flavor :si)))
  "Write SIZE to STREAM, in human-readable form.

STREAM is interpreted as by `format'.

If FLAVOR is `:si' (the default) the base is 1000 and SI prefixes are used.

If FLAVOR is `:file', the base is 1024 and SI prefixes are used.

If FLAVOR is `:iec', the base is 1024 bytes and IEC prefixes (Ki, Mi,
etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si'.)"
  (if (zerop size)
      (format stream "0")
      (multiple-value-bind (formatter args)
          (human-size-formatter size :flavor flavor :space space)
        (format stream "~?" formatter args))))

(defun format-file-size-human-readable (stream file-size
                                        &key flavor
                                             (space (eql flavor :si))
                                             (suffix (if (eql flavor :iec) "B" "")))
  "Write FILE-SIZE, a file size in bytes, to STREAM, in human-readable form.

STREAM is interpreted as by `format'.

If FLAVOR is nil, kilobytes are 1024 bytes and SI prefixes are used.

If FLAVOR is `:si', kilobytes are 1000 bytes and SI prefixes are used.

If FLAVOR is `:iec', kilobytes are 1024 bytes and IEC prefixes (Ki,
Mi, etc.) are used.

If SPACE is non-nil, include a space between the number and the
prefix. (Defaults to T if FLAVOR is `:si'.)

SUFFIX is the suffix to use; defaults to B if FLAVOR is `:iec',
otherwise empty."
  (check-type file-size (integer 0 *))
  (if (zerop file-size)
      (format stream "0")
      (let ((flavor (if (null flavor) :file flavor)))
        (multiple-value-bind (formatter args)
            (human-size-formatter file-size :flavor flavor :space space)
          (format stream "~?~a" formatter args suffix)))))

(defun file-size-human-readable (file &key flavor space suffix stream)
  "Format the size of FILE (in octets) using `format-file-size-human-readable'.
The size of file is found by `trivial-file-size:file-size-in-octets'.

Inspired by the function of the same name in Emacs."
  (let ((file-size (file-size-in-octets file)))
    (format-file-size-human-readable
     stream
     file-size
     :flavor flavor
     :suffix suffix
     :space space)))

(defmacro with-open-files ((&rest args) &body body)
  "A simple macro to open one or more files providing the streams for the BODY. The ARGS is a list of `(stream filespec options*)` as supplied to WITH-OPEN-FILE."
  (case (length args)
    ((0)
     `(progn ,@body))
    ((1)
     `(with-open-file ,(first args) ,@body))
    (t `(with-open-file ,(first args)
          (with-open-files
              ,(rest args) ,@body)))))

(defmacro with-open-file* ((stream filespec &key direction element-type
                                   if-exists if-does-not-exist external-format)
                           &body body)
  "Just like WITH-OPEN-FILE, but NIL values in the keyword arguments
mean to use the default value specified for OPEN."
  (once-only (direction element-type if-exists if-does-not-exist external-format)
    `(with-open-stream
         (,stream (apply #'open ,filespec
                         (append
                          (when ,direction
                            (list :direction ,direction))
                          (list :element-type (or ,element-type
                                                  +default-element-type+))
                          (when ,if-exists
                            (list :if-exists ,if-exists))
                          (when ,if-does-not-exist
                            (list :if-does-not-exist ,if-does-not-exist))
                          (when ,external-format
                            (list :external-format ,external-format)))))
       ,@body)))

(defmacro with-input-from-file ((stream-name file-name &rest args
                                             &key (direction nil direction-p)
                                             &allow-other-keys)
                                &body body)
  "Evaluate BODY with STREAM-NAME to an input stream on the file
FILE-NAME. ARGS is sent as is to the call to OPEN except EXTERNAL-FORMAT,
which is only sent to WITH-OPEN-FILE when it's not NIL."
  (declare (ignore direction))
  (when direction-p
    (error "Can't specify :DIRECTION for WITH-INPUT-FROM-FILE."))
  `(with-open-file* (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name &rest args
                                            &key (direction nil direction-p)
                                            &allow-other-keys)
                               &body body)
  "Evaluate BODY with STREAM-NAME to an output stream on the file
FILE-NAME. ARGS is sent as is to the call to OPEN except EXTERNAL-FORMAT,
which is only sent to WITH-OPEN-FILE when it's not NIL."
  (declare (ignore direction))
  (when direction-p
    (error "Can't specify :DIRECTION for WITH-OUTPUT-TO-FILE."))
  `(with-open-file* (,stream-name ,file-name :direction :output ,@args)
     ,@body))

(defun write-stream-into-file (stream pathname &key (if-exists :error) if-does-not-exist)
  "Read STREAM and write the contents into PATHNAME.

STREAM will be closed afterwards, so wrap it with
`make-concatenated-stream' if you want it left open."
  (check-type pathname pathname)
  (with-open-stream (in stream)
    (with-output-to-file (out pathname
                              :element-type (stream-element-type in)
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream in out)))
  pathname)

(defun write-file-into-stream (pathname output &key (if-does-not-exist :error)
                                                    (external-format :default))
  "Write the contents of FILE into STREAM."
  (check-type pathname pathname)
  (with-input-from-file (input pathname
                               :element-type (stream-element-type output)
                               :if-does-not-exist if-does-not-exist
                               :external-format external-format)
    (copy-stream input output)))

(defun file= (file1 file2 &key (buffer-size 4096))
  "Compare FILE1 and FILE2 octet by octet, \(possibly) using buffers
of BUFFER-SIZE."
  (declare (ignorable buffer-size))
  (let ((file1 (truename file1))
        (file2 (truename file2)))
    (or (equal file1 file2)
        (and (= (file-size-in-octets file1)
                (file-size-in-octets file2))
             #+ccl (file=/mmap file1 file2)
             #-ccl (file=/loop file1 file2 :buffer-size buffer-size)))))

(defun file=/loop (file1 file2 &key (buffer-size 4096))
  "Compare two files by looping over their contents using a buffer."
  (declare
   (type pathname file1 file2)
   (type array-length buffer-size)
   (optimize (safety 1) (debug 0) (compilation-speed 0)))
  (flet ((make-buffer ()
           (make-array buffer-size
                       :element-type 'octet
                       :initial-element 0)))
    (declare (inline make-buffer))
    (with-open-files ((file1 file1 :element-type 'octet :direction :input)
                      (file2 file2 :element-type 'octet :direction :input))
      (and (= (file-length file1)
              (file-length file2))
           (locally (declare (optimize speed))
             (loop with buffer1 = (make-buffer)
                   with buffer2 = (make-buffer)
                   for end1 = (read-sequence buffer1 file1)
                   for end2 = (read-sequence buffer2 file2)
                   until (or (zerop end1) (zerop end2))
                   always (and (= end1 end2)
                               (octet-vector= buffer1 buffer2
                                              :end1 end1
                                              :end2 end2))))))))

(defun file-size (file &key (element-type '(unsigned-byte 8)))
  "The size of FILE, in units of ELEMENT-TYPE (defaults to bytes).

The size is computed by opening the file and getting the length of the
resulting stream.

If all you want is to read the file's size in octets from its
metadata, consider `trivial-file-size:file-size-in-octets' instead."
  (check-type file (or string pathname))
  (with-input-from-file (in file :element-type element-type)
    (file-length in)))

(defun file-timestamp ()
  "Returns current timestamp as a string suitable as the name of a timestamped-file."
  (multiple-value-bind (sec min hr day mon yr)
                       (get-decoded-time)
    (format nil "~4d~2,'0d~2,'0d_~2,'0d~2,'0d~2,'0d" yr mon day hr min sec)))

(defun file-date ()
  "Returns current date as a string suitable as the name of a timestamped-file."
  (multiple-value-bind (sec min hr day mon yr)
                       (get-decoded-time)
    (declare (ignore sec min hr))
    (format nil "~4d~2,'0d~2,'0d" yr mon day)))

;; see https://www.n16f.net/blog/counting-lines-with-common-lisp/

(defun directory-path-p (path)
  "Return T if PATH is a directory or NIL else."
  (declare (type (or pathname string) path))
  (and (not (pathname-name path))
       (not (pathname-type path))))

(defun hidden-path-p (path)
  "Return T if PATH is a hidden file or directory or NIL else."
  (declare (type pathname path))
  (let ((name (if (directory-path-p path)
                  (car (last (pathname-directory path)))
                  (file-namestring path))))
    (and (plusp (length name))
         (eq (char name 0) #\.))))

(defun directory-path (path)
  "If PATH is a directory pathname, return it as it is. If it is a file
pathname or a string, transform it into a directory pathname."
  (declare (type (or pathname string) path))
  (if (directory-path-p path)
      path
      (make-pathname :directory (append (or (pathname-directory path)
                                            (list :relative))
                                        (list (file-namestring path)))
                     :name nil :type nil :defaults path)))

(defun find-files (path)
  "Return a list of all files contained in the directory at PATH or any of its
subdirectories."
  (declare (type (or pathname string) path))
  (flet ((list-directory (path)
           (directory
            (make-pathname :defaults (directory-path path)
                           :type :wild :name :wild))))
    (let ((paths nil)
          (children (list-directory (directory-path path))))
      (dolist (child children paths)
        (unless (hidden-path-p child)
          (if (directory-path-p child)
              (setf paths (append paths (find-files child)))
              (push child paths)))))))

(defun count-file-lines (path)
  "Count the number of non-empty lines in the file at PATH. A line is empty if
it only contains space or tabulation characters."
  (declare (type pathname path))
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (do ((nb-lines 0)
         (blank-line t))
        (nil)
      (let ((octet (read-byte stream nil)))
        (cond
          ((or (null octet) (eq octet #.(char-code #\Newline)))
           (unless blank-line
             (incf nb-lines))
           (when (null octet)
             (return-from count-file-lines nb-lines))
           (setf blank-line t))
          ((and (/= octet #.(char-code #\Space))
                (/= octet #.(char-code #\Tab)))
           (setf blank-line nil)))))))
