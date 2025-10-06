;;; bib.lisp --- Bibliography Encoder/Decoder

;; 

;;; Code:
(in-package :dat/bib)

(defclass bibliography (ast)
  ()
  (:documentation "A Bibliography object containing a list of bibtex entries typically read
from or written to a BibTeX file."))

(deftype bibtex-entry-type () 
  `(member
    :article :book :booklet :conference
    :inbook :incollection :inproceedings :manual
    :masterthesis :misc :phdthesis :proceedings
    :techreport :unpublished))

(deftype bibtex-field-name ()
  `(member
    ;; standard
    :address :annote :author :booktitle
    :chapter :edition :editor :howpublished
    :institution :journal :month :note
    :number :organization :pages :publisher
    :school :series :title :type
    :volume :year
    ;; non-standard
    :doi :issn :isbn :url))

(defclass bibtex-entry (id ast) 
  ((type :initform :misc :initarg :type :type bibtex-entry-type))
  (:documentation "A Bibliographic reference entry containing a citation-key
  stored in the ID slot, the entry type in the TYPE slot and a alist of fields
  in the AST slot."))

(defun read-bibtex-field (stream)
  (let ((ret (cons nil nil)))
    (setf (car ret)
          (string-trim " "
                       (concatenate 'string
                                    (loop for c = (read-char stream nil)
                                          while (and c (not (char= c #\=)))
                                          unless (whitespace-p c)
                                          collect c))))
    (let* ((val (loop for c = (read-char stream nil)
                      while (and c (not (char= c #\newline)))
                      collect c))
           (p (char= (lastcar val) #\})))
      (setf (cdr ret) (string-trim " {}," (concatenate 'string val)))
      (values ret p))))

;; The citekey can be any combination of alphanumeric characters including the
;; characters "-", "_", and ":". The most frequent pattern is to use the last
;; name of the first author followed by the year.
(defun read-bibtex-entry (stream)
  (sb-int:listen-skip-whitespace stream)
  (when (char= #\@ (read-char stream nil nil))
    (let ((e (make-instance 'bibtex-entry)))
      (setf (slot-value e 'type) ; type
            (keywordicate
             (string-upcase
              (concatenate 'string
                           (loop for c = (read-char stream)
                                 while (and c (not (char= c #\{)))
                                 collect c))))
            (slot-value e 'id) ; id
            (concatenate 'string
                         (loop for c = (read-char stream)
                               while (and c (not (char= c #\,)))
                               collect c))
            (slot-value e 'ast) ; fields
            (loop for f = (multiple-value-list (read-bibtex-field stream))
                  until (cadr f)
                  collect (car f)))
      e)))

(defun read-bibtex-stream (stream)
  (make-instance 'bibliography
    :ast (loop while (peek-char t stream nil nil)
               for b = (read-bibtex-entry stream)
               until (not b)
               collect b)))

(defun write-bibtex-field (cons stream)
  (format stream "~&~A={~A}" (car cons) (cdr cons)))

(defun write-bibtex-fields (ast stream)
  (format stream "~{~A~^,~%~}" (mapcar (lambda (x) (write-bibtex-field x nil)) ast)))

(defun write-bibtex-entry (entry stream)
  (format stream "~&@~(~A~){~A,~%~A}" (slot-value entry 'type) (id entry) (write-bibtex-fields (ast entry) nil)))

(defun write-bibliography (bib stream)
  (mapc (lambda (x) (write-bibtex-entry x stream)) (ast bib)))

(defmethods serde 
  (((from bibliography) to)
   (write-bibliography from to))
  (((from bibliography) (to pathname))
   (with-open-file (f to :direction :output)
     (write-bibliography from f))))

(defmethod deserialize ((from stream) (fmt (eql :bib)) &key)
  (read-bibtex-stream from))

(defmethod deserialize ((from pathname) (fmt (eql :bib)) &key)
  (with-open-file (f from)
    (read-bibtex-stream f)))

(defmethod deserialize ((from string) (fmt (eql :bib)) &key)
  (with-input-from-string (s from)
    (read-bibtex-stream s)))
