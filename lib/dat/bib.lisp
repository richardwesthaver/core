;;; bib.lisp --- Bibliography Encoder/Decoder

;; 

;;; Code:
(in-package :dat/bib)

(defclass bibliography (ast)
  ()
  (:documentation "A Bibliography object containing a list of reference entries typically read
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

(defclass reference-entry (id ast) 
  ((type :initform :misc :initarg :type :type bibtex-entry-type))
  (:documentation "A Bibliographic reference entry containing a citation-key
  stored in the ID slot, the entry type in the TYPE slot and a plist of fields
  in the AST slot."))

;; The citekey can be any combination of alphanumeric characters including the
;; characters "-", "_", and ":". The most frequent pattern is to use the last
;; name of the first author followed by the year.
