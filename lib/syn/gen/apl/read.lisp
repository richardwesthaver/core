;;; read.lisp --- APL Readers

;; 

;;; Code:
(in-package :syn/gen/apl)

;; first char must be non-numeric
(defvar *apl-name-chars*
  (concatenate 'string
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
               "abcdefghijklmnopqrstuvwxyz"
               "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝß"
               "àáâãäåæçèéêëìíîïðñòóôõöøùúûüþ"
               "0123456789"
               "∆⍙"
               "ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ"))

(defvar *apl-glyphs* "←→$⍢:⍝/⌿\⍀¨⍤⍥⌸⌺⍨⍣.∘⍠@&⌶+-×÷⌊⌈|*⍟○!≠~?∊,⍪⌷⍳⍸⍴↑↓⊣⊢⌽⊖⍉⍋⍒⌹≡≢⊂⊆⊃∪⍎⍕∧∨⍲⍱<≤=≥>⍷⊤⊥∩")

(defconstant +apl-comment+ #\⍝)

(defun read-apl (stream &optional (eof-error-p t) eof-value)
  "Read an APL expression from STREAM."
  (let ((c (peek-char t stream eof-error-p eof-value)))
    (cond
      ((digit-char-p (read-apl-number stream)))
      ((char= #\. c) (read-apl-number-or-dot stream))
      ((or (char= #\' c) (char= #\" c)) (read-apl-string stream))
      ((char= #\[ c) (read-apl-vector stream)))))

;;; Readers
(defmethod gen-reader ((self (eql :apl))) (function apl-reader))
(defmethod gen-reader-switch ((self (eql :apl))) (function apl-reader-switch))

(defmethod load-gen ((self (eql :apl)))
  (init-gen :apl)
  (apl-reader))

(defmethod unload-gen ((self (eql :apl)))
  (init-gen nil)
  (cl-reader))

(defmethod gen-package ((self (eql :apl))) (find-package :syn/gen/apl/sym))

