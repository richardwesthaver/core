(defpackage :nlp/tokenize
  (:use :cl :std :cl-ppcre :nlp/data :nlp/stem/porter)
  (:export :word-tokenize :sentence-tokenize))

(in-package :nlp/tokenize)

(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t) (alphabetic t))
  "Split a string into a list of words."
  (let* ((tokens (split " " (collapse-whitespaces string)))
         (tokens (if remove-stop-words
                     (delete-if (lambda (x) (gethash (string-downcase  x) (stop-words-lookup *language-data*))) tokens)
                     tokens))
         (tokens (if stem
                     (mapcar #'stem tokens)
                     tokens))
         (tokens (if down-case
                     (mapcar #'string-downcase tokens)
                     tokens))
         (tokens (if alphabetic
                     (delete-if-not (lambda (x) (cl-ppcre:scan "^[A-Za-z]*$" x)) tokens)
                     tokens)))
    tokens))

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  ;; TODO: Use "\\p{Terminal_Punctuation}" regexp instead to catch all terminal
  ;; punctuation marks, including "," and ";"?
  (remove "" (mapcar #'std:trim (cl-ppcre:split "[.!?]" string)) :test #'equal))
