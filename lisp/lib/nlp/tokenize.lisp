(defpackage :nlp/tokenize
  (:use :cl :std :std/str :cl-ppcre :nlp/data)
  (:export :word-tokenize :sentence-tokenize))

(in-package :nlp/tokenize)

(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t) (alphabetic t))
  "Split a string into a list of words."
  (let* ((alpha-scanner (cl-ppcre:create-scanner "^[A-Za-z]*$"))
         (tokens (split " " (collapse-whitespaces string)))
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
                     (delete-if-not (lambda (x) (cl-ppcre:scan alpha-scanner x)) tokens)
                     tokens)))
    tokens))

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  ;; TODO: Use "\\p{Terminal_Punctuation}" regexp instead to catch all terminal
  ;; punctuation marks, including "," and ";"?
  (remove "" (mapcar #'std/str:trim (cl-ppcre:split "[.!?]" string)) :test #'equal))
