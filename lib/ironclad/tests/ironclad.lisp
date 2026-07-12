;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; ironclad.lisp -- tests for non-cryptography functionality

(in-package :ironclad/tests)

(deftest quotationp.1 ()
  (crypto::quotationp '(quote foo))
  t)

(deftest quotationp.2 ()
  (crypto::quotationp '(quote foo bar))
  nil)

(deftest unquote.1 ()
  (crypto::unquote (quote foo))
  foo)

(deftest unquote.2 ()
  (crypto::unquote 2)
  2)

(deftest unquote.3 ()
  (crypto::unquote '#1=(list 'foo 'bar))
  #1#)
