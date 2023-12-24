;;; lib/parse/tests.lisp --- Parser Tests

;;

;;; Code:
(defpackage :parse/tests
  (:use :cl :rt :std :parse))

(in-package :parse/tests)

(defsuite :parse)
(in-suite :parse)

(deftest lex ()
  (is (string=
       (with-lexer-environment ("<foo>")
         (when (char= #\< (consume))
           (consume-until (make-matcher (is #\>)))))
       "foo"))
  (is (string=
       (let ((q "baz"))
         (with-lexer-environment ("foo bar baz")
           (consume-until (make-matcher (is q)))))
       "foo bar ")))

(deftest lalr ()
  (defun parse (string)
    (lalr-parser (tokenizer string) #'error))

  (defun tokenizer (string  &key (start 0) end)
    (setq end (or end (length string)))
    (lambda ()
      (multiple-value-bind (more sem val) (chop-token string :start start :end end)
        (setq start more)
        (values sem val))))

  (defun chop-token (string &key (start 0) end &aux it)
    (setq end (or end (length string)))
    (cond ((>= start end)
           (values start :eof :eof))
          ((member (char string start) '(#\space #\newline))
           (chop-token string :start (+ start 1) :end end))
          ((setq it (find-if (lambda (tok)
                               (setq tok (string tok))
                               (and (> (- end start) (length tok))
                                    (string= string tok :start1 start :end1 (+ start (length tok)))))
                             '(+ - * / \( \) \, \;)))
           (values (+ start (length (string it))) it it))
          ((alpha-char-p (char string start))
           (let ((p2 (or (position-if-not (lambda (c)
                                            (or (alphanumericp c) (find c "-_$")))
                                          string :start (1+ start) :end end)
                         end)))
             (values p2 :identifier (intern (string-upcase (subseq string start p2))))))
          ((digit-char-p (char string start))
           (multiple-value-bind (val p2)
               (parse-integer string :start start :end end :junk-allowed t)
             (values p2 :literal val)))
          (t
           (error "Scanning error:~%~A~%~v<~>^" string start))))

  (defun parse-expr (string)
    (parse 
     (:next-input (tokenizer string))
     (:tokens + * - / :literal :identifier \( \))
     (:precedence
      (:left + -)
      (:left * /))
     ;;
     (start -> expr)
     (expr -> expr + expr => `(+ ,$1 ,$3)
           -> expr - expr => `(- ,$1 ,$3)
           -> expr * expr => `(* ,$1 ,$3)
           -> expr / expr => `(/ ,$1 ,$3))
     (expr -> :literal
           -> :identifier)
     (expr -> \( expr \) => $2)))
  (defun blah ()
    (make-parser
     (quote ((:tokens + * - / :literal :identifier \( \) )
             (:precedence (:left + -)
                          (:left * /))
             (start -> expr)
             (expr -> expr + expr => `(+ ,$1 ,$3)
                   -> expr - expr => `(- ,$1 ,$3)
                   -> expr * expr => `(* ,$1 ,$3)
                   -> expr / expr => `(/ ,$1 ,$3)
                   -> :literal
                   -> :identifier
                   -> \( expr \) => $2)))
     nil
     :eof))
  (is (blah)))

