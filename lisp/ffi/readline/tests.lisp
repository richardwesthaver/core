;;; readline/tests.lisp --- readline tests

;;; Code:
(defpackage :readline/tests
  (:use :cl :std :rt :readline))

(in-package :readline/tests)

(defsuite :readline)
(in-suite :readline)

(load-readline)

(deftest sanity ()
  (is= 1 *rl-history-base*)
  (is= 0 *rl-history-length*)
  ;; (rl :prompt "hi: ")
  (isnt *rl-point*))

;;; CL-READLINE example
(defvar *verbs*  '("eat" "get" "throw" "quit"))
(defvar *fruits* '("banana" "apple" "orange" "banana_two"))

(defun prefix-1 (item1 item2)
  (subseq item1 0 (or (mismatch item1 item2) (length item1))))

(defun prefix (items)
  "Find the common prefix between strings.

   Uses the built-in `mismatch', that returns the position at which
   the strings fail to match.

   Example: `(str:prefix '(\"foobar\" \"foozz\"))` => \"foo\"

   - items: list of strings
   - Return: a string.

  "
  (when items
    (reduce #'prefix-1 items)))

(defun custom-complete (text start end)
  (declare (ignore end))
  (labels ((select-completions (list)
             (let ((els (remove-if-not (lambda (it)
                                         (starts-with-subseq text it))
                                       list)))
               (if (cdr els)
                   (cons (prefix els) els)
                   els))))
    (if (zerop start)
        (select-completions *verbs*)
        (select-completions *fruits*))))

;; (readline::register-function :complete #'custom-complete)

;;; Let's also create a custom command and bind it to some key
;;; sequence so the user can invoke it. In this example the user can
;;; automagically insert the phrase 'inserted text' pressing
;;; Control-o.

(defun print-some-text (arg key)
  (declare (ignore arg key))
  (readline::rl-insert-text "inserted text"))

;; (readline::bind-keyseq "\\C-o" #'print-some-text)

;;; Let's write novelty-check, so if the actual line is equal to the most
;;; recent history line it will not be added to the history.

(defun novelty-check (x y)
  (string/= (string-trim " " x)
            (string-trim " " y)))

;;; Finally, this is our main function. To exit from the loop, enter 'quit'.

(defun cl-readline-example ()
  ;; see cl-readline-example to see how to catch a C-c.
  (do ((i 0 (1+ i))
       (text ""))
      ((string= "quit" (string-trim " " text)))
    (setf text
          (readline::rl :prompt (format nil "[~a]> " i)
                        :add-history t
                        :novelty-check #'novelty-check))))

(deftest cl-readline-example (:skip t)
  (with-input-from-string (*standard-input* (format nil "quit~%"))
    (isnt (cl-readline-example))))
