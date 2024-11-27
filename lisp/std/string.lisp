;;; std/str.lisp --- String utilities

;;; Code:

;; (defvar sb-unicode-syms 
;;   '(words lines sentences whitespace-p uppercase lowercase titlecase
;;     word-break-class line-break-class sentence-break-class char-block
;;     cased-p uppercase-p lowercase-p titlecase-p casefold
;;     graphemes grapheme-break-class
;;     bidi-mirroring-glyph bidi-class
;;     normalize-string normalized-p default-ignorable-p
;;     confusable-p hex-digit-p mirrored-p alphabetic-p math-p
;;     decimal-value digit-value
;;     unicode< unicode> unicode= unicode-equal
;;     unicode<= unicode>=))
(in-package :std/string)

;; (mapc (lambda (s) (export s)) sb-unicode-syms)
;; (reexport-from 
;;  :sb-unicode
;;  :include sb-unicode-syms)
(defparameter *omit-nulls* nil)
(defvar *whitespaces* (list #\Backspace #\Tab #\Linefeed #\Newline #\Vt #\Page
                            #\Return #\Space #\Rubout
                            #+sbcl #\Next-Line #-sbcl (code-char 133)
                            #\No-break_space)
  "On some implementations, linefeed and newline represent the same character (code).")

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defmacro string-byte-length (s)
  "Return the number of bytes of the internal representation
of a string."
  `(etypecase ,s 
     (base-string (length ,s))
     (string (* (length ,s) 4))))

(defun ssplit (separator s &key (omit-nulls *omit-nulls*))
  "Split s into substring by separator (cl-ppcre takes a regex, we do not).

   `limit' limits the number of elements returned (i.e. the string is
   split at most `limit' - 1 times)."
  ;; cl-ppcre:split doesn't return a null string if the separator appears at the end of s.
  (let* ((res (cl-ppcre:split separator s)))
    (if omit-nulls
        (remove-if (lambda (it) (sequence:emptyp it)) res)
        res)))

(defun collapse-whitespaces (s)
  "Ensure there is only one space character between words.
  Remove newlines."
  (cl-ppcre:regex-replace-all "\\s+" s " "))

(defun trim (s &key (char-bag *whitespaces*))
  "Removes all characters in `char-bag` (default: whitespaces) at the beginning and end of `s`.
   If supplied, char-bag has to be a sequence (e.g. string or list of characters).

   Examples: (trim \"  foo \") => \"foo\"
             (trim \"+-*foo-bar*-+\" :char-bag \"+-*\") => \"foo-bar\"
             (trim \"afood\" :char-bag (str:concat \"a\" \"d\")) => \"foo\""
  (when s
    (string-trim char-bag s)))

;;;  TODO 2023-08-27: camel snake kebab

(defun make-template-parser (start-delimiter end-delimiter &key (ignore-case nil))
  "Returns a closure than can substitute variables
  delimited by \"start-delimiter\" and \"end-delimiter\"
  in a string, by the provided values."
  (check-type start-delimiter string)
  (check-type end-delimiter string)
  (when (or (string= start-delimiter "")
            (string= end-delimiter ""))
      (error 'simple-type-error
              :format-control "The empty string is not a valid delimiter."))
  (let ((start-len (length start-delimiter))
        (end-len (length end-delimiter))
        (test (if ignore-case
                  #'string-equal
                  #'string=)))

    (lambda (string values)
      (check-type string string)
      (unless (listp values)
        (error 'simple-type-error
               :format-control "values should be an association list"))

      (with-output-to-string (stream)
        (loop for prev = 0 then (+ j end-len)
              for i = (search start-delimiter string)
                      then (search start-delimiter string :start2 j)
              for j = (if i (search end-delimiter string :start2 i))
                      then (if i (search end-delimiter string :start2 i))
              while (and i j)
          do (write-string (subseq string prev i) stream)
             (let ((instance (rest (assoc (subseq string (+ i start-len) j)
                                          values
                                          :test test))))
               (if instance
                (princ instance stream)
                (write-string (subseq string i (+ j end-len)) stream)))

          finally (write-string (subseq string prev) stream))))))

;;; STRING-CASE
;;; Implementing an efficient string= case in Common Lisp
;;;
;;; 2015-11-15: Defknown don't have explicit-check in SBCL 1.3.0
;;;  Remove the declaration.  It's never useful the way we use
;;;  numeric-char=.
;;;
;;; 2015-11-15: Make this a real ASDF system for Xach
;;;  I copied the system definition from Quicklisp and mangled as
;;;  necessary.
;;;
;;; 2010-06-30: Tiny bugfix
;;;  Widen the type declarations inside cases to allow vectors that
;;;  have a length that's shorter than the total size (due to fill-
;;;  pointers).

;;;
;;;# Introduction
;;;
;;; In `<http://neverfriday.com/blog/?p=10>', OMouse asks how
;;; best to implement a `string= case' (in Scheme). I noted that
;;; naively iterating through the cases with `string=' at runtime
;;; is suboptimal. Seeing the problem as a simplistic pattern
;;; matching one makes an efficient solution obvious.
;;; Note that, unlike Haskell, both Scheme and CL have random-
;;; access on strings in O(1), something which I exploit to
;;; generate better code.
;;;
;;; This is also a pbook.el file (the pdf can be found at
;;; `<http://www.discontinuity.info/~pkhuong/string-case.pdf>' ).
;;; I'm new at this not-quite-illiterate programming thing, so
;;; please bear with me (: I'm also looking for comments on the
;;; formatting. I'm particularly iffy with the way keywords look
;;; like. It just looks really fuzzy when you're not really zoomed
;;; in (or reading it on paper).

;;; I usually don't use packages for throw-away code, but this looks
;;; like it could be useful to someone.

;;;# Some utility code

(defun split-tree (list &key (test 'eql) (key 'identity))
  "Splits input list into sublists of elements
   whose elements are all such that (key element)
   are all test.
   It's assumed that test and key form an equality class.
   (This is similar to groupBy)"
  (when list
    (let* ((lists ())
           (cur-list (list (first list)))
           (cur-key  (funcall key (first list))))
      (dolist (elt (rest list) (nreverse (cons (nreverse cur-list)
                                               lists)))
        (let ((new-key (funcall key elt)))
          (if (funcall test cur-key new-key)
              (push elt cur-list)
              (progn
                (push (nreverse cur-list) lists)
                (setf cur-list (list elt)
                      cur-key  new-key))))))))

(defun iota (n)
  (loop for i below n collect i))

(defun hash-table->list (table &key (keep-keys t) (keep-values t))
  "Saves the keys and/or values in table to a list.
   As with hash table iterating functions, there is no
   implicit ordering."
  (let ((list ()))
    (maphash (cond ((and keep-keys
                         keep-values)
                    (lambda (k v)
                      (push (cons k v) list)))
                   (keep-keys
                    (lambda (k v)
                      (declare (ignore v))
                      (push k list)))
                   (keep-values
                    (lambda (k v)
                      (declare (ignore k))
                      (push v list))))
             table)
    list))

(defun all-equal (list &key (key 'identity) (test 'eql))
  (if (or (null list)
          (null (rest list)))
      t
      (let ((first-key (funcall key (first list))))
        (every (lambda (element)
                 (funcall test first-key
                          (funcall key element)))
               (rest list)))))

(defun split-at (list n)
  "Split list in k lists of n elements (or less for the last list)"
  (declare (type (and fixnum (integer (0))) n))
  (let ((lists    '())
        (cur-list '())
        (counter  0))
    (declare (type (and fixnum unsigned-byte) counter))
    (dolist (elt list (nreverse (if cur-list
                                    (cons (nreverse cur-list)
                                          lists)
                                    lists)))
      (push elt cur-list)
      (when (= (incf counter) n)
        (push (nreverse cur-list) lists)
        (setf cur-list '()
              counter   0)))))

;;;# The string matching compiler per se
;;;
;;; I use special variables here because I find that
;;; preferable to introducing noise everywhere to thread
;;; these values through all the calls, especially
;;; when `*no-match-form*' is only used at the very end.

(defparameter *input-string* nil
  "Symbol of the variable holding the input string")

(defparameter *no-match-form* nil
  "Form to insert when no match is found.")

;;; The basic idea of the pattern matching process here is
;;; to first discriminate with the input string's length;
;;; once that is done, it is very easy to safely use random
;;; access until only one candidate string (pattern) remains.
;;; However, even if we determine that only one case might be
;;; a candidate, it might still be possible for another string
;;; (not in the set of cases) to match the criteria. So we also
;;; have to make sure that *all* the indices match. A simple
;;; way to do this would be to emit the remaining checks at the
;;; every end, when only one candidate is left. However, that
;;; can result in a lot of duplicate code, and some useless
;;; work on mismatches. Instead, the code generator always
;;; tries to find (new) indices for which all the candidates
;;; left in the branch share the same character, and then emits
;;; a guard, checking the character at that index as soon as possible.

;;; In my experience, there are two main problems when writing
;;; pattern matchers: how to decide what to test for at each
;;; fork, and how to ensure the code won't explode exponentially.
;;; Luckily, for our rather restricted pattern language (equality
;;; on strings), patterns can't overlap, and it's possible to guarantee
;;; that no candidate will ever be possible in both branches of a
;;; fork.

;;; Due to the the latter guarantee, we have a simple fitness
;;; measure for tests: simply maximising the number of
;;; candidates in the smallest branch will make our search tree
;;; as balanced as possible. Of course, we don't know whether
;;; the subtrees will be balanced too, but I don't think it'll
;;; be much of an issue.

;;; Note that, if we had access, whether via annotations or profiling,
;;; to the probability of each case, the situation would be very
;;; different. In fact, on a pipelined machine where branch
;;; mispredictions are expensive, an unbalanced tree will yield
;;; better expected runtimes. There was a very interesting and rather
;;; sophisticated Google lecture on that topic on Google video
;;; (the speaker used markov chains to model dynamic predictors,
;;; for example), but I can't seem to find the URL.

;;; TODO: Find bounds on the size of the code!

(defun find-best-split (strings to-check)
  "Iterate over all the indices left to check to find
   which index (and which character) to test for equality
   with, keeping the ones which result in the most balanced
   split."
  (flet ((evaluate-split (i char)
           "Just count all the matches and mismatches"
           (let ((=  0)
                 (/= 0))
             (dolist (string strings (min = /=))
               (if (eql (aref string i) char)
                   (incf =)
                   (incf /=)))))
         (uniquify-chars (chars)
           "Only keep one copy of each char in the list"
           (mapcar 'first (split-tree (sort chars 'char<) :test #'eql))))
    (let ((best-split 0)            ; maximise size of smallest branch
          (best-posn  nil)
          (best-char  nil))
      (dolist (i to-check (values best-posn best-char))
        (dolist (char (uniquify-chars (mapcar (lambda (string)
                                                (aref string i))
                                              strings)))
          (let ((Z (evaluate-split i char)))
            (when (> Z best-split)
              (setf best-split Z
                    best-posn  i
                    best-char  char))))))))

;;; We sometimes have to execute sequences of checks for
;;; equality. The natural way to express this is via a
;;; sequence of checks, wrapped in an `and'. However, that
;;; translates to a sequence of conditional branches, predicated
;;; on very short computations. On (not so) modern architectures,
;;; it'll be faster to coalesce a sequence of such checks together
;;; as straightline code (e.g. via `or' of `xor'), and only branch
;;; at the very end. The code doesn't become much more complex,
;;; and benchmarks have shown it to be beneficial (giving a speed
;;; up of 2-5% for both predictable and unpredictable workloads,
;;; on a Core 2).

;;; Benchmarks (and experience) have shown that, instead of executing
;;; a cascade of comparison/conditional branch, it's slightly
;;; faster, both for predictable and unpredictable workloads,
;;; to `or' together a bunch of comparisons (e.g. `xor'). On a Core 2
;;; processor, it seems that doing so for sequences of around 4
;;; comparisons is the sweetspot. On perfectly predictable input,
;;; aborting early (on the first check) saves as much time as
;;; the 4 test/conditional branch add, compared to a sequence of
;;; `xor' and `or'. 

;;; Numeric char= abstracts out the xor check, and, on SBCL,
;;; is replaced by a short assembly sequence when the first
;;; argument is a constant. The declared return type is then
;;; wider than strictly necessary making it fit in a machine
;;; register, but not as a fixnum ensures that the compiler
;;; won't repeatedly convert the values to fixnums, when all
;;; we'll do is `or' them together and check for zero-ness.
;;; This function is the only place where the macro isn't
;;; generic over the elements stored in the cases. It shouldn't
;;; be too hard to implement a numeric-eql, which would
;;; restore genericity to the macro, while keeping the 
;;; speed-up.

;; (progn
;;   (defknown numeric-char= (character character)
;;       (unsigned-byte #. (1- sb-vm:n-machine-word-bits))
;;       (movable foldable flushable))

;;   (define-vop (numeric-char=)
;;     (:args (x :scs (sb-vm::character-reg sb-vm::character-stack)
;;               :target r
;;               :load-if (not (location= x r))))
;;     (:info y)
;;     (:arg-types (:constant character) character)
;;     (:results (r :scs (sb-vm::unsigned-reg)
;;                  :load-if (not (location= x r))))
;;     (:result-types sb-vm::unsigned-num)
;;     (:translate numeric-char=)
;;     (:policy :fast-safe)
;;     (:note "inline constant numeric-char=")
;;     (:generator 1
;;       (move r x)
;;       (sb-vm::inst #:xor r (char-code y)))))

#+ (and sbcl (or x86 x86-64))
(defun numeric-char= (x y)
  (declare (type character x y))
  (logxor (char-code x)
          (char-code y)))

;;; At each step, we may be able to find positions for which
;;; there can only be one character. If we emit the check for
;;; these positions as soon as possible, we avoid duplicating
;;; potentially a lot of code. Since benchmarks have shown
;;; it to be useful, this function implements the checks
;;; as a series of (zerop (logior (numeric-char= ...)...)),
;;; if there is more than one such check to emit.

(defun emit-common-checks (strings to-check)
  (labels ((emit-char= (pairs)
             (mapcar (lambda (pair)
                       (destructuring-bind (posn . char)
                           pair
                         `(numeric-char= ,char
                                         (aref ,*input-string* ,posn))))
                     pairs))
           (emit-checking-form (common-chars)
             (when common-chars
               (let ((common-chars (sort common-chars '< :key 'car)))
                 #+ (and) `(and ,@(mapcar
                                   (lambda (chunk)
                                     (if (null (rest chunk))
                                         (destructuring-bind ((posn . char))
                                             chunk
                                           `(eql ,char
                                                 (aref ,*input-string* ,posn)))
                                         `(zerop
                                           (logior ,@(emit-char= chunk)))))
                                   (split-at common-chars 4)))
                 #+ (or) `(and ,@(mapcar
                                  (lambda (pair)
                                    (destructuring-bind (posn . char)
                                        pair
                                      `(eql ,char
                                            (aref ,*input-string* ,posn))))
                                  common-chars))))))
    (let ((common-chars  ())
          (left-to-check ()))
      (dolist (posn to-check (values (emit-checking-form common-chars)
                                     (nreverse           left-to-check)))
        (if (all-equal strings :key (lambda (string)
                                      (aref string posn)))
            (push (cons posn (aref (first strings) posn))
                  common-chars)
            (push posn left-to-check))))))

;;; The driving function: First, emit any test that is
;;; common to all the candidates. If there's only one
;;; candidate, then we just have to execute the body;
;;; if not, we look for the `best' test and emit the
;;; corresponding code: execute the test, and recurse
;;; on the candidates that match the test and on those
;;; that don't.

(defun make-search-tree (strings bodies to-check)
  (multiple-value-bind (guard to-check)
      (emit-common-checks strings to-check)
    (if (null (rest strings))
        (progn
          (assert (null to-check)) ; there shouldn't be anything left to check
          (if guard
              `(if ,guard
                   (progn ,@(first bodies))
                   ,*no-match-form*)
              `(progn ,@(first bodies))))
        (multiple-value-bind (posn char)
            (find-best-split strings to-check)
          (assert posn) ; this can only happen if all strings are equal
          (let ((=strings  ())
                (=bodies   ())
                (/=strings ())
                (/=bodies  ()))
            (loop
               for string in strings
               for body   in bodies
               do (if (eql char (aref string posn))
                      (progn
                        (push string =strings)
                        (push body   =bodies))
                      (progn
                        (push string /=strings)
                        (push body   /=bodies))))
            (let ((tree `(if (eql ,char (aref ,*input-string* ,posn))
                             ,(make-search-tree  =strings   =bodies
                                                 (remove posn to-check))
                             ,(make-search-tree /=strings /=bodies
                                                to-check))))
              (if guard
                  `(if ,guard
                       ,tree
                       ,*no-match-form*)
                  tree)))))))

;;; Finally, we can glue it all together.
;;; To recapitulate, first, dispatch on string
;;; length, then execute a search tree for the
;;; few candidates left, and finally make sure
;;; the input string actually matches the one 
;;; candidate left at the leaf.

(defun emit-string-case (cases input-var no-match)
  (flet ((case-string-length (x)
           (length (first x))))
    (let ((*input-string*  input-var)
          (*no-match-form* no-match)
          (cases-lists     (split-tree (sort cases '<
                                        :key #'case-string-length)
                                  :key #'case-string-length)))
      `(locally (declare (type vector ,input-var))
         (case (length ,input-var)
           ,@(loop for cases in cases-lists
                   for length = (case-string-length (first cases))
                   collect `((,length)
                             ;; arrays with fill pointers expose the total length
                             ;; in their type, not the position of the fill-pointer.
                             ;; The type below only applies to simple-arrays.
                             (locally (declare (type (or (not simple-array)
                                                         (simple-array * (,length)))
                                                     ,input-var))
                               ,(make-search-tree (mapcar 'first cases)
                                                  (mapcar 'rest  cases)
                                                  (iota length)))))
           (t ,no-match))))))

;;; Just wrapping the previous function in a macro,
;;; and adding some error checking (the rest of the code
;;; just assumes there won't be duplicate patterns).
;;; Note how we use a local function instead of passing
;;; the default form directly. This can save a lot on
;;; code size, especially when the default form is
;;; large.

(defmacro string-case ((string &key (default '(error "No match")))
                       &body cases)
  "(string-case (string &key default)
     case*)
   case ::= string form*
          | t      form*
   Where t is the default case."
  (let ((cases-table (make-hash-table :test 'equal)))
    "Error checking cruft"
    (dolist (case cases)
      (assert (typep case '(cons (or string (eql t)))))
      (let ((other-case (gethash (first case) cases-table)))
        (if other-case
            (warn "Duplicate string-case cases: ~A -> ~A or ~A~%"
                  (first case)
                  (rest other-case)
                  (rest case))
            (setf (gethash (first case) cases-table)
                  (rest case)))))
    (let ((input-var    (gensym "INPUT"))
          (default-fn   (gensym "ON-ERROR"))
          (default-body (gethash t cases-table (list default))))
      `(let ((,input-var ,string))
         (flet ((,default-fn ()
                  ,@default-body))
           ,(emit-string-case (progn
                                (remhash t cases-table)
                                (hash-table->list cases-table))
                              input-var
                              `(,default-fn)))))))

(defvar *tab-width* 8)

;; pulled from SB-COVER
(defun detabify (string)
"Read STRING and replace all #\Tab characters with *TAB-WIDTH* spaces."
  (with-output-to-string (stream)
    (loop for char across string
          for col from 0
          for i from 0
          do (if (eql char #\Tab)
                 (loop repeat (- *tab-width* (mod col *tab-width*))
                       do (write-char #\Space stream)
                       do (incf col)
                       finally (decf col))
                 (progn
                   (when (eql char #\Newline)
                     ;; Filter out empty last line
                     (when (eql i (1- (length string)))
                       (return))
                     (setf col -1))
                   (write-char char stream))))))
