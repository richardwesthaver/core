;;;  lalr.lisp
;;;
;;;  This is an LALR parser generator.
;;;  (c) 1988 Mark Johnson. mj@cs.brown.edu
;;;  This is *not* the property of Xerox Corporation!

;;;  Modified to cache the first terminals, the epsilon derivations
;;;  the rules that expand a category, and the items that expand
;;;  a category

;;;  There is a sample grammar at the end of this file.
;;;  Use your text-editor to search for "Test grammar" to find it.

;;; (in-package 'LALR)
;;; (export '(make-parser lalr-parser *lalr-debug* grammar lexforms $ parse))

;;; (shadow '(first rest))
;;; (defmacro first (x) `(car ,x))
;;; (defmacro rest (x) `(cdr ,x))

;;;  The external interface is MAKE-PARSER.  It takes three arguments, a
;;;  CFG grammar, a list of the lexical or terminal categories, and an
;;;  atomic end marker.  It produces a list which is the Lisp code for
;;;  an LALR(1) parser for that grammar.  If that list is compiled, then
;;;  the function LALR-PARSER is defined.  LALR-PARSER is a function with
;;;  two arguments, NEXT-INPUT and PARSE-ERROR.
;;;
;;;  The first argument to LALR-PARSER, NEXT-INPUT must be a function with
;;;  zero arguments; every time NEXT-INPUT is called it should return
;;;  a CONS cell, the CAR of which is the category of the next lexical
;;;  form in the input and the CDR of which is the value of that form.
;;;  Each call to NEXT-INPUT should advance one lexical item in the
;;;  input.  When the input is consumed, NEXT-INPUT should return a
;;;  CONS whose CAR is the atomic end marker used in the call to MAKE-PARSER.
;;;
;;;  The second argument to LALR-PARSER, PARSE-ERROR will be called
;;;  if the parse fails because the input is ill-formed.
;;;
;;;
;;;  There is a sample at the end of this file.
(in-package :parse/lalr)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 1) (safety 3))))

;;; definitions of constants and global variables used

(defconstant +topcat+ '$Start)
(defvar      *end-marker*)
(defvar      *lex*)
(defvar      *lex-index*)
(defvar      *lex-vector*)
(defvar      *rules*)
(defvar      *nrules*)
(defvar      *start*)
(defvar      *starts*)
(defvar      *cats*)
(defvar      *firsts*)
(defvar      *epsilons*)
(defvar      *expansions*)
(defvar      *lalr-debug* NIL "Inserts debugging code into parser if non-NIL")
(defvar      *state-list* '())
(defvar      *state-hash*)
(defvar      *precedence*)

(defparameter *next-state-no* -1)

;;;; -- Compatibilty with old interface -------------------------------------------------------

(defmacro define-grammar (name lex-forms &rest grammar)
  (make-parser grammar lex-forms :eof :name name))

(defun make-parser (grammar lex end-marker &key (name 'lalr-parser))
  ;; This _must_ look like:
  ;;     (LET ((TABLE state-table)
  ;;           (ACTIONS actions))
  ;;       (DEFUN name (NEXT-INPUT PARSE-ERROR) ...))
  ;;
  (let* ((state-table (grammar-state-table grammar lex end-marker))
         (actions (make-array 0 :fill-pointer t :adjustable t))
         (state-vector (make-array (length state-table) :initial-element nil)))
    (loop for state in state-table do
          (destructuring-bind (qn &rest transitions) state
            (setf (aref state-vector qn)
                  (mapcar (lambda (tr)
                            (cond ((eq :reduce (cadr tr))
                                   (destructuring-bind (name n action) (cddr tr)
                                     (list (car tr) (cadr tr)
                                           name 
                                           n
                                           (or (position action actions :test 'equal)
                                               (prog1
                                                   (length actions)
                                                 (vector-push-extend action actions))))))
                                  (t
                                   tr)))
                          transitions))))
    (fill-parser-skeleton 
     name
     (caar state-table)
     `',state-vector
     `(vector ,@(coerce actions 'list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Rules and Grammars
;;;

(defstruct rule no mother daughters action)

(defun transform-rule (rule no)
  (make-rule :no no
             :mother (first rule)
             :daughters (butlast (cddr rule))
             :action (car (last rule))))

(defun compute-expansion (cat)
  (remove-if-not #'(lambda (rule)
                     (eq (rule-mother rule) cat))
                 *rules*))

(defmacro expand (cat)
  `(gethash ,cat *expansions*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                    Properties of grammars

(defun get-all-cats ()
  (labels ((try (deja-vu cat)
             (if (find cat deja-vu)
                 deja-vu
                 (try-rules (cons cat deja-vu) (compute-expansion cat))))
           (try-rules (deja-vu rules)
             (if rules
                 (try-rules (try-cats deja-vu (rule-daughters (car rules)))
                            (cdr rules))
                 deja-vu))
           (try-cats (deja-vu cats)
             (if cats
                 (try-cats (try deja-vu (car cats)) (cdr cats))
                 deja-vu)))
    (try '() *start*)))

(defun derives-eps (c)
  "t if c can be rewritten as the null string"
  (labels ((try (deja-vu cat)
             (unless (find cat deja-vu)
               (some #'(lambda (r)
                         (every #'(lambda (c1) (try (cons cat deja-vu) c1))
                                (rule-daughters r)))
                     (expand cat)))))
    (try '() c)))

(declaim (inline derives-epsilon))
(defun derives-epsilon (c)
  "looks up the cache to see if c derives the null string"
  (member c *epsilons* :test 'eq))

(declaim (inline cat-firsts))
(defun cat-firsts (cat)
  (gethash cat *starts*))

(defun first-terms (cat-list)
  "the leading terminals of an expansion of cat-list"
  (assert (= 1 (length cat-list)))
  (labels ((first-ds (cats)
             (if cats
                 (if (derives-epsilon (car cats))
                     (cons (car cats) (first-ds (cdr cats)))
                     (list (car cats)))))
           (try (deja-vu cat)
             (if (member cat deja-vu)
                 deja-vu
                 (try-list (cons cat deja-vu)
                           (mapcan #'(lambda (r)
                                       (first-ds (rule-daughters r)))
                                   (expand cat)))))
           (try-list (deja-vu cats)
             (if cats
                 (try-list (try deja-vu (car cats)) (cdr cats))
                 deja-vu)))
    (remove-if-not #'(lambda (term)
                       (or (eq *end-marker* term)
                           (find term *lex*)))
                   (try-list '() (first-ds cat-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parsing table constructor
;;;

(defstruct (item (:constructor cons-item (rule pos la)))
  rule pos la)
(defstruct citem rule pos la)

(defmacro item-daughters (i) `(rule-daughters (item-rule ,i)))
(defmacro citem-daughters (i) `(rule-daughters (citem-rule ,i)))

(defmacro item-right (i) `(nthcdr (item-pos ,i) (item-daughters ,i)))
(defmacro citem-right (i) `(nthcdr (citem-pos ,i) (citem-daughters ,i)))

(defmacro item-equal (i1 i2)
  `(and (eq (item-rule ,i1) (item-rule ,i2))
        (= (item-pos ,i1) (item-pos ,i2))
        (eq (item-la ,i1) (item-la ,i2))))

(defun item-core-equal (c1 c2)
  "T if the cores of c1 and c2 are equal"
  (and (eq (item-rule c1) (item-rule c2))
       (= (item-pos c1) (item-pos c2))))

(defun citem-core-equal (c1 c2)
  "T if the cores of c1 and c2 are equal"
  (and (eq (citem-rule c1) (citem-rule c2))
       (= (citem-pos c1) (citem-pos c2))))

(defun citem-item-core-equal (c1 c2)
  "T if the cores of c1 and c2 are equal"
  (and (eq (citem-rule c1) (item-rule c2))
       (= (citem-pos c1) (item-pos c2))))

(defun shift-citems (items cat)
  "shifts a set of items over cat"
  (labels ((shift-item (item)
             (if (eq (first (citem-right item)) cat)
                 (make-citem :rule (citem-rule item)
                             :pos (1+ (citem-pos item))
                             :la (citem-la item)))))
    (let ((new-items '()))
      (dolist (i items)
        (let ((n (shift-item i)))
          (if n
              (push n new-items))))
      new-items)))

(defun items-right (items)
  "returns the set of categories appearing to the right of the dot"
  (let ((right '()))
    (dolist (i items)
      (let ((d (first (item-right i))))
        (if (and d (not (find d right)))
          (push d right))))
    right))

(defun citems-right (items)
  "returns the set of categories appearing to the right of the dot"
  (let ((right '()))
    (dolist (i items)
      (let ((d (first (citem-right i))))
        (and d (pushnew d right))))
    right))

(defun compact-items (items)
  "collapses items with the same core to compact items"
  (let ((sofar '()))
    (dolist (i items)
      (let ((ci (dolist (s sofar)
                  (if (citem-item-core-equal s i)
                      (return s)))))
        (if ci
            (push (item-la i) (citem-la ci))
            (push (make-citem :rule (item-rule i)
                              :pos (item-pos i)
                              :la (list (item-la i)))
                  sofar))))
    (sort sofar #'<
          :key #'(lambda (i) (rule-no (citem-rule i))))))

(defun expand-citems (citems)
  "expands a list of compact items into items"
  (let ((items '()))
    (dolist (ci citems)
      (dolist (la (citem-la ci))
        (push (cons-item  (citem-rule ci)
                          (citem-pos ci)
                          la)
              items)))
    items))

(defun subsumes-citems (ci1s ci2s)
  "T if the sorted set of items ci2s subsumes the sorted set ci1s"
  (and (= (length ci1s) (length ci2s))
       (every #'(lambda (ci1 ci2)
                  (and (citem-core-equal ci1 ci2)
                       (subsetp (citem-la ci1) (citem-la ci2))))
              ci1s ci2s)))

(defun merge-citems (ci1s ci2s)
  "Adds the las of ci1s to ci2s.  ci2s should subsume ci1s"
  (mapcar #'(lambda (ci1 ci2)
              (setf (citem-la ci2) (union (citem-la ci1) (citem-la ci2))))
          ci1s ci2s)
  ci2s)

;;;  The actual table construction functions

(defstruct state name citems reduces shifts conflict)
(defstruct shift cat where)

;(defun lookup (citems)
;  "finds a state with the same core items as citems if it exits"
;  (find-if #'(lambda (state)
;               (and (= (length citems) (length (state-citems state)))
;                    (every #'(lambda (ci1 ci2)
;                               (item-core-equal ci1 ci2))
;                            citems (state-citems state))
;                    ))
;           *state-list*))

#+(OR)
(defun lookup (citems)
  "finds a state with the same core items as citems if it exits"
  (dolist (state *state-list*)
    (if (and (= (length citems) (length (state-citems state)))
             (do ((ci1s citems (cdr ci1s))
                  (ci2s (state-citems state) (cdr ci2s)))
                 ((null ci1s) t)
               (unless (citem-core-equal (car ci1s) (car ci2s))
                 (return nil))))
        (return state))))

(defun lookup (citems)
  "finds a state with the same core items as citems if it exits"
  (gethash (citems-key citems) *state-hash*))

(defun add-state (citems)
  "creates a new state and adds it to the state list"
  (let ((new-state
         (make-state :name (incf *next-state-no*)
                     :citems citems)))
    (push new-state *state-list*)
    (setf (gethash (citems-key citems) *state-hash*) new-state)
    new-state))

(defun citems-key (citems)
  (mapcan (lambda (citem)
            (list (citem-rule citem) (citem-pos citem)))
          citems))

(defun get-state-name (citems)
  "returns the state name for this set of items"
  (let* ((state (lookup citems)))
    (cond ((null state)
           (setq state (add-state citems))
           (build-state state citems))
          ((subsumes-citems citems (state-citems state))
           nil)
          (t
           (merge-citems citems (state-citems state))
           (follow-state citems)))
    (state-name state)))

(defun build-state (state citems)
  "creates the states that this state can goto"
  (let ((closure (close-citems citems)))
    (dolist (cat (citems-right closure))
      (push (make-shift :cat cat
                        :where (get-state-name (shift-citems closure cat)))
            (state-shifts state)))))

(defun follow-state (citems)
  "percolates look-ahead onto descendant states of this state"
  (let ((closure (close-citems citems)))
    (dolist (cat (citems-right closure))
      (get-state-name (shift-citems closure cat)))))

(defun build-table ()
  "Actually builds the table"
  (setq *state-list* '())
  (setq *state-hash* (make-hash-table :test 'equal))
  (setq *next-state-no* -1)
  (get-state-name (list (make-citem :rule (make-rule :no 0
                                                     :mother +topcat+
                                                     :daughters (list *start*))
                                    :pos 0
                                    :la (list *end-marker*))))
  (setq *state-list* (nreverse *state-list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parsing table printer
;;;

(defun print-table (*state-list*)
  "Prints the state table"
  (dolist (state *state-list*)
    (format t "~%~%~a:" (state-name state))
    (dolist (citem (state-citems state))
      (format t "~%  ~a -->~{ ~a~} .~{ ~a~}, ~{~a ~}"
              (rule-mother (item-rule citem))
              (subseq (rule-daughters (item-rule citem)) 0 (item-pos citem))
              (subseq (rule-daughters (item-rule citem)) (item-pos citem))
              (item-la citem)))
    (dolist (shift (state-shifts state))
      (format t "~%    On ~a shift ~a" (shift-cat shift) (shift-where shift)))
    (dolist (reduce (delete-if #'(lambda (i) (citem-right i))
                               (close-citems
                                (state-citems state))))
      (format t "~%    On~{ ~a~} reduce~{ ~a~} --> ~a"
              (citem-la reduce)
              (rule-daughters (citem-rule reduce))
              (rule-mother (citem-rule reduce)))))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                  LALR(1) parser constructor
;;;

;;;  next-input performs lexical analysis.  It must return a cons cell.
;;;  its car holds the category, its cdr the value.

#+(OR)
(defun close-citems (citems)
  "computes the closure of a set of items"
  (declare (optimize (speed 3) (safety 0)))
  (let ((sofar (make-array *nrules* :initial-element nil)))
    (declare (type (simple-array t (*)) sofar))
    (let ((todo nil))
      (dolist (q (expand-citems citems))
        (push (list (item-rule q)
                    (item-pos q)
                    (item-la q))
              todo))
      (do () ((null todo))
        (destructuring-bind (rule pos la)
            (pop todo)
          (let* ((rgt (nthcdr pos (rule-daughters rule)))
                 (las (first-terminals-1 (rest rgt) la)))
            (when rgt
              (dolist (r (expand (first rgt)))
                (let ((ri (rule-no r)))
                  (declare (type fixnum ri))
                  (let* ((q (or (svref sofar ri)
                                (setf (svref sofar ri)
                                      (let ((new (make-citem :rule r :pos 0)))
                                        (push new citems)
                                        new)))))
                    (dolist (la las)
                      (unless (member la (citem-la q) :test 'eq)
                        (push la (citem-la q))
                        (push (list r 0 la) todo))))))))))))
  (sort (copy-list citems) #'< 
        :key #'(lambda (i) (rule-no (citem-rule i)))))

#+(OR)
(defun close-citems (citems)
  "computes the closure of a set of items"
  (declare (optimize (speed 3) (safety 0)))
  (let ((sofar (make-array *nrules* :initial-element nil)))
    (declare (type (simple-array t (*)) sofar))
    (let ((todo nil))
      (dolist (q (expand-citems citems))
        (push q todo))
      (do () ((null todo))
        (multiple-value-bind (rule pos la)
            (let ((i (pop todo)))
              (values (item-rule i) (item-pos i) (item-la i)))
          (let* ((rgt (nthcdr pos (rule-daughters rule))))
            (when rgt
              (let ((las (first-terminals-1 (rest rgt) la)))
                (dolist (r (expand (first rgt)))
                  (let ((ri (rule-no r)))
                    (declare (type fixnum ri))
                    (let* ((q (or (svref sofar ri)
                                  (setf (svref sofar ri)
                                        (let ((new (make-citem :rule r :pos 0)))
                                          (push new citems)
                                          new)))))
                      (dolist (la las)
                        (unless (member la (citem-la q) :test 'eq)
                          (push la (citem-la q))
                          (push (cons-item r 0 la) todo)))))))))))))
  (sort (copy-list citems) #'< 
        :key #'(lambda (i) (rule-no (citem-rule i)))))

(defun close-citems (citems)
  "computes the closure of a set of items"
  (declare (optimize (speed 3) (safety 0)))
  (let ((sofar (make-array *nrules* :initial-element nil)))
    (declare (type (simple-array t (*)) sofar))
    (let ((todo nil))
      (declare (type list todo))
      (dolist (q (expand-citems citems))
        (push q todo))
      (do () ((null todo))
        (multiple-value-bind (rule pos la)
            (let ((i (pop todo)))
              (values (item-rule i) (item-pos i) (item-la i)))
          (let* ((rgt (nthcdr pos (rule-daughters rule))))
            (when rgt
              (let ((las (first-terminals-1 (rest rgt) la)))
                (dolist (r (expand (first rgt)))
                  (let ((ri (rule-no r)))
                    (declare (type fixnum ri))
                    (let* ((q (or (svref sofar ri)
                                  (setf (svref sofar ri)
                                        (let ((new (make-citem :rule r :pos 0)))
                                          (push new citems)
                                          new)))))
                      (let ((new-la (citem-la q))) 
                        (dolist (la las)
                          (unless (member la new-la :test 'eq)
                            (push la new-la)
                            (push (cons-item r 0 la) todo)))
                        (setf (citem-la q) new-la))))))))))))
  (sort (copy-list citems) #'< 
        :key #'(lambda (i) (rule-no (citem-rule i)))))

(defun mask->la (mask)
  (loop for bit below (integer-length mask)
        when (logbitp bit mask)
        collect (aref *lex-vector* bit)))

(defun la->mask (la)
  (loop for term in la sum (ash 1 (gethash term *lex-index*))))

(defun build-parser-table-1 (state-list)
  (mapcar #'translate-state-1 state-list))

(defun translate-state-1 (state)
  "translates a state into lisp code that could appear in a labels form"
  (let ((reduces (state-reduces state))
        (symbols-sofar '())) ; to ensure that a symbol never occurs twice
    (setf (state-reduces state) reduces)
    (labels ((translate-shift (shift)
               (push (shift-cat shift) symbols-sofar)
               `((,(shift-cat shift))
                 :shift
                 ,(shift-where shift)))
             (translate-reduce (item)
               (when (intersection (citem-la item) symbols-sofar)
                 (fresh-line)
                 (pprint-logical-block (*standard-output* nil :per-line-prefix ";; ")
                   (format t
                           "Warning, shift/reduce conflict: state ~a: ~a --> ~{~a ~} <*> ~{~a ~}~%"
                           (state-name state)
                           (rule-mother (citem-rule item))
                           (subseq (rule-daughters (citem-rule item)) 0 (citem-pos item))
                           (subseq (rule-daughters (citem-rule item)) (citem-pos item))
                           )
                   (format t "On ~@<~{~S~^, ~:_~}~:>"
                           (citem-la item)))
                 (terpri)
                 (force-output)
                 '(setf (citem-la item)
                       (set-difference (citem-la item)
                                       symbols-sofar)))
               (dolist (la (citem-la item))
                 (push la symbols-sofar))
               `(,(copy-list (citem-la item))
                  :reduce
                  ,(rule-mother (citem-rule item))
                  ,(citem-pos item)
                  ,(rule-action (citem-rule item)))))
      `(,(state-name state) 
         ,@(mapcar #'translate-shift (state-shifts state))
         ,@(mapcar #'translate-reduce reduces)))))

;;      table ::= ( { <state> }* )
;;      state ::= (<state> { <transition> }*)
;; transition ::= (<cat-list> :SHIFT <state-name>)
;;              | (<cat-list> :REDUCE <cat> <n> <action>)



(defstruct (lalr-table (:constructor cons-lalr-table))
  topcat
  states)

(defstruct lalr-state
  name
  transitions)

(defstruct lalr-action
  categories
  goto)

(defstruct (reduce-action (:include lalr-action))
  npop
  function)

(defstruct (shift-action (:include lalr-action))
  )


;;;;;;;;;

(defun make-lalr-table (state-table)
  (cons-lalr-table
   :topcat (caar state-table)
   :states (mapcar (lambda (state)
                     (make-lalr-state
                      :name (car state)
                      :transitions
                      (mapcar (lambda (tr)
                                (ecase (cadr tr)
                                  (:shift (make-shift-action :categories (car tr)
                                                             :goto (caddr tr)))
                                  (:reduce (make-reduce-action :categories (car tr)
                                                               :goto (third tr)
                                                               :npop (fourth tr)
                                                               :function (let ((f (fifth tr)))
                                                                           (cadr f))))))
                              (cdr state))))
                   state-table)))

(defun grammar-lalr-table (lex rules &key (eof :eof))
  (make-lalr-table (grammar-state-table rules lex eof)))

(defun grammar-state-table (grammar lex end-marker)
  "Takes a grammar and produces the Lisp code for a parser for that grammar"
  (setq *end-marker* end-marker)
  (setq *precedence* nil)
  (setq *lex* nil)
  (dolist (k lex)
    (if (stringp k) (keywordify k) (push k *lex*)))
  ;; Pull :tokens
  (setq grammar (remove-if (lambda (rule)
                             (cond ((typep rule '(cons (member :tokens)))
                                    (setq *lex* (append (keywordify (cdr rule)) *lex*))
                                    t)
                                   (t nil)))
                           grammar))
  ;; Pull precedence
  (setq grammar (remove-if (lambda (rule)
                             (cond ((typep rule '(cons (member :precedence) list))
                                    (dolist (prec (cdr rule))
                                      (etypecase prec
                                        ((cons (member :left :right :nonassoc) list)
                                         (setq *precedence* (nconc *precedence* (list (keywordify prec)))))))
                                    t)
                                   (t nil)))
                           grammar))
  (setq grammar (mapcan #'edit-rule grammar))
  (setq grammar (mapcar (lambda (x)
                          (append (keywordify (butlast x)) (list (car (last x)))))
                        grammar))
  (let ((def *lex*) (use nil))
    (push (caar grammar) use)
    (dolist (r grammar)
      (pushnew (car r) def)
      (dolist (k (butlast (cddr r))) (pushnew k use)))
    (let ((missing (set-difference use def)))
      (when missing
        (warn "Undefined categories: ~@<~{~S~^ ~:_~}~:>" missing)))
    (let ((unused  (set-difference def use)))
      (when unused
        (warn "Unused categories: ~@<~{~S~^ ~:_~}~:>" unused))))
  ;;  cache some data that will be useful later
  (setq *lex-vector* (coerce (cons *end-marker* *lex*) 'vector))
  (setq *lex-index* (make-hash-table :test 'eq))
  (loop for j from 0 for c across *lex-vector* do (setf (gethash c *lex-index*) j))
  (setq *start* (caar grammar))
  (setq *rules* (let ((i 0))
                  (mapcar #'(lambda (r) (transform-rule r (incf i)))
                          grammar)))
  (setq *nrules* (1+ (length *rules*)))
  (setq *cats* (get-all-cats))
  (setq *expansions* (make-hash-table :test 'eq))
  (dolist (cat *cats*)
    (setf (gethash cat *expansions*) (compute-expansion cat)))
  #-(OR)
  (setq *epsilons* (remove-if-not #'derives-eps *cats*))
  #+(OR)
  (progn
    (setq *epsilons* (make-hash-table :test 'eq))
    (dolist (cat *cats*) (when (derives-eps cat) (setf (gethash cat *epsilons*) t))))
  (setq *starts* (make-hash-table :test 'eq))
  #+(OR)
  (dolist (cat (cons *end-marker* *cats*))
    (setf (gethash cat *starts*) (la->mask (first-terms (list cat)))))
  #-(OR)
  (dolist (cat (cons *end-marker* *cats*))
    (setf (gethash cat *starts*) (first-terms (list cat))))
  ;; now actually build the parser
  (build-table)
  (when (and (listp *lalr-debug*) (member 'print-table *lalr-debug*))
    (print-table *state-list*))
  (format t "~&Table ready, ~D rules, ~D states.~%"
          *nrules* (length *state-list*))
  (force-output)
  (setq *state-list* (compute-reduces *state-list*))
  (setq *state-list* (resolve-conflicts *state-list*))
  (build-parser-table-1 *state-list*))

(defun keywordify (thing)
  (cond ((stringp thing)
         (let ((x (intern (string thing) :keyword)))
           (pushnew x *lex*)
           x))
        ((atom thing) thing)
        ((mapcar #'keywordify thing))))

(defun first-terminals-1 (cat-list more &aux res)
  (declare (optimize (speed 3) (safety 0))
           (type list cat-list))
  (loop
    (cond ((null cat-list)
           (return (if res (union res (cat-firsts more)) (cat-firsts more))))
          ((derives-epsilon (first cat-list))
           (setf res (if res
                         (union res (cat-firsts (first cat-list)))
                         (cat-firsts (first cat-list))))
           (setq cat-list (rest cat-list)))
          (t
           (return (if res
                       (union res (cat-firsts (first cat-list)))
                       (cat-firsts (first cat-list))))))))

(defun fill-parser-skeleton (name start-state state-table actions)
  `(LET ((TABLE ,state-table)
         (ACTIONS ,actions))
     (DEFUN ,name (NEXT-INPUT PARSE-ERROR)
       (LET ((CAT-LA '())                 ;category lookahead
             (VAL-LA '())                 ;value lookahead
             (VAL-STACK '())              ;value stack
             (STATE-STACK '())            ;state stack
             (CUR-STATE ',start-state))   ;current state
         (LABELS ((INPUT-PEEK ()
                    (UNLESS CAT-LA
                      (SETF (VALUES CAT-LA VAL-LA) (FUNCALL NEXT-INPUT)
                            CAT-LA (LIST CAT-LA)
                            VAL-LA (LIST VAL-LA)))
                    (FIRST CAT-LA))
                  (SHIFT-FROM (NEW-STATE)
                    (PUSH CUR-STATE STATE-STACK)
                    (POP CAT-LA)
                    (PUSH (POP VAL-LA) VAL-STACK)
                    (SETF CUR-STATE NEW-STATE))
                  (REDUCE-CAT (NAME CAT NDAUGHTERS ACTION)
                    (IF (EQ CAT ',+topcat+)
                        (RETURN-FROM ,name (POP VAL-STACK))
                        (LET ((DAUGHTER-VALUES '())
                              (STATE NAME))
                          (DOTIMES (I NDAUGHTERS)
                            (PUSH (POP VAL-STACK) DAUGHTER-VALUES)
                            (SETQ STATE (POP STATE-STACK)))
                          (PUSH CAT CAT-LA)
                          (LET ((VALUE (APPLY (AREF ACTIONS ACTION) DAUGHTER-VALUES)))
                            ;; (warn "Reducing ~S ~S => ~S" cat DAUGHTER-VALUES VALUE)
                            (PUSH  VALUE VAL-LA))
                          (SETQ CUR-STATE STATE)))))
           (LOOP
             (LET ((Q (CDR (LET ((P (AREF TABLE CUR-STATE)))
                             (OR (ASSOC (INPUT-PEEK) P :TEST #'MEMBER)
                                 (FUNCALL PARSE-ERROR
                                          (LET ((*PRINT-LEVEL* 5)
                                                (*PRINT-LENGTH* 99))
                                            (error (list (MAPCAR #'CAR P) (CAR CAT-LA))))))))))
               (ECASE (FIRST Q)
                 ((:SHIFT) (SHIFT-FROM (CADR Q)))
                 ((:REDUCE) (APPLY #'REDUCE-CAT CUR-STATE (REST Q)))))))))))


;;;; ------------------------------------------------------------------------------------------

(defun lalr-parse (table next-input)
  (LET ((CAT-LA '())                    ;category lookahead
        (VAL-LA '())                    ;value lookahead
        (VAL-STACK '())                 ;value stack
        (STATE-STACK '())               ;state stack
        (CUR-STATE 0))                  ;current state
    (LABELS ((INPUT-PEEK ()
               (UNLESS CAT-LA
                 (SETF (VALUES CAT-LA VAL-LA) (FUNCALL NEXT-INPUT)
                       CAT-LA (LIST CAT-LA)
                       VAL-LA (LIST VAL-LA)))
               (FIRST CAT-LA)))
      (LOOP
        (LET ((ACTION (FIND (INPUT-PEEK)
                            (LALR-STATE-TRANSITIONS (ELT (LALR-TABLE-STATES TABLE) CUR-STATE))
                            :KEY #'LALR-ACTION-CATEGORIES
                            :TEST #'MEMBER)))
          (TYPECASE ACTION
            (SHIFT-ACTION
             (PUSH CUR-STATE STATE-STACK)
             (POP CAT-LA)
             (PUSH (POP VAL-LA) VAL-STACK)
             (SETF CUR-STATE (SHIFT-ACTION-GOTO ACTION)))
            (REDUCE-ACTION
             (IF (EQL (REDUCE-ACTION-GOTO ACTION) +TOPCAT+)     ;hmm
                 (RETURN-FROM LALR-PARSE (POP VAL-STACK))
                 (LET ((DAUGHTER-VALUES '()))
                   (DOTIMES (I (REDUCE-ACTION-NPOP ACTION))
                     (PUSH (POP VAL-STACK) DAUGHTER-VALUES)
                     (SETQ CUR-STATE (POP STATE-STACK)))
                   (PUSH (REDUCE-ACTION-GOTO ACTION) CAT-LA)
                   (LET ((VALUE (APPLY (REDUCE-ACTION-FUNCTION ACTION) DAUGHTER-VALUES)))
                     (PUSH  VALUE VAL-LA)))))
            (T
             (error "parse error~%LA = ~S~%STATE = ~S~%" CAT-LA CUR-STATE)) ))))))

;;;; ------------------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                   Test grammar and lexical analyser
;;;

;;;  A Test grammar

#+(or)
(progn
  (define-grammar lalr-parser
    (det n v)
    (s --> np vp      #'(lambda (np vp) (list 's np vp)))
    (np --> det n     #'(lambda (det n) (list 'np det n)))
    (np -->           #'(lambda () '(np)))
    (vp --> v np      #'(lambda (v np) (list 'vp v np)))
    (vp --> v s       #'(lambda (v s) (list 'vp v s))))

  (defparameter *lexicon* '((the det)
                            (man n)
                            (woman n)
                            (cat n)
                            (dog n)
                            (loves v)
                            (thinks v)
                            (hates v)))

  (defun parse (words)
    (labels ((lookup (word)
               (cadr (assoc word *lexicon*)))
             (next-input ()
               (cond ((null words) :eof)
                     (t
                      (let* ((word (pop words))
                             (cat (lookup word)))
                        (values cat                ; category
                                (list cat word)))))) ; value
             (parse-error (msg)
               (format nil "Error before ~a: ~a" words msg)))
      (lalr-parser #'next-input #'parse-error))))

;;; *EOF*

;;;; -- Conflict Resolution -------------------------------------------------------------------

#+(or)
(progn
  (defun op-cons (a op b) (list op a b))

  (defun blah ()
    (make-parser
     '((:precedence
        (:left + -)
        (:left * /))
       (start -> expr #'identity)
       (expr -> expr + expr #'op-cons)
       (expr -> expr - expr #'op-cons)
       (expr -> expr * expr #'op-cons)
       (expr -> expr / expr #'op-cons)
       ;; (expr -> expr * expr #'op-cons)
       (expr -> number #'identity)
       (expr -> [ expr ] #'(lambda (a b c) a b c b)))
     '(+ * - / number [ ] )
     :eof))

  (defun blah ()
    (make-parser
     '((:precedence
        (:left + -)
        (:left * /))
       (start -> expr)
       (expr -> expr + expr => `(+ ,$1 ,$3))
       (expr -> expr - expr => `(- ,$1 ,$3))
       (expr -> expr * expr => `(* ,$1 ,$3))
       (expr -> expr / expr => `(/ ,$1 ,$3))
       (expr -> number)
       (expr -> [ expr ] => $2))
     '(+ * - / number [ ] )
     :eof))

  (defun blah ()
    (make-parser
     '((stmt -> expr :\; #'(lambda (a b) a))
       (stmt -> if [ expr ] stmt #'(lambda (a b c d e) `(if ,c ,e)))
       (stmt -> if [ expr ] stmt else stmt #'(lambda (a b c d e f g) `(if ,c ,e ,g)))
       (expr -> expr + expr #'op-cons)
       (expr -> expr - expr #'op-cons)
       (expr -> expr * expr #'op-cons)
       (expr -> expr / expr #'op-cons)
       ;; (expr -> expr * expr #'op-cons)
       (expr -> number #'identity)
       (expr -> [ expr ] #'(lambda (a b c) a b c b)))
     '(+ * - / number [ ] if else :\;)
     :eof))

  (defun parse (words)
    (lalr-parser (lambda ()
                   (cond ((null words) :eof)
                         ((numberp (car words)) (values 'number (pop words)))
                         (t (let ((q (pop words))) (values q q)))))
                 #'error))
  )

(defun compute-reduces (state-list)
  (dolist (state state-list state-list)
    (setf (state-reduces state) (delete-if #'(lambda (i) (citem-right i))
                                           (close-citems (state-citems state))))))

(defun resolve-conflicts (state-list)
  (loop for state in state-list
        for state-no from 0
        collect
        (let* ((all-cat
                (remove-duplicates (append (mapcan (lambda (p)
                                                     (copy-list (citem-la p)))
                                                   (state-reduces state))
                                           (mapcar #'shift-cat (state-shifts state)))))
               (new-transitions
                (loop for cat in all-cat append
                      (let ((candidates
                             (append (remove cat (state-shifts state) :test-not 'eq :key #'shift-cat)
                                     (remove-if-not
                                      (lambda (p) (eq cat (item-la p)))
                                      (expand-citems (state-reduces state))))))
                        (cond ((= (length candidates) 1)
                               candidates)
                              ((and (= (length candidates) 2)
                                    (shift-p (car candidates))
                                    (item-p (cadr candidates)))
                               (destructuring-bind (shift reduce) candidates
                                 (let ((shift-prec (precedence cat))
                                       (reduce-prec (some #'precedence (item-daughters reduce))))
                                   (cond ((and shift-prec reduce-prec
                                               (> (cadr shift-prec) (cadr reduce-prec)))
                                          (list shift))
                                         ((and shift-prec reduce-prec
                                               (< (cadr shift-prec) (cadr reduce-prec)))
                                          (list reduce))
                                         ((eq (car shift-prec) :left)
                                          (list reduce))
                                         ((eq (car shift-prec) :right)
                                          (list shift))
                                         ((eq (car shift-prec) :nonassoc)
                                          (list shift))
                                         (t
                                          (warn "~@<Unresolvable shift/reduce conflict in state ~D on ~S:~%~S~%shift-prec = ~S~:@>"
                                                (state-name state)
                                                cat
                                                candidates
                                                shift-prec)
                                          (list shift))))))
                              (t
                               (warn "Unresolvable conflict on ~S:~%~S"
                                                cat candidates)
                               #+NIL
                               (multiple-value-call #'warn
                                 "Unresolvable conflict on ~S:~%~@<    ~@;~?~:>"
                                 cat
                                 (conflict-formatting candidates))
                               (list (cond ((find-if #'shift-p candidates))
                                           ((car (sort (copy-list candidates) #'<
                                                       :key (lambda (i)
                                                              (rule-no (item-rule i))))))))
                               ))))))
          (setq state (copy-state state))
          (setf (state-shifts state)  (remove-if-not #'shift-p new-transitions)
                (state-reduces state) (compact-items (remove-if-not #'item-p new-transitions)))
          state)))

#+(or)
(defun conflict-formatting (candidates)
  (values "~@{~{~S -> ~{~?~^ ~}~%~}~}"
          (mapcar (lambda (item)
                    (let ((r (item-rule item))
                          (p (item-pos item)))
                      (list (rule-mother r)
                            (append (mapcan (lambda (x) (list "~S" (list x))) (subseq (rule-daughters r) 0 p))
                                    (list "." nil)
                                    (mapcan (lambda (x) (list "~S" (list x))) (subseq (rule-daughters r) p))))))
                  (cdr candidates))))

(defun conflict-formatting (candidates)
  (values "~@{~{~S -> ~{~?~^ ~}~%~}~}"
          (mapcar (lambda (item)
                    (let ((r (item-rule item))
                          (p (item-pos item)))
                      (list (rule-mother r)
                            (append (mapcan (lambda (x) (list "~S" (list x))) (subseq (rule-daughters r) 0 p))
                                    (list "." nil)
                                    (mapcan (lambda (x) (list "~S" (list x))) (subseq (rule-daughters r) p))))))
                  candidates)))

;;
;; (:precedence { :left | :right | :nonassoc } lexeme...)
;; 

(defun precedence (lexeme)
  (do ((i 0 (1+ i))
       (p *precedence* (cdr p)))
      ((null p) nil)
    (when (member lexeme (cdar p))
      (return (list (caar p) i)))))

(defun edit-rule (rule)
  (let ((p (member-if (lambda (x) (and (symbolp x) (not (keywordp x)) (string= x "->")))
                      (cddr rule))))
    (cond ((not (null p))
           (append (edit-rule (ldiff rule p))
                   (edit-rule (cons (car rule) p))))
          (t
           (let* ((p (member-if (lambda (x) (and (symbolp x) (not (keywordp x)) (string= x "=>"))) rule))
                  (head (and p (ldiff rule p))))
             (labels ((wrap-parameters (n body)
                        (let* ((params (loop for i from 1 to n collect (intern (format nil "$~D" i)))))
                          `#'(lambda ,params
                               (declare (ignorable ,@params))
                               ,@(if body nil (list (car params)))
                               ,@body))))
               (cond ((and p (>= (length head) 2))
                      (list `(,@(ldiff rule p) ,(wrap-parameters (- (length head) 2) (cdr p)))))
                     ((symbolp (car (last rule)))
                      (list `(,@rule ,(wrap-parameters (- (length rule) 2) nil))))
                     (t
                      (list rule)))))))))

;;;; -- Demo ----------------------------------------------------------------------------------

#+(or)
(progn
  (defun parse (string)
    (lalr-parser (tokenizer string) #'error))

;;; A dead simple scanner

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
     :eof)))

;;;; ------------------------------------------------------------------------------------------
