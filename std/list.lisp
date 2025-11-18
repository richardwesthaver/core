;;; std/list.lisp --- List utils

;;; Code:
(in-package :std/list)

(defun ensure-car (thing)
  "If THING is a CONS, its CAR is returned. Otherwise THING is returned."
  (if (consp thing)
      (car thing)
      thing))

(defun ensure-cons (cons)
  "If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr."
  (if (consp cons)
      cons
      (cons cons nil)))

(define-modify-macro appendf (&rest lists) append
  "Modify-macro for APPEND. Appends LISTS to the place designated by the first
argument.")

(define-modify-macro nconcf (&rest lists) nconc
  "Modify-macro for NCONC. Concatenates LISTS to place designated by the first
argument.")

(define-modify-macro unionf (list &rest args) union
  "Modify-macro for UNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place.")

(define-modify-macro nunionf (list &rest args) nunion
  "Modify-macro for NUNION. Saves the union of LIST and the contents of the
place designated by the first argument to the designated place. May modify
either argument.")

(define-modify-macro reversef () reverse
  "Modify-macro for REVERSE. Copies and reverses the list stored in the given
place and saves back the result into the place.")

(define-modify-macro nreversef () nreverse
  "Modify-macro for NREVERSE. Reverses the list stored in the given place by
destructively modifying it and saves back the result into the place.")

(declaim (inline remove/swapped-arguments))
(defun remove/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'remove item sequence keyword-arguments))

(define-modify-macro removef (item &rest keyword-arguments)
  remove/swapped-arguments
  "Modify-macro for REMOVE. Sets place designated by the first argument to
the result of calling REMOVE with ITEM, place, and the KEYWORD-ARGUMENTS.")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest keyword-arguments)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the KEYWORD-ARGUMENTS.")

(defun let-binding-transform (bs)
  (if bs
      (cons
       (cond ((symbolp (car bs))
              (list (car bs)))
             ((consp (car bs))
              (car bs))
             (t
              (error "Bad let bindings")))
       (let-binding-transform (cdr bs)))))

(defun circular-list (&rest elements)
  "Creates a circular list of ELEMENTS."
  (let ((cycle (copy-list elements)))
    (nconc cycle cycle)))

(defun circular-list-p (object)
  "Returns true if OBJECT is a circular list, NIL otherwise."
  (and (listp object)
       (do ((fast object (cddr fast))
            (slow (cons (car object) (cdr object)) (cdr slow)))
           (nil)
         (unless (and (consp fast) (listp (cdr fast)))
           (return nil))
         (when (eq fast slow)
           (return t)))))

(defun circular-tree-p (object)
  "Returns true if OBJECT is a circular tree, NIL otherwise."
  (labels ((circularp (object seen)
             (and (consp object)
                  (do ((fast (cons (car object) (cdr object)) (cddr fast))
                       (slow object (cdr slow)))
                      (nil)
                    (when (or (eq fast slow) (member slow seen))
                      (return-from circular-tree-p t))
                    (when (or (not (consp fast)) (not (consp (cdr slow))))
                      (return
                        (do ((tail object (cdr tail)))
                            ((not (consp tail))
                             nil)
                          (let ((elt (car tail)))
                            (circularp elt (cons object seen))))))))))
    (circularp object nil)))

;;; On Lisp
(declaim (inline group))
(defun group (source n)
  "Return a list of lists by grouping SOURCE into N-element batches."
  (declare (fixnum n))
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun flatten (x)
    "Flatten list X, removing nil elements."
    (let (list)
      (labels ((rec (tree)
                 (when tree
                   (if (consp tree)
                       (progn 
                         (rec (car tree))
                         (rec (cdr tree)))
                       (push tree list)))))
        (rec x)
        (nreverse list))))

  (defun flatten* (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun zip-list (&rest args)
    "Return a list of lists containing every member of ARGS at the same position."
    (apply 'map 'list 'list args)))

(defun zip-tree (&rest args)
  (if (and (some #'atom args) (some #'consp args)) nil
      (if (every #'atom args) args
          (apply #'mapcar #'zip-tree args))))

(defun ziptree (tree &rest more-trees)
  (if (atom tree)
      (cons tree more-trees)
      (apply #'mapcar (list* #'ziptree tree more-trees))))

(defun zip (&rest args)
  "Zips the elements of @arg{args}.
  Example:
  (zip '(2 3 4) '(a b c) '(j h c s))
  => ((2 A J) (3 B H) (4 C C))"
  (apply #'map 'list #'list args))

(defun unzip (list)
  "UnZips the elements of @arg{args}.
  Example:
  (unzip ((2 A J) (3 B H) (4 C C)))
  => ((2 3 4) (a b c) (j h c))"
  (mapcar #'(lambda (n) (mapcar #'(lambda (x) (elt x n)) list))
          (loop for i from 0 below (length (first list)) 
                collect i)))

(defun zipsym (lst)
  "Zips a unique gensym with each element of LST.

Example:
(zipsym '(a b c)) ;; ((#:G1064 A) (#:G1065 B) (#:G1066 C))"  
  (map 'list #'(lambda (x) (list (gensym) x)) lst))

(defun list-dimensions (lst)
  (if (atom lst) nil
      (cons (length lst) (list-dimensions (car lst)))))

(defun recursive-append (&rest lsts)
  "Append lists in a nested manner.

Example:

(recursive-append
  '(let ((x 1)))
  '(+ x 2))

;; (LET ((X 1)) (+ X 2))"
  (reduce #'(lambda (x y)
              (if (null x)
                  (if (typep (car y) 'symbol) y (car y))
                  (append x (and y (if (typep (car y) 'symbol) `(,y) y)))))
          lsts :from-end t))

(defmacro ziprm (r m &rest args)
  "Reduce-Map on ARGS.

Example:

(macroexpand-1
  `(ziprm (and =) (a b c) (1 2 3)))
;; (AND (= A 1) (= B 2) (= C 3))"
  `(,r ,@(apply #'mapcar #'(lambda (&rest atoms) (cons m atoms)) (mapcar #'ensure-list args))))

(defun circular-list-error (list)
  (error 'type-error
         :datum list
         :expected-type '(and list (not circular-list))))

(declaim (inline safe-endp))
(defun safe-endp (x)
  (declare (optimize safety))
  (endp x))

(macrolet ((def (name lambda-list doc step declare ret1 ret2)                      
               (assert (member 'list lambda-list))                                   
               `(defun ,name ,lambda-list                                            
                  ,doc                                                               
                  (unless (listp list)                                               
                    (error 'type-error :datum list :expected-type 'list))            
                  (do ((last list fast)                                              
                       (fast list (cddr fast))                                       
                       (slow (cons (car list) (cdr list)) (cdr slow))                
                       ,@(when step (list step)))                                    
                      (nil)                                                          
                    (declare (dynamic-extent slow) ,@(when declare (list declare))   
                             (ignorable last))                                       
                    (when (safe-endp fast)                                           
                      (return ,ret1))                                                
                    (when (safe-endp (cdr fast))                                     
                      (return ,ret2))                                                
                    (when (eq fast slow)                                             
                      (circular-list-error list))))))                                
  (def proper-list-length (list)                                                   
      "Returns length of LIST, signalling an error if it is not a proper list."      
    (n 1 (+ n 2))                                                                  
    ;; KLUDGE: Most implementations don't actually support lists with bignum       
    ;; elements -- and this is WAY faster on most implementations then declaring   
    ;; N to be an UNSIGNED-BYTE.                                                   
    (fixnum n)                                                                     
    (1- n)                                                                         
    n)                                                                             
  
  (def lastcar (list)                                                              
      "Returns the last element of LIST. Signals a type-error if LIST is not a     
proper list."                                                                      
    nil                                                                            
    nil                                                                            
    (cadr last)                                                                    
    (car fast))                                                                    
  
  (def (setf lastcar) (object list)                                                
      "Sets the last element of LIST. Signals a type-error if LIST is not a proper 
list."                                                                             
    nil                                                                            
    nil                                                                            
    (setf (cadr last) object)                                                      
    (setf (car fast) object)))                                                     

(defun mappend (fn &rest lists)
  (loop for ret in (apply #'mapcar fn lists)
        append ret))

(defun cart (list &rest more-lists)
  "Returns the cartesian product of LIST and MORE-LISTS.

The length of the result is equal to the product of the lengths of all input
lists. A zero-length list anywhere in the input will always return NIL.

The length of each element of the result is equal to the length of the
shortest input list.

Example:

(cart (list 1 2) (list 3 4 5)) ;; ((1 3) (2 3) (1 4) (2 4) (1 5) (2 5))
(cart (list 1 2 3) (list 4 5)) ;; ((1 4) (2 4) (3 4) (1 5) (2 5) (3 5))
(cart (list 1 2 3) nil (list 4 5)) ;; nil
"
  (if more-lists
      (mapcan #'(lambda (y) (mapcar #'(lambda (x) (cons x y)) list)) (apply #'cart more-lists))
      (mapcar #'list list)))

(defun mapcart (function list &rest more-lists)
  "(MAPCAR (LAMBDA (X) (APPLY FUNCTION X)) (APPLY CART LIST MORE-LISTS))

Remember that CART always returns elements with a length equal to the smallest
input list. FUNCTION will need to accept at least as many args as the element
size.

Example:
(mapcart '+ '(1 2 3) '(4 5)) ;; (5 6 7 6 7 8)"
  (mapcar (lambda (args) (apply function args)) (apply #'cart list more-lists)))

(flet ((cart-case-macrofunction (vars cases append)
         (let ((decl (zipsym vars)))
           `(let (,@decl)
              (cond ,@(mapcar #'(lambda (clause) `((and ,@(mapcar #'(lambda (x)
                                                                      (if (consp (second x))
                                                                          `(or ,@(mapcar #'(lambda (u) `(eql ,(first x) (quote ,u))) (second x)))
                                                                          `(eql ,(first x) (quote ,(second x)))))
                                                                  (remove t (zip (mapcar #'car decl) (first clause)) :key #'second))) ,@(cdr clause))) cases)
                    ,@append)))))
  (defmacro cart-case ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases nil))
  (defmacro cart-ecase ((&rest vars) &body cases)
    (cart-case-macrofunction vars cases `((t (error "cart-ecase: Case failure."))))))

(flet ((cart-typecase-fn (vars cases append)
         (let* ((decl (zipsym vars)))
           `(let (,@decl)
              (cond ,@(mapcar #'(lambda (clause)
                                  `((ziprm (and typep) ,(mapcar #'car decl) ,(mapcar #'(lambda (x) `(quote ,x)) (first clause)))
                                    (locally (declare ,@(mapcar #'(lambda (x y) `(type ,x ,y)) (first clause) (mapcar #'car decl))) ,@(cdr clause))))
                              cases)
                    ,@append)))))
  (defmacro cart-typecase (vars &body cases)
    (cart-typecase-fn vars cases nil))
  (defmacro cart-etypecase (vars &body cases)
    (cart-typecase-fn vars cases `((t (error "cart-etypecase: Case failure."))))))

(declaim (inline pairs))
(defun pairs (list)
  "Return a new list containing each pair of elements in LIST."
  (loop for (a . b) on list by #'cddr collect (if b (list a (first b)) (list a))))

(defun maptree-if (predicate transformer tree)
  "Returns a new tree by recursively calling TRANSFORMER on sub-trees which
satisfy the PREDICATE.

predicate : tree -> boolean
transformer: tree -> (or tree atom) *control

If the transformer returns a CONTROL function, then the tree returned by the
transformer is replaced in-turn by the result of:

(funcall CONTROL #'(lambda (x) (maptree-if PREDICATE TRANSFORMER x)) transformed-tree)

otherwise it is left as it is.

Example:
  (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
              #'(λ (x) `(pong ,@(cdr x)))
              '(progn (ping (ping (ping 1)))))
  ;; (PROGN (PONG (PING (PING 1))))
  (maptree-if #'(λ (x) (and (consp x) (eq (car x) 'ping)))
              #'(λ (x) (values `(pong ,@(cdr x)) #'mapcar))
              '(progn (ping (ping (ping 1)))))
  ;; (PROGN (PONG (PONG (PONG 1))))
  "
  (multiple-value-bind (t-tree control) (if (funcall predicate tree)
					    (funcall transformer tree)
					    (values tree #'mapcar))
    (if (and (consp t-tree) control)
	(funcall control #'(lambda (x) (maptree-if predicate transformer x)) t-tree)
	t-tree)))

(defun maptree (keys transformer tree)
  (maptree-if (if (eql keys t)
                  #'(lambda (x) (declare (ignore x)) t)
                  #'(lambda (x) (and (consp x) (member (car x) keys))))
              (if (or (eql keys t) (functionp transformer)) transformer
                  (let ((alist (mapcar #'(lambda (x y) (cons x y)) keys transformer)))
                    #'(lambda (x) (values (cons (cdr (assoc (car x) alist)) (cdr x)) #'mapcar))))  tree))

(defmacro nconsc (var &rest args)
  "Macro to do setf and nconc for destructive list updates. 

If VAR is null then VAR is set to (apply #'nconc ARGS), 

else does (apply #'nconc (cons VAR ARGS)).

Example:
(let ((x nil))
  (nconsc x (list 1 2 3) (list 'a 'b 'c))
  x)
;; (1 2 3 A B C)

(let ((x (list 'a 'b 'c)))
  (nconsc x (list 1 2 3))
   x)
;; (A B C 1 2 3)"
  (assert (and (symbolp var) (not (member var '(t nil)))))
  (if (null args) var
      `(if (null ,var)
	   (progn
	     (setf ,var ,(car args))
	     (nconc ,var ,@(cdr args)))
	   (nconc ,var ,@args))))

;; from serapeum
(declaim (inline firstn))
(defun firstn (n list)
  (loop repeat n for x in list collect x))

;;; cl-bench utils
;; From Hansen's MS thesis.
(defun merge! (a b predicate)
  "Destructively merge two sorted lists given comparison function PREDICATE."
  (labels ((merge-loop (r a b)
             (cond ((funcall predicate (car b) (car a))
                    (setf (cdr r) b)
                    (if (null (cdr b))
                        (setf (cdr b) a)
                        (merge-loop b a (cdr b))))
                   (t ; (car a) <= (car b)
                    (setf (cdr r) a)
                    (if (null (cdr a))
                        (setf (cdr a) b)
                        (merge-loop a (cdr a) b))))))
    (cond ((null a) b)
          ((null b) a)
          ((funcall predicate (car b) (car a))
           (if (null (cdr b))
               (setf (cdr b) a)
               (merge-loop b a (cdr b)))
           b)
          (t                           ; (car a) <= (car b)
           (if (null (cdr a))
               (setf (cdr a) b)
               (merge-loop a (cdr a) b))
           a))))

;; Due to Richard O'Keefe; algorithm attributed to D.H.D. Warren.
(defun sort! (seq predicate)
  "Stable sort which copies the input list SEQ and then sorts the new list
imperatively according to PREDICATE."
  (labels ((astep (n)
             (cond ((> n 2)
                    (let* ((j (truncate n 2))
                           (a (astep j))
                           (k (- n j))
                           (b (astep k)))
                      (merge! a b predicate)))
                   ((= n 2)
                    (let ((x (car seq))
                          (y (cadr seq))
                          (p seq))
                      (setf seq (cddr seq))
                      (when (funcall predicate y x)
                        (setf (car p) y)
                        (setf (cadr p) x))
                      (setf (cddr p) nil)
                      p))
                   ((= n 1)
                    (let ((p seq))
                      (setf seq (cdr seq))
                      (setf (cdr p) nil)
                      p))
                   (t nil))))
    (astep (length seq))))

;; from alexandria
(defun set-equal (list1 list2 &key (test #'eql) (key nil keyp))
  "Returns true if every element of LIST1 matches some element of LIST2 and
every element of LIST2 matches some element of LIST1. Otherwise returns false."
  (let ((keylist1 (if keyp (mapcar key list1) list1))
        (keylist2 (if keyp (mapcar key list2) list2)))
    (and (dolist (elt keylist1 t)
           (or (member elt keylist2 :test test)
               (return nil)))
         (dolist (elt keylist2 t)
           (or (member elt keylist1 :test test)
               (return nil))))))

;;; ALIST
(declaim (inline racons))
(defun racons (key value ralist)
  (acons value key ralist))

(macrolet
    ((define-alist-get (name get-entry get-value-from-entry add doc)
       `(progn
          (declaim (inline ,name))
          (defun ,name (alist key &key (test 'eql))
            ,doc
            (let ((entry (,get-entry key alist :test test)))
              (values (,get-value-from-entry entry) entry)))
          (define-setf-expander ,name (place key &key (test ''eql)
                                                 &environment env)
            (multiple-value-bind
                  (temporary-variables initforms newvals setter getter)
                (get-setf-expansion place env)
              (when (cdr newvals)
                (error "~A cannot store multiple values in one place" ',name))
              (with-gensyms (new-value key-val test-val alist entry)
                (values
                 (append temporary-variables
                         (list alist
                               key-val
                               test-val
                               entry))
                 (append initforms
                         (list getter
                               key
                               test
                               `(,',get-entry ,key-val ,alist :test ,test-val)))
                 `(,new-value)
                 `(cond
                    (,entry
                     (setf (,',get-value-from-entry ,entry) ,new-value))
                    (t
                     (let ,newvals
                       (setf ,(first newvals) (,',add ,key ,new-value ,alist))
                       ,setter
                       ,new-value)))
                 `(,',get-value-from-entry ,entry))))))))
  (define-alist-get assoc-value assoc cdr acons
    "ASSOC-VALUE is an alist accessor very much like ASSOC, but it can
be used with SETF.")
  (define-alist-get rassoc-value rassoc car racons
    "RASSOC-VALUE is an alist accessor very much like RASSOC, but it can
be used with SETF."))

;;; DLIST

;; Simple doubly-linked lists

;; ref: https://github.com/bharath1097/matlisp/blob/94b65e68f2de5208ef9641cd105e25512c36a7f5/src/utilities/dlist.lisp

;; ref: https://github.com/krzysz00/dlist
(defun dcons (obj)
  (let ((lst (list* nil nil obj)))
    (setf (first lst) lst
	  (second lst) lst)
    lst))

(defmacro dpush (obj dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new)
      (error "Can't expand this."))
    (with-gensyms (left right ele ncon)
      (let ((new (car new)))
	`(let* (,@(zip-list dummies vals)
		(,new ,getter)
		(,ncon (dcons ,obj)))
	   (when ,new
	     (destructuring-bind (,left ,right . ,ele) ,new
	       (declare (ignore ,right ,ele))
	       (setf (first ,ncon) ,left
		     (second ,left) ,ncon
		     (second ,ncon) ,new
		     (first ,new) ,ncon)))
	   (setf ,new ,ncon)
	   ,setter)))))

(defmacro dpop (dll &environment env)
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion dll env)
    (when (cdr new)
      (error "Can't expand this."))
    (with-gensyms (left right ele)
      (let ((new (car new)))
	`(let* (,@(zip-list dummies vals)
		(,new ,getter))
	   (when ,new
	     (destructuring-bind (,left ,right . ,ele) ,new
	       (prog1 ,ele
		 ;;update cons cell
		 (setf (first ,new) ,new
		       (second ,new) ,new)
		 ;;update place
		 (if (and (eql ,new ,left) (eql ,new ,right))
		     (setf ,new nil)
		     (setf (second ,left) ,right
			   (first ,right) ,left
			   ,new ,right))
		 ,setter))))))))

(defun dlist (&rest objs)
  (let* ((rev (reverse objs))
	 (ret (dcons (car rev))))
    (loop :for ele :in (cdr rev)
          :do (dpush ele ret))
    ret))

(declaim (inline drdc dcdr dcar))
(defun drdc (buf) (first buf))
(defun dcdr (buf) (second buf))
(defun dcar (buf) (cddr buf))

(defun dappendf (&rest dlsts)
  (let ((dlsts (remove-if #'null dlsts)))
    (loop for se in (cdr dlsts)
          with ft = (car dlsts)
          do (progn
	       (rotatef (first ft) (first se))
	       (rotatef (second (first ft)) (second (first se))))
          finally (return ft))))

;;; Template utils
;; Topological sort (matlisp)
(defun toposort (lst func &optional (test #'eql))
  (multiple-value-bind (nlst len) (loop :for ele :in lst
				        :for i := 0 :then (1+ i)
				        :collect (cons i ele) :into ret
				        :finally (return (values ret (1+ i))))
    (let* ((s nil)
	   (graph (let ((ret (make-array len)))
		    (loop :for (i . ele) :in nlst
		          :do (let ((children (mapcar #'car (remove-if-not #'(lambda (x) (and (not (funcall test (cdr x) ele)) (funcall func (cdr x) ele))) nlst)))
				    (parents (mapcar #'car (remove-if-not #'(lambda (x) (and (not (funcall test (cdr x) ele)) (funcall func ele (cdr x)))) nlst))))
			        (when (null parents)
			          (push i s))
			        (setf (aref ret i) (list ele children parents))))
		    ret))
	   (ordering nil))
      (let ((last-s (last s)))
        (do ((slst s (cdr slst)))
	    ((null slst))
	  (let* ((i (car slst))
	         (children (second (aref graph i))))
	    (mapcar #'(lambda (x)
		        (let ((par (third (aref graph x))))
			  (let ((par (remove i par)))
			    (setf (third (aref graph x)) par)
			    (when (null par)
			      (setf (cdr last-s) (cons x nil)
				    last-s (cdr last-s))))))
		    children)
	    (push i ordering))))
      (mapcar #'(lambda (x) (car (aref graph x))) ordering))))

(defun match-lambda-lists (lsta lstb)
  (let ((optional? nil))
    (labels ((optp? (a b)
	       (if (and (consp a) (atom b)) (optp? b a)
		   (progn
		     (if (or (member a lambda-list-keywords) (not optional?)) nil
			 (if (null (cddr b)) t nil)))))
	     (lst-walker (a b)
	       (cond
		 ((and (atom a) (atom b))
		  (if (eq a b)
		      (progn
			(when (member a lambda-list-keywords)
			  (setq optional? (if (member a '(&optional &key)) t nil)))
			t)
		      (if (or (member a lambda-list-keywords) (member b lambda-list-keywords)) nil t)))
		 ((or (atom a) (atom b))
		  (if (optp? a b) t nil))
		 ((and (consp a) (consp b))
		  (and (lst-walker (car a) (car b))
		       (lst-walker (cdr a) (cdr b)))))))
      (lst-walker lsta lstb))))

(defun remove-from-plist (plist &rest props)
  "Return a new PLIST with all keys in PROPS dropped."
  (loop for (options value) on plist by #'cddr
        append (unless (member options props)
                 (list options value))))

;;; Consify
(defun deconsify (x sym)
  (if (atom x) x
      (loop for ll on x
            collect (deconsify (car ll) sym) into ret
            when (and (cdr ll) (not (consp (cdr ll))))
            collect sym into ret
            and
            collect (deconsify (cdr ll) x) into ret
            finally (return ret))))

(defun reconsify (x sym)
  (if (atom x) x
      (loop for ll on x 
            with right = nil
            if (eql (car ll) sym)
            do (progn (assert (not (caddr ll)) nil "Misformed x")
                      (setf right (cadr ll))
                      (loop-finish))
            else
            collect (reconsify (car ll) sym) into left
            finally 
               (progn
                 (if right (setf (cdr (last left)) right)) 
                 (return left)))))
