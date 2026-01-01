;;; dfs.lisp --- Graph Loop Path

;; 

;;; Code:
(in-package :obj/tensor)

;; FIX 2025-12-20: 
(defun loop-graph-iteration-path (v data-type prep-phrases)
  (declare (ignore data-type))
  (destructuring-bind (%g &optional root order color p visited-array) prep-phrases
    (let ((g (cadr %g)))
      (print prep-phrases)
      (binding-gensyms (gm gf)
        (let* ((order (or order :dfs)) 
               (colorp color) 
               (color (or color (gf 'color))))
          ;;(visited (ecase order (:sfd (gf 'visited)) ((:dfs :bfs) color)))
          (check-type color symbol) (check-type v symbol)
          (let* ((pushor `(letv* ((,(gm l) ,(gm r) (fence ,(gm g) (the index-type ,v)) :type index-type index-type))
                            ;; REVIEW 2025-12-20: 
                            (loop for ,(gm u) across (δ-i ,(gm g)) for ,(gm switch) from ,(gm l) below ,(gm r)
                                  finally (return ,(gm switch)))
                            (unless (aref ,(gm visited) ,(gm u)) (setf ,(gm switch) t)
                                    ,@(let ((up (if (and p (not (eql order :sfd))) `(cons ,(gm u) ,v) (gm u))))
                                        (ecase order ((:sfd :dfs) `((push ,up ,(gm stack)))) (:bfs `((setf ,(gm stack) (dcdr (dpush ,up ,(gm stack)))))))))))
                 (poppor `(loop repeat (dimensions ,(gm g) 1)
                                unless ,(gm stack) do (go sb-loop::end-loop)
                                do (letv* ((,@(if (and p (not (eql order :sfd))) 
                                                  `((,(gm v) . ,(gm p))) `(,(gm v)))
                                             ,(ecase order 
                                                ((:dfs :sfd) `(pop ,(gm stack))) 
                                                (:bfs `(dpop ,(gm stack)))) 
                                            :type ,@(if (and p (not (eql order :sfd))) 
                                                        `((index-type . (or index-type null))) 
                                                        `(index-type))))
                                     (unless (aref ,(gm visited) ,(gm v)) 
                                       (return (setf ,v ,(gm v) (aref ,(gm visited) ,(gm v)) t
                                                     ,@(if (and p (not (eql order :sfd)))
                                                           `(,p ,(gm p)))))))))
                 (path-findor `(loop repeat (dimensions ,(gm g) 1)
                                     unless ,pushor do (return)
                                     do (push ,v ,(gm path))
                                     unless ,poppor do (return))))
            #+nil
            `(progn
               (with ,(gm g) = (the graph-accessor ,g)) (with ,v = (the index-type ,(or root `(random (dimensions ,(gm g) 1)))))
               (repeat (dimensions ,g 1))
               (with ,(gm visited) = (let ((,(gm visited) ,(or visited-array `(make-array (dimensions ,(gm g) 1) :element-type 'boolean :initial-element nil))))
                                       (setf (aref ,(gm visited) ,v) t) ,(gm visited)))
               ,@(if colorp `((with ,color = (make-array (dimensions ,(gm g) 1) :element-type 'boolean :initial-element nil))))
               (with ,(gm stack) = nil)
               ,@(if (and p (not (eql order :sfd))) `((with ,p = nil)))
               ,@(if (eql order :sfd)
                     `(,@(if p `((with ,p = nil)))
                       (with ,(gm path) = nil)
                       (initially ,path-findor ,@(if colorp `((setf (aref ,color ,v) t))) ,@(if p `((setf ,p (car ,(gm path))))))))
               (declare (type graph-accessor ,(gm g))
                        (type (simple-array boolean (*)) ,(gm visited) ,@(if (and colorp (eql order :sfd)) `(,color))))
               (after-each
                ,@(if (not (eql order :sfd)) `(,pushor (unless ,poppor (finish)) ,@(if colorp `((setf (aref ,color ,v) t))) )
                      `((unless ,(gm path) (finish))
                        (if ,poppor
                            (if (δ-i ,(gm g) (or (first ,(gm path)) 0) ,v)
                                ,path-findor
                                (progn (push ,v ,(gm stack)) (setf (aref ,(gm visited) ,v) nil ,v (pop ,(gm path)))))
                            (setf ,v (pop ,(gm path))))
                        ,@(if p `((setf ,p (car ,(gm path)))))))
                ,@(if colorp `((setf (aref ,color ,v) t)))))
            #+nil
            (push `(progn
                     )
                  (sb-loop::wrappers sb-loop::*loop*))
            `(((,(gm g) ,g graph-accessor)
               (,v ,(or root `(random (dimensions ,(gm g) 1))) index-type)
               ;; repeat (dimensions ,g 1)
               (,(gm visited) (let ((,(gm visited) ,(or visited-array `(make-array (dimensions ,(gm g) 1) :element-type 'boolean :initial-element nil))))
                                (setf (aref ,(gm visited) ,v) t) ,(gm visited)))
               ,@(when colorp `((,color (make-array (dimensions ,(gm g) 1) :element-type 'boolean :initial-element nil))))
               (,(gm stack) nil)
               ,@(when (and p (not (eql order :sfd))) `((,p nil)))
               ,@(when (eql order :sfd)
                   `(,@(when p `((,p nil)))
                     (,(gm path) nil))))
              ()
              #+nil
              `(,@(when (eql order :sfd)
                    ,path-findor 
                    ,@(when colorp `((setf (aref ,color ,v) t)))
                    ,@(when p `((setf ,p (car ,(gm path)))))))
              ()
              ()
              ;; ,@(if (not (eql order :sfd)) 
              ;;       `(,pushor (unless ,poppor (finish)) ,@(if colorp `((setf (aref ,color ,v) t))) )
              ;;       `((unless ,(gm path) (finish))
              ;;         (if ,poppor
              ;;             (if (δ-i ,(gm g) (or (first ,(gm path)) 0) ,v)
              ;;                 ,path-findor
              ;;                 (progn (push ,v ,(gm stack)) (setf (aref ,(gm visited) ,v) nil ,v (pop ,(gm path)))))
              ;;             (setf ,v (pop ,(gm path))))
              ;;         ,@(if p `((setf ,p (car ,(gm path)))))))
              ;; ,@(when colorp `((setf (aref ,color ,v) t)))
              (not ,(gm path))
              ())))))))

        
(sb-loop::add-loop-path '(gidx graph-idx graph-index gindex) 'loop-graph-iteration-path *loop-ansi-universe*
                        :preposition-groups '((:of :in :across) (:from) (:in-order) (:with-color) (:with-parent) (:with-visited))
                        :inclusive-permitted nil)

;; (print (macroexpand-all '(loop for i being the gidx of (zeros '(10 10) (tensor 'single-float 'graph-tensor)))))
