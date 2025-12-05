;;; dfs.lisp --- Graph Loop Path

;; 

;;; Code:
(in-package :obj/tensor)

(defun loop-graph-iteration-path (v data-type prep-phrases)
  (declare (ignore data-type))
  (destructuring-bind (g &optional root order color p visited-array) prep-phrases
    (binding-gensyms (gm gf)
      (let* ((order (or order :dfs)) 
             (colorp color) 
             (color (or color (gf 'color))))
        ;;(visited (ecase order (:sfd (gf 'visited)) ((:dfs :bfs) color)))
        (check-type color symbol) (check-type v symbol)
        (let* ((pushor `(letv* ((,(gm l) ,(gm r) (fence ,(gm g) (the index-type ,v)) :type index-type index-type))
                          (loop for ,(gm u) across (δ-i ,(gm g)) from ,(gm l) below ,(gm r) with ,(gm switch) = nil 
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
                                   unless ,pushor return
                                   do (push ,v ,(gm path))
                                   unless ,poppor return)))
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
          '(()
            ()
            ()
            ()
            ()
            ()))))))

        
  
