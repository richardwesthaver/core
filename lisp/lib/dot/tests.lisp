(defpackage :dot/tests
  (:use :cl :std :rt :dot))
(in-package :dot/tests)

(defsuite :dot)
(in-suite :dot)

(defmethod graph-object-node ((graph (eql 'example)) (obj cons))
  (make-instance 'node
    :attributes '(:label "cell \\N"
                  :shape :box)))

(defmethod graph-object-points-to ((graph (eql 'example)) (object cons))
  (list (car object)
        (make-instance 'attributed
                       :object (cdr object)
                       :attributes '(:weight 3))))
;; Symbols
(defmethod graph-object-node ((graph (eql 'example)) (object symbol))
  (make-instance 'node
    :attributes `(:label ,object
                  :shape :hexagon
                  :style :filled
                  :color :black
                  :fillcolor "#ccccff")))

(deftest dot ()
  (let* ((test-img "/tmp/dot-test-lr.svg")
         (data '(a b c #1=(b z) c d #1#))
         (dgraph (generate-graph-from-roots 'example (list data)
                                            '(:rankdir "LR" :layout "twopi" :labelloc "t"))))
    (is (null (dot-graph dgraph test-img :format :svg)))
    (is (stringp (print-graph dgraph :stream nil)))
    (is (delete-file test-img))))
