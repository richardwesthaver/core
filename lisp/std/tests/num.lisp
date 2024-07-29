;;; num.lisp --- Number Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest leb128 ()
  (loop for i from 0 below 1000
        do (is (= i (decode-uleb128 (encode-uleb128 i)))))
  (signals division-by-zero (decode-uleb128 (encode-uleb128 -1)))
  (loop for i from -1000 below 0
        do (is (= i (decode-leb128 (encode-leb128 i))))
        do (is (= (* i i) (decode-leb128 (encode-leb128 (* i i)))))))
