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

(deftest parse-numbers ()
  (is= 12000 (parse-number "12e3"))
  (is= 8 (parse-number "#o10"))
  (is= (parse-number "#2r11") 3)
  (is= (parse-number "#10r3.2") 3.2)
  (signals invalid-number (parse-positive-real-number "-100"))
  (is= 100 (parse-positive-real-number "100"))
  (destructuring-bind (val . len) (std/num::parse-integer-and-places "1280" 0 4)
    (is= val 1280)
    (is= len 4)))
