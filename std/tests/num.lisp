;;; num.lisp --- Number Tests

;; 

;;; Code:
(in-package :std/tests)
(in-suite :std)

(deftest leb128 ()
  (loop for i in (iota 100 :step 100)
        do (is (= i (decode-uleb128 (encode-uleb128 i)))))
  (signals division-by-zero (decode-uleb128 (encode-uleb128 -1)))
  (loop for i in (iota 100 :start -1000 :step 1000)
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

(deftest floats ()
  (let ((f1 (float 1/3)) (f2 0.1))
    (is= f1 (decode-float32 (encode-float32 f1)))
    (is= f1 (decode-float64 (encode-float64 f1)))
    (is= f2 (decode-float32 (encode-float32 f2)))
    (is= f2 (decode-float64 (encode-float64 f2)))))

