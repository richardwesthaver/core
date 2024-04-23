;;; std/seq.lisp --- Standard Sequences

;;

;;; Code:
(in-package :std/seq)

(defun take (n seq)
  "Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied.

If N is negative, then |N| elements are taken (in their original
order) from the end of SEQ."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (declare (type signed-array-length n))
  (seq-dispatch seq
    (if (minusp n)
        (last seq (abs n))
        (firstn n seq))
    (if (minusp n)
        (subseq seq (max 0 (+ (length seq) n)))
        (subseq seq 0 (min n (length seq))))))
