;;; yaml.lisp --- YAML serialization

;; YAML Ain't Markup Language

;;; Commentary:

;; My least favorite markup format, but common enough to warrant building some
;; support for it.

;; The use-cases we have in mind are: 

;; - Kubernetes :: https://kubernetes.io/docs/concepts/overview/working-with-objects/

;; - git*-ci :: https://docs.gitlab.com/ci/yaml/

;; ref: https://github.com/dtolnay/serde-yaml

;; ref: https://yaml.org/spec/1.2.2/

;;; Code:
(in-package :dat/yaml)

(defun yaml-decode (string &key (start 0) end)
  "Convert a YAML string into a Lisp object."
  (with-input-from-string (stream string :start start :end end)
    (values (yaml-read stream)
            (file-position stream))))

(defmethod deserialize ((obj string) (format (eql :yaml)) &key (start 0) end)
  (declare (ignore format))
  (yaml-decode obj :start start :end end))

(defmethod deserialize ((obj pathname) (format (eql :yaml)) &key (start 0) end)
  (declare (ignore format))
  (with-open-file (f obj)
    (yaml-decode obj :start start :end end)))

(defun yaml-read (stream &optional (eof-error-p t) eof-value)
  "Read a YAML object from a stream."
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)
      ;; ...
      (t (std:nyi!)))))
