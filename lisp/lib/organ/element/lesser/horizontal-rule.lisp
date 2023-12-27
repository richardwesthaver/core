(in-package :organ)

(define-org-element horizontal-rule () :lesser t)

(defvar *org-horizontal-rule-min* 5
  "Minimum number of #\- chars in sequence which will be parsed as ORG-HORIZONTAL-RULE.")

(defvar *org-horizontal-rule-char* #\-)

(define-org-parser (horizontal-rule :from string)
  (unless (< (length input) *org-horizontal-rule-min*)
    (with-input-from-string (in input)
      (catch 'bail
        (loop repeat *org-horizontal-rule-min*
              for c = (read-char in nil :eof)
              when (not (char-equal c *org-horizontal-rule-char*))
                do (throw 'bail nil))
        horizontal-rule))))
