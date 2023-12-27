;; Inline source blocks match the pattern:

;; src_LANG{BODY} or src_LANG[HEADERS]{BODY}

;;; Code:
(in-package :organ)

(define-org-object inline-source-block (lang headers body))
