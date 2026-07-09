;;; js.lisp --- JS Tests

;; 

;;; Code:
(in-package :syn/tests/lang)
(in-suite :syn)
(in-readtable :std)

(defparameter *js-src*
  #"(function() {
        if (typeof URL === 'undefined' || typeof URLSearchParams === 'undefined') {
            return;
        }
        if (!document.documentElement) {
            return;
        }
        var defaultAtbWeeks = ['*']
        var atbWeeks = typeof $STATS_ATB_WEEKS$ !== "undefined" ? $STATS_ATB_WEEKS$ : defaultAtbWeeks
        document.documentElement.dataset.ntpStatsAtbWeeks = atbWeeks;
    })();"#)

(deftest js-src ()
  (istype 'sb-alien::alien-value (parse-lang-string :javascript *js-src*)))
