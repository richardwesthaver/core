(in-package :skel/core/types)

(deftype vc-designator () `(member :hg :git list))

;; ref: https://spdx.org/licenses/
(deftype license-designator () `(or string pathname (member :mpl2 :wtfpl :lgpg :llgpl :gpl :mit :mit0)))

(deftype script-designator () '(member :bin :sh :bash :zsh :nu :lisp :python))

(deftype document-designator () '(member :org :txt :pdf :html :md))

(deftype stack-slot-kind () '(member :shell :lisp :comment :var :rule :directive :nop))

(deftype contact-designator () '(or string (cons string string)))
