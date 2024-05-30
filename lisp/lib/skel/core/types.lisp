(in-package :skel/core/types)

(deftype vc-designator () `(member :hg :git list))

;; ref: https://spdx.org/licenses/
(deftype license-designator () `(or string pathname (member :mpl2 :wtfpl :lgpg :llgpl :gpl :mit :mit0)))

(deftype script-designator () '(member :bin :sh :bash :zsh :nu :lisp :python))
