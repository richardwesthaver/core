;;; lib/organ/element/greater/item.lisp --- Org Item Elements

;; Items match the pattern: 'BULLET COUNTER-SET CHECK-BOX TAG CONTENTS'

;; examples
#|
- item
3. [@3] set to three
+ [-] tag :: item contents
 * item, note whitespace in front
* not an item, but heading - heading takes precedence
|#

;;; Code:
(in-package :organ)
