;;; lipsum.lisp --- Lorem Ipsum 

;; Lorem Ipsum(s) Generator for Lisp

;;; Commentary:

;; In the simplest case does as you would expect - generates pseudo-random
;; sequences of prepared latin statements.

;; In the future we may extend this to support custom generators based on
;; user-supplied corpus (*lipsum-corpus*) as well as a light protocol for
;; mixing into higher-level objects such as ORGAN:ORG-DOCUMENT.

;;; Code:
(in-package :rt/lipsum)

(defvar *lipsum-corpus*
  '(("Lorem ipsum dolor sit amet, consectetuer adipiscing elit."
     "Donec hendrerit tempor tellus."
     "Donec pretium posuere tellus."
     "Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus."
     "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."
     "Nulla posuere."
     "Donec vitae dolor."
     "Nullam tristique diam non turpis."
     "Cras placerat accumsan nulla."
     "Nullam rutrum."
     "Nam vestibulum accumsan nisl.")

    ("Pellentesque dapibus suscipit ligula."
     "Donec posuere augue in quam."
     "Etiam vel tortor sodales tellus ultricies commodo."
     "Suspendisse potenti."
     "Aenean in sem ac leo mollis blandit."
     "Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi."
     "Phasellus lacus."
     "Etiam laoreet quam sed arcu."
     "Phasellus at dui in ligula mollis ultricies."
     "Integer placerat tristique nisl."
     "Praesent augue."
     "Fusce commodo."
     "Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus."
     "Nullam libero mauris, consequat quis, varius et, dictum id, arcu."
     "Mauris mollis tincidunt felis."
     "Aliquam feugiat tellus ut neque."
     "Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.")

    ("Aliquam erat volutpat."
     "Nunc eleifend leo vitae magna."
     "In id erat non orci commodo lobortis."
     "Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus."
     "Sed diam."
     "Praesent fermentum tempor tellus."
     "Nullam tempus."
     "Mauris ac felis vel velit tristique imperdiet."
     "Donec at pede."
     "Etiam vel neque nec dui dignissim bibendum."
     "Vivamus id enim."
     "Phasellus neque orci, porta a, aliquet quis, semper a, massa."
     "Phasellus purus."
     "Pellentesque tristique imperdiet tortor."
     "Nam euismod tellus id erat.")

    ("Nullam eu ante vel est convallis dignissim."
     "Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio."
     "Nunc porta vulputate tellus."
     "Nunc rutrum turpis sed pede."
     "Sed bibendum."
     "Aliquam posuere."
     "Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio."
     "Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna."
     "Curabitur vulputate vestibulum lorem."
     "Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros."
     "Sed id ligula quis est convallis tempor."
     "Curabitur lacinia pulvinar nibh."
     "Nam a sapien.")))

(defvar *lipsum-paragraph-separator* (make-string 2 :initial-element #\NewLine))
(defvar *lipsum-sentence-separator* " ")
(defvar *lipsum-list-start* "")
(defvar *lipsum-list-item-start* "* ")
(defvar *lipsum-list-item-end* (string #\NewLine))
(defvar *lipsum-list-end* "")

;; sentences
(defun lipsum-sentences (&key (count 1) (corpus *lipsum-corpus*) stream)
  "Return COUNT lorem ipsum sentences from CORPUS as a string or print to STREAM."
  (format 
   stream "~{~A~}"
   (intersperse 
    *lipsum-sentence-separator* 
    (collecting
      (dotimes (i count)
        (collect (random-elt (random-elt corpus))))))))

;; paragraphs
(defun lipsum-paragraphs (&key (count 1) (corpus *lipsum-corpus*) stream)
  "Return COUNT lorem ipsum paragraphs from CORPUS as a string or print to STREAM."
  (format 
   stream "~{~A~}"
   (intersperse
    *lipsum-paragraph-separator*
    (collecting
      (dotimes (i count)
        (collect (apply 'concat (intersperse *lipsum-sentence-separator* (random-elt corpus)))))))))

;; lipsum-list
(defun lipsum-list (&key (count 1) (corpus *lipsum-corpus*) stream)
  "Return COUNT lorem ipsum list items from CORPUS as a string or print to STREAM."
  (format stream "~A~{~A~}~A" 
          *lipsum-list-start*
          (collecting
            (dotimes (i count)
              (collect (concat 
                        *lipsum-list-item-start*
                        (random-elt (random-elt corpus))
                        *lipsum-list-item-end*))))
          *lipsum-list-end*))

;; lipsum
(defun lipsum (type &rest args)
  (apply 
   (ecase type
     (:sentence 'lipsum-sentences)
     (:paragraph 'lipsum-paragraphs)
     ((or :list t) 'lipsum-list))
   args))
