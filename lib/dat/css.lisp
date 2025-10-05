;;; css.lisp --- Cascading Style Sheets

;; https://www.w3.org/Style/CSS/

;;; Commentary:

;; for a list of all properties refer to:
;; https://www.w3.org/Style/CSS/all-properties.en.html

;; for other web data: https://github.com/mdn/data/tree/main

;; ref: https://github.com/inaimathi/cl-css

;;; Code:
(in-package :dat/css)

;; SHEET     ::= (BLOCK*)
;; BLOCK     ::= (:BLOCK SELECTOR PROPERTY*)
;; SELECTOR  ::= (string*)
;; PROPERTY  ::= (:PROPERTY string string)

;;; Vars
(defvar *minify-css* nil
  "When non-nil, CSS output is minified.")
(defvar *css-indent* nil
  "When non-nil, indicates the number of spaces to use for indentation.")

(color:make-palette :css
  :black "000000"
  :silver "c0c0c0"
  :gray "808080"
  :white "ffffff"
  :maroon "800000"
  :red "ff0000"
  :purple "800080"
  :fuchsia "ff00ff"
  :magenta "ff00ff"
  :green "008000"
  :lime "00ff00"
  :olive "808000"
  :yellow "ffff00"
  :navy "000080"
  :blue "0000ff"
  :teal "008080"
  :aqua "00ffff"
  :cyan "00ffff"
  :orange "ffa500"
  :aliceblue "f0f8ff"
  :antiquewhite "faebd7"
  :aquamarine "7fffd4"
  :azure "f0ffff"
  :beige "f5f5dc"
  :bisque "ffe4c4"
  :blanchedalmond "ffebcd"
  :blueviolet "8a2be2"
  :brown "a52a2a"
  :burlywood "deb887"
  :cadetblue "5f9ea0"
  :chartreuse "7fff00"
  :chocolate "d2691e"
  :coral "ff7f50"
  :cornflowerblue "6495ed"
  :cornsilk "fff8dc"
  :crimson "dc143c"
  :darkblue "00008b"
  :darkcyan "008b8b"
  :darkgoldenrod "b8860b"
  :darkgray "a9a9a9"
  :darkgreen "006400"
  :darkgrey "a9a9a9"
  :darkkhaki "bdb76b"
  :darkmagenta "8b008b"
  :darkolivegreen "556b2f"
  :darkorange "ff8c00"
  :darkorchid "9932cc"
  :darkred "8b0000"
  :darksalmon "e9967a"
  :darkseagreen "8fbc8f"
  :darkslateblue "483d8b"
  :darkslategray "2f4f4f"
  :darkslategrey "2f4f4f"
  :darkturquoise "00ced1"
  :darkviolet "9400d3"
  :deeppink "ff1493"
  :deepskyblue "00bfff"
  :dimgray "696969"
  :dimgrey "696969"
  :dodgerblue "1e90ff"
  :firebrick "b22222"
  :floralwhite "fffaf0"
  :forestgreen "228b22"
  :gainsboro "dcdcdc"
  :ghostwhite "f8f8ff"
  :gold "ffd700"
  :goldenrod "daa520"
  :greenyellow "adff2f"
  :grey "808080"
  :honeydew "f0fff0"
  :hotpink "ff69b4"
  :indianred "cd5c5c"
  :indigo "4b0082"
  :ivory "fffff0"
  :khaki "f0e68c"
  :lavender "e6e6fa"
  :lavenderblush "fff0f5"
  :lawngreen "7cfc00"
  :lemonchiffon "fffacd"
  :lightblue "add8e6"
  :lightcoral "f08080"
  :lightcyan "e0ffff"
  :lightgoldenrodyellow "fafad2"
  :lightgray "d3d3d3"
  :lightgreen "90ee90"
  :lightgrey "d3d3d3"
  :lightpink "ffb6c1"
  :lightsalmon "ffa07a"
  :lightseagreen "20b2aa"
  :lightskyblue "87cefa"
  :lightslategray "778899"
  :lightslategrey "778899"
  :lightsteelblue "b0c4de"
  :lightyellow "ffffe0"
  :limegreen "32cd32"
  :linen "faf0e6"
  :mediumaquamarine "66cdaa"
  :mediumblue "0000cd"
  :mediumorchid "ba55d3"
  :mediumpurple "9370db"
  :mediumseagreen "3cb371"
  :mediumslateblue "7b68ee"
  :mediumspringgreen "00fa9a"
  :mediumturquoise "48d1cc"
  :mediumvioletred "c71585"
  :midnightblue "191970"
  :mintcream "f5fffa"
  :mistyrose "ffe4e1"
  :moccasin "ffe4b5"
  :navajowhite "ffdead"
  :oldlace "fdf5e6"
  :olivedrab "6b8e23"
  :orangered "ff4500"
  :orchid "da70d6"
  :palegoldenrod "eee8aa"
  :palegreen "98fb98"
  :paleturquoise "afeeee"
  :palevioletred "db7093"
  :papayawhip "ffefd5"
  :peachpuff "ffdab9"
  :peru "cd853f"
  :pink "ffc0cb"
  :plum "dda0dd"
  :powderblue "b0e0e6"
  :rosybrown "bc8f8f"
  :royalblue "4169e1"
  :saddlebrown "8b4513"
  :salmon "fa8072"
  :sandybrown "f4a460"
  :seagreen "2e8b57"
  :seashell "fff5ee"
  :sienna "a0522d"
  :skyblue "87ceeb"
  :slateblue "6a5acd"
  :slategray "708090"
  :slategrey "708090"
  :snow "fffafa"
  :springgreen "00ff7f"
  :steelblue "4682b4"
  :tan "d2b48c"
  :thistle "d8bfd8"
  :tomato "ff6347"
  :turquoise "40e0d0"
  :violet "ee82ee"
  :wheat "f5deb3"
  :whitesmoke "f5f5f5"
  :yellowgreen "9acd32"
  :rebeccapurple "663399")

;;; Utils
(defun %-or-word (v) 
  (etypecase v
    (number (concatenate 'string (write-to-string v) "%"))
    (null nil)
    (symbol (symbol-name v))
    (string v)))

(defmacro split-directive (directive-name value &optional (prefix-list '(-ms- -o- -webkit- -moz-)))
  (with-gensyms (val)
    `(let ((,val ,value)) 
       (list ,directive-name ,val
	     ,@(loop 
		 for p in prefix-list
		 collect (keywordicate p directive-name)
		 collect val)))))

;; unit helpers
(defun px (val) (format nil "~apx" val))
(defun % (val) (format nil "~a%" val))
(defun em (val) (format nil "~aem" val))
(defun ch (val) (format nil "~ach" val))

;;; transform
(defun transform-origin (x y &optional z)
  "Takes x, y, z percentages, returns a cross-browser CSS3 transform-origin directive"
  (split-directive :transform (apply #'format nil "~a ~a~@[ ~a~]" (mapcar #'%-or-word (list x y z)))))

(defun rotate (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-directive :transform (format nil "rotate(~adeg)" degrees)))

(defun scale (scale-x &optional (scale-y scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-directive :transform (format nil "scale(~a,~a)" scale-x scale-y)))

(defun skew (x-deg y-deg)
  (split-directive :transform (format nil "skew(~adeg, ~adeg)" x-deg y-deg)))

(defun translate (x y &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-directive :transform (format nil "translate(~a~a, ~a~a)" x units y units)))

(defun matrix (&rest 6-numbers)
  "Takes six numbers and uses them to build a CSS3 transformation matrix directive"
  (split-directive :transform (format nil "matrix(~{~a~^,~})" 6-numbers)))

;;; 3d-transform
(defun perspective (n)
  (split-directive :perspective n (-webkit-)))

(defun perspective-origin (x y)
  (split-directive :perspective-origin 
      (concatenate 'string (%-or-word x) " " (%-or-word y)) (-webkit-)))

(defun backface-visibility (visible/hidden)
  (split-directive :backface-visibility visible/hidden (-webkit- -moz-)))

(defun transform-style (flat/preserve-3d)
  (split-directive :transform-style flat/preserve-3d (-webkit-)))

(defun matrix3d (&rest 16-numbers)
  (split-directive :transform (format nil "matrix3d(~{~a~^,~})" 16-numbers) (-webkit- -moz-)))

(defun translate3d (x y z &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-directive :transform 
      (format nil "translate3d(~a~a, ~a~a, ~a~a)" x units y units z units) (-webkit- -moz-)))

(defun scale3d (scale-x &optional (scale-y scale-x) (scale-z scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-directive :transform (format nil "scale3d(~a,~a,~a)" scale-x scale-y scale-z) (-webkit- -moz-)))

(defun rotate3d (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-directive :transform (format nil "rotate3d(~adeg)" degrees) (-webkit- -moz-)))

;;; animations/transitions
(defun keyframes (animation-name &rest keyframes)
  (flet ((sel (browser-type) 
	   (list (format nil "@~@[~(~a~)~]keyframes ~a" browser-type (format-selector animation-name)) 
		 keyframes)))
    `(,(sel nil) ,(sel :-moz-) ,(sel :-webkit-))))

(defun animation (name &key (duration 0) (timing-function :linear) (delay 0) (iteration-count 1) (direction :normal) (play-state :running))
  (split-directive 
      :animation 
      (format nil "~a ~as ~a ~as ~a ~a ~a"
	      name duration timing-function delay iteration-count direction play-state)
      (-webkit- -moz-)))

(defun transition (property &key (duration 0) (timing-function :ease) (delay 0))
  (split-directive
      :transition
      (format nil "~a ~as ~a ~as" property duration timing-function delay)
      (-webkit- -moz- -o-)))

;;; format
(defun format-selector (s)
  (if (stringp s) s (string-downcase s)))

(defun format-declaration-value (v)
  (typecase v
    (null v)
    (string v)
    (list (format-declaration-value (apply (car v) (cdr v))))
    (function (funcall v))
    (symbol (string-downcase v))
    (color (print-hex-rgb v))
    (t (format nil "~A" v))))

(defun format-declaration (k v)
  (etypecase v
    (null (format-declarations-list k))
    (number (format nil "~(~A~): ~A;" k v))
    (symbol (concatenate 'string (string-downcase k) ": " (string-downcase v) ";"))
    (string (format nil "~(~A~): ~S;" k v))
    (vector (format 
             nil "~(~A~): ~{~A~^, ~};"
             k (mapcar 'format-declaration-value (coerce v 'list))))
    (list 
     (if (fboundp (car v))
         (format nil "~(~A~): ~A;" 
                 k 
                 (format-declaration-value (apply (car v) (cdr v))))
         (concatenate 'string (string-downcase k) " { " (format-declarations-list v) "}")))))

(defun format-declarations-list (list-of-declarations)
  (apply #'concatenate 
	 'string 
	 (loop with remaining = list-of-declarations
	       for head = (pop remaining) 
	       if (consp head) collect (format-rule (car head) (cdr head))
	       else if head collect (format-declaration head (pop remaining))
	       collect " "
	       while remaining)))

(defun format-rule (selector declarations)
  (concatenate 'string (format-selector selector) 
	       " { " (format-declarations-list declarations) "}"))

;;; generator
(defun inline-css (rule) (format-declarations-list rule))

(defun css (rules)
  (apply #'concatenate 
	 'string 
	 (loop for r in rules
	       collect (format-rule (car r) (cdr r))
	       collect (list #\Newline))))

(defun compile-css (file-path directives)
  (ensure-directories-exist file-path)
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (format stream (css directives))))

(definline compile-css-file (input output)
  (dat/css:compile-css output (read-lisp-file input)))
