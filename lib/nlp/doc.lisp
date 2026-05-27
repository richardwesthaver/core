;;; doc.lisp --- Text Document Analysis

;;; Code:
(in-package :nlp/doc)

;;; Vars
(defvar *stop-words*
  (list "a" "able" "about" "above" "according" "accordingly" "across" "actually" "after"
        "afterwards" "again" "against" "ain't" "all" "allow" "allows" "almost" "alone"
        "along" "already" "also" "although" "always" "am" "among" "amongst" "an" "and"
        "another" "any" "anybody" "anyhow" "anyone" "anything" "anyway" "anyways" "anywhere"
        "apart" "appear" "appreciate" "appropriate" "are" "aren't" "around" "as" "a's" "aside"
        "ask" "asking" "associated" "at" "available" "away" "awfully" "be" "became" "because" "become"
        "becomes" "becoming" "been" "before" "beforehand" "behind" "being" "believe" "below" "beside"
        "besides" "best" "better" "between" "beyond" "both" "brief" "but" "by" "came" "can" "cannot"
        "cant" "can't" "cause" "causes" "certain" "certainly" "changes" "clearly" "c'mon" "co" "com"
        "come" "comes" "concerning" "consequently" "consider" "considering" "contain" "containing"
        "contains" "corresponding" "could" "couldn't" "course" "c's" "currently" "definitely" "described"
        "despite" "did" "didn't" "different" "do" "does" "doesn't" "doing" "don" "done" "don't" "down"
        "downwards" "during" "each" "edu" "eg" "eight" "either" "else" "elsewhere" "enough" "entirely"
        "especially" "et" "etc" "even" "ever" "every" "everybody" "everyone" "everything" "everywhere"
        "ex" "exactly" "example" "except" "far" "few" "fifth" "first" "five" "followed" "following" "follows"
        "for" "former" "formerly" "forth" "four" "from" "further" "furthermore" "get" "gets" "getting" "given"
        "gives" "go" "goes" "going" "gone" "got" "gotten" "greetings" "had" "hadn't" "happens" "hardly" "has"
        "hasn't" "have" "haven't" "having" "he" "he'd" "he'll" "hello" "help" "hence" "her" "here" "hereafter"
        "hereby" "herein" "here's" "hereupon" "hers" "herself" "he's" "hi" "him" "himself" "his" "hither"
        "hopefully" "how" "howbeit" "however" "how's" "i" "i'd" "ie" "if" "ignored" "i'll" "i'm" "immediate"
        "in" "inasmuch" "inc" "indeed" "indicate" "indicated" "indicates" "inner" "insofar" "instead" "into"
        "inward" "is" "isn't" "it" "it'd" "it'll" "its" "it's" "itself" "i've" "just" "keep" "keeps" "kept"
        "know" "known" "knows" "last" "lately" "later" "latter" "latterly" "least" "less" "lest" "let" "let's"
        "like" "liked" "likely" "little" "look" "looking" "looks" "ltd" "mainly" "many" "may" "maybe" "me"
        "mean" "meanwhile" "merely" "might" "more" "moreover" "most" "mostly" "much" "must" "mustn't" "my"
        "myself" "name" "namely" "nd" "near" "nearly" "necessary" "need" "needs" "neither" "never" "nevertheless"
        "new" "next" "nine" "no" "nobody" "non" "none" "noone" "nor" "normally" "not" "nothing" "novel" "now"
        "nowhere" "obviously" "of" "off" "often" "oh" "ok" "okay" "old" "on" "once" "one" "ones" "only" "onto"
        "or" "other" "others" "otherwise" "ought" "our" "ours" "ourselves" "out" "outside" "over" "overall" "own"
        "particular" "particularly" "per" "perhaps" "placed" "please" "plus" "possible" "presumably" "probably"
        "provides" "que" "quite" "qv" "rather" "rd" "re" "really" "reasonably" "regarding" "regardless" "regards"
        "relatively" "respectively" "right" "s" "said" "same" "saw" "say" "saying" "says" "second" "secondly" "see"
        "seeing" "seem" "seemed" "seeming" "seems" "seen" "self" "selves" "sensible" "sent" "serious" "seriously"
        "seven" "several" "shall" "shan't" "she" "she'd" "she'll" "she's" "should" "shouldn't" "since" "six" "so"
        "some" "somebody" "somehow" "someone" "something" "sometime" "sometimes" "somewhat" "somewhere" "soon" "sorry"
        "specified" "specify" "specifying" "still" "sub" "such" "sup" "sure" "t" "take" "taken" "tell" "tends" "th"
        "than" "thank" "thanks" "thanx" "that" "thats" "that's" "the" "their" "theirs" "them" "themselves" "then"
        "thence" "there" "thereafter" "thereby" "therefore" "therein" "theres" "there's" "thereupon" "these" "they"
        "they'd" "they'll" "they're" "they've" "think" "third" "this" "thorough" "thoroughly" "those" "though"
        "three" "through" "throughout" "thru" "thus" "to" "together" "too" "took" "toward" "towards" "tried"
        "tries" "truly" "try" "trying" "t's" "twice" "two" "un" "under" "unfortunately" "unless" "unlikely"
        "until" "unto" "up" "upon" "us" "use" "used" "useful" "uses" "using" "usually" "value" "various" "very"
        "via" "viz" "vs" "want" "wants" "was" "wasn't" "way" "we" "we'd" "welcome" "well" "we'll" "went" "were"
        "we're" "weren't" "we've" "what" "whatever" "what's" "when" "whence" "whenever" "when's" "where"
        "whereafter" "whereas" "whereby" "wherein" "where's" "whereupon" "wherever" "whether" "which" "while"
        "whither" "who" "whoever" "whole" "whom" "who's" "whose" "why" "why's" "will" "willing" "wish" "with"
        "within" "without" "wonder" "won't" "would" "wouldn't" "yes" "yet" "you" "you'd" "you'll" "your"
        "you're" "yours" "yourself" "yourselves" "you've" "zero"))

(defparameter *language-data* 
  (loop with ht = (make-hash-table :test #'equal)
        for stop in *stop-words*
        do (setf (gethash stop ht) t)
        finally (return ht)))

;;; Tokenize
(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t) (alphabetic t))
  "Split a string into a list of words."
  (let* ((tokens (split " " (collapse-whitespaces string)))
         (tokens (if remove-stop-words
                     (delete-if (lambda (x) (gethash (string-downcase  x) *language-data*)) tokens)
                     tokens))
         (tokens (if stem
                     (mapcar #'stem tokens)
                     tokens))
         (tokens (if down-case
                     (mapcar #'string-downcase tokens)
                     tokens))
         (tokens (if alphabetic
                     (delete-if-not (lambda (x) (ppcre:scan "^[A-Za-z]*$" x)) tokens)
                     tokens)))
    tokens))

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  (remove "" (mapcar #'std:trim (ppcre:split "[.!?]" string)) :test #'equal))

;;; Documents
(defclass document ()
  ((source :accessor source :initarg :source
           :documentation "The source object for the document.")
   (string-contents :initarg :string-contents :accessor string-contents)
   (term-count-table :initform (make-hash-table :test #'equal)
                     :documentation "Contains a mapping of term ->
amount of times word appears in the document.")
   (vector-data :accessor vector-data
                :documentation "Vector representation of the document.")
   (rank :accessor rank :documentation "Rank used for sorting.")
   (tokens :accessor tokens)
   (token-count :accessor token-count))
  (:documentation 
   "The document class represents a search-optimized document. 

After creating a document, you can perform several operations on it, some
examples:

+ term count: how many times does a term appear in a document?
+ term frequency: how many times does a term appear divided by the
  total number of words in the document?"))

(defclass document-collection ()
  ((documents :initform () :initarg :documents :accessor documents))
  (:documentation "The document collection class represents a
collection of documents. As with a document, there are several operations
available, some examples:

+ dictionary: which words appear in the document collection?
+ keywords: what are the important keywords in this document
  collection?"))

(defmethod initialize-instance :after ((document document) &key)
  (setf (tokens document) (word-tokenize (string-contents document)))
  (setf (token-count document) (length (tokens document)))
  (loop for token in (tokens document) do
    (incf (gethash token (slot-value document 'term-count-table) 0))))

(defmethod term-count ((document document) term)
  (gethash term (slot-value document 'term-count-table) 0))

(defmethod term-frequency ((document document) term)
  "How often does the word exist in the document?"
  (/ (term-count document term)
     ;; prevent division by zero for malformed documents
     (max 1 (token-count document))))

(defmethod termp ((document document) term)
  "Does the term exist in the document?"
  (> (term-count document term) 0))

(defmethod add-document ((document-collection document-collection) document)
  "Add a document to the document collection."
  (push document (documents document-collection)))

(defun match-term (term)
  (lambda (document)
    (termp document term)))

(defmethod document-frequency ((document-collection document-collection) term)
  (/ (count-if (match-term term) (documents document-collection))
     (length (documents document-collection))))

(defmethod inverse-document-frequency ((document-collection document-collection) term)
  (log (/ (length (documents document-collection))
          (count-if (match-term term) (documents document-collection)))))

(defmethod term-frequency-inverse-document-frequency ((document document)
                                                      (document-collection document-collection)
                                                      term)
  (* (term-frequency document term) (inverse-document-frequency document-collection term)))

(defmethod dictionary ((document document))
  "Return a list of all of the words that appear in a document."
  (loop for key being the hash-keys of (slot-value document 'term-count-table)
        collect key))

(defmethod dictionary ((document-collection document-collection))
  "Return a list of all of the words that appear in a document collection."
  (let ((words (list)))
    (loop for document in (documents document-collection)
          do (appendf words (tokens document)))
    (remove-duplicates words :test #'equalp)))

(defmethod keywords ((document document) &optional document-collection)
  (if document-collection
      (sort (loop for word in (dictionary document)
                  collect (cons word (term-frequency-inverse-document-frequency
                                      document document-collection word)))
            #'>
            :key #'rest)
      (sort (loop for word in (dictionary document)
                  collect (cons word (term-frequency document word)))
            #'>
            :key #'rest)))

(defun extract-keywords (text &key (limit 5))
  "Extract keywords from a string of text."
  (take limit (keywords (make-instance 'document
                                       :string-contents text))))

;;; Doc Vector
(defmethod word-count-vectorize ((document document) dictionary)
  "Transform a document into a vector using word counts."
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index) (term-count document word)))
    (setf (vector-data document) vector-data)))

(defmethod tf-idf-vectorize ((document document) (collection document-collection) dictionary)
  "Transform a document into a vector using tf-idf.
Definition: tf-idf: term frequency, inverse document frequency. How
often does a term a appear in a document as compared to all other
documents?"
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index)
                   (term-frequency-inverse-document-frequency document collection word)))
    (setf (vector-data document) vector-data)))

(defmethod tf-vectorize ((document document) dictionary)
  "Transform a document into a vector using tf.
Definition: tf: term frequency. How often does a term appear in a
document?"
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index)
                   (term-frequency document word)))
    (setf (vector-data document) vector-data)))

(defmethod vectorize-documents ((document-collection document-collection) operation)
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (funcall operation document dictionary))))

(defmethod word-count-vectorize-documents ((document-collection document-collection))
  (vectorize-documents document-collection #'word-count-vectorize))

(defmethod tf-vectorize-documents ((document-collection document-collection))
  "Definition: tf: term frequency. How often does a term appear in a
document?"
  (vectorize-documents document-collection #'tf-vectorize))

(defmethod tf-idf-vectorize-documents ((document-collection document-collection))
  "Definition: tf-idf: term frequency, inverse document frequency. How
often does a term appear in a document as compared to all other
documents?"
  (vectorize-documents document-collection (lambda (document dictionary)
                                             (tf-idf-vectorize document document-collection dictionary))))

;;; textrank

;; based on https://web.eecs.umich.edu/~mihalcea/papers/mihalcea.emnlp04.pdf
(defclass document-vertex (document ast:node)
  ((edges :accessor edges :initform (make-hash-table)
          :documentation "The keys of the hash table represent the
          edges, the values of the hash table represent the edge
          weights."))
  (:documentation "The document vertex class represents a document
that is part of a graph. The edges slot of the document vertex class
is used to store edges of that particular vertex. The keys in the
edges slot hash table are the actual vertexes, and the values are the
edge weights."))

(defmethod cosine-similarity ((document-a document) (document-b document))
  "Calculate the cosine similarity between two vectors."
  (flet ((vector-product (document-a document-b)
           (loop for a across (vector-data document-a)
                 for b across (vector-data document-b)
                 sum (* a b)))
         (vector-sum-root (document)
           (sqrt (loop for i across (vector-data document)
                       sum (* i i))))
         (vector-zero-p (document)
           (every #'zerop (vector-data document))))
    (if (or (vector-zero-p document-a) (vector-zero-p document-b))
        0 ; if either vector is completely zero, they are dissimilar
        (/ (vector-product document-a document-b)
           (* (vector-sum-root document-a) (vector-sum-root document-b))))))

(defmethod generate-document-similarity-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (cosine-similarity document-a document-b))))))

(defmethod textrank ((collection document-collection) &key (epsilon 0.001)
                                                            (damping 0.85)
                                                            (initial-rank)
                                                            (iteration-limit 100))
  "This method is used to calculate the text rankings for a document
   collection. The `epsilon' is the maximum delta for a given node
   rank change during an iteration to be considered convergent. The
   `damping' is a factor utilized to normalize the data. The
   `initial-rank' is the rank given to nodes before any
   iterations. The `iteration-limit' is the amount of times the
   algorithm may traverse the graph before giving up (if the algorithm
   does not converge)."
  (with-accessors ((documents documents)) collection
    (unless (zerop (length documents))
      (labels ((set-initial-rank ()
                 "Set the initial rank of all documents to a supplied
                value OR 1/length of the documents."
                 (let ((initial-rank (or initial-rank (/ 1 (length documents)))))
                   (mapcar (lambda (document) (setf (rank document) initial-rank)) documents)))
               (graph-neighbors (document)
                 "Return a list of neighbors. In a fully connected graph,
                all nodes are a neighbor except for the node itself."
                 (remove document documents))
               (graph-neighbor-edge-sum (document)
                 "Add up the edges of all neighbors of a given node."
                 (let ((sum (- (reduce #'+ (hash-table-values (edges document))) 1)))
                   (if (> sum 0) sum 1)))
               (document-similarity (document-a document-b)
                 (gethash document-b (edges document-a) 0))
               (convergedp (previous-score current-score)
                 "Check if a delta qualifies for convergence."
                 (<=  (abs (- previous-score current-score)) epsilon))
               (calculate-rank (document)
                 "Calculate the rank of a document."
                 (loop for neighbor in (graph-neighbors document)
                       sum (/ (* damping (rank neighbor) (document-similarity document neighbor))
                              (graph-neighbor-edge-sum neighbor)))))
        (set-initial-rank)
        (loop with converged = nil
              for iteration from 0 to iteration-limit until converged
              do (setf converged t)
                 (loop for document in documents
                       for old-rank = (rank document)
                       for new-rank = (calculate-rank document)
                       do (setf (rank document) new-rank)
                       unless (convergedp old-rank new-rank)
                       do (setf converged nil)))))))

(defun summarize-text (text &key (summary-length 3) (show-rank-p nil))
  (let ((collection (make-instance 'document-collection)))
    (loop for sentence in (sentence-tokenize text)
          do (add-document collection
                           (make-instance 'document-vertex
                                          :string-contents sentence)))
    (tf-idf-vectorize-documents collection)
    (generate-document-similarity-vectors collection)
    (textrank collection :iteration-limit 100)
    (take summary-length
                   (mapcar (if show-rank-p
                               (lambda (i) (cons (rank i) (string-contents i)))
                               #'string-contents)
                           (sort (documents collection) #'> :key #'rank)))))

;;; dbscan

;; Density-based spacial clustering of applications with noise (DBSCAN)
(defclass document-cluster (document-vertex)
  ((cluster :accessor cluster :initform :noise)
   (neighbors :accessor neighbors))
  (:documentation "The document cluster class represents a document
that is part of a graph which will be clustered. It extends the
documenet-vertex class and adds support for a cluster tag and a list
of neighbors. These slots are useful for clustering algorithms."))

(defmethod clusters ((collection document-collection))
  "Return a list of clusters. Each hash key represents a cluster, and
   the hash value is the list of elements in that cluster.

   Please note: this function is not responsible for computing the
   clusters, only for returning the list of pre-tagged documents in
   cluster lists."
  (let ((result (make-hash-table)))
    (loop for document in (documents collection)
          do (push document (gethash (cluster document) result (list))))
    result))

(defun get-cluster (cluster-label points)
  "Return all matching points for a given cluster label."
  (remove-if-not (lambda (i) (eq (cluster i) cluster-label)) points))

(defmethod distance ((vector-1 t) (vector-2 t))
  "Return the Euclidean distance between two vectors."
  (sqrt (loop for i across vector-1
              for j across vector-2
              sum (expt (- i j) 2))))

(defmethod distance ((document-a document-cluster) (document-b document-cluster))
  (distance (vector-data document-a) (vector-data document-b)))

(defmethod generate-document-distance-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (distance document-a document-b))))))

(defmethod dbscan ((collection document-collection) &key (minimum-points 3)
                                                         (epsilon 0.5))
  "Minimum points refers to the minimum amount of points that must
   exist in the neighborhood of a point for it to be considered a
   core-point in a cluster. Epsilon refers to the distance between
   two points for them to be considered neighbors."
  (labels ((range-query (document)
             "Return all points that have a distance less than epsilon."
             (loop for vertex being the hash-keys of (edges document)
                   when (and (<= (gethash vertex (edges document)) epsilon)
                             (not (eq vertex document)))
                   collect vertex))
           (core-point-p (point)
             "Is a point a core-point?"
             (<= minimum-points (length (range-query point))))
           (cluster-match-p (point cluster)
             "Check if a core point belongs to a cluster."
             (intersection cluster (range-query point))))
    ;;; identify core points
    (let* ((core-points (remove-if-not #'core-point-p (documents collection)))
           (non-core-points (set-difference (documents collection) core-points)))
      ;;; assign labels to core points
      (loop for point in core-points
            with cluster-count = 0
            do (loop named cluster-set
                     for i from 0 to cluster-count
                     ;; point found cluster match, setf and break
                     when (cluster-match-p point (get-cluster i core-points))
                     do (setf (cluster point) i)
                        (return-from cluster-set)
                     ;; point found no cluster-match, create new cluster
                     finally (setf (cluster point) (incf cluster-count))))
      ;;; assign labels to non-core points
      (loop for point in non-core-points
            for intersection = (intersection core-points (range-query point))
            when intersection
            do (setf (cluster point) (cluster (first intersection)))))))

;;; Section
(defun extract-sections (text &key (epsilon 0.5))
  "Extract the sections from a string of text. Epsilon refers to the
   distance between two points for them to be considered related."
  (labels ((average-distance (point points)
             (/ (reduce #'+ points
                        :key (lambda (i) (distance (vector-data i)
                                                   (vector-data point))))
                (length points))))
    (let ((collection (make-instance 'document-collection)))
      (loop for sentence in (sentence-tokenize text)
            do (add-document collection
                             (make-instance 'document-cluster
                                            :string-contents sentence)))
      (tf-vectorize-documents collection)
      (loop for document in (documents collection)
            with cluster-index = 0
            for cluster = (get-cluster cluster-index (documents collection))
            do (if (and cluster (>= epsilon (average-distance document cluster)))
                   (setf (cluster document) cluster-index)
                   (setf (cluster document) (incf cluster-index))))
      collection)))

;;; String Metrics
;; mk-string-metrics—library of efficient implementations of various string
;; metric algorithms.

;; Copyright © 2014–2018 Mark Karpov

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
(defun hamming (x y)
  "Calculate Hamming distance between two given strings X and Y, they have
to be of the same length."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (let ((result 0))
    (declare (type array-index result))
    (dotimes (i (length x) result)
      (declare (type array-index i))
      (unless (char= (char x i)
             (char y i))
    (incf result)))))

(defun levenshtein (x y)
  "Calculate Levenshtein distance between two given strings X and Y."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (let* ((x-len (length x))
     (y-len (length y))
     (v0 (make-array (1+ y-len) :element-type 'array-index))
     (v1 (make-array (1+ y-len) :element-type 'array-index)))
    (declare (type (simple-array array-index) v0 v1))
    (dotimes (i (1+ y-len))
      (declare (type array-index i))
      (setf (aref v0 i) i))
    (dotimes (i x-len (aref v0 y-len))
      (declare (type array-index i))
      (setf (aref v1 0) (1+ i))
      (dotimes (j y-len)
    (declare (type array-index j))
    (setf (aref v1 (1+ j))
          (min (1+ (aref v1 j))
           (1+ (aref v0 (1+ j)))
           (+  (aref v0 j)
               (if (char= (char x i)
                  (char y j))
               0 1)))))
      (rotatef v0 v1))))

(defun damerau-levenshtein (x y)
  "Calculate Damerau-Levenshtein distance between two given strings X and
Y."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (let* ((x-len (length x))
     (y-len (length y))
     (v0 (make-array (1+ y-len) :element-type 'array-index))
     (v1 (make-array (1+ y-len) :element-type 'array-index))
     (v* (make-array (1+ y-len) :element-type 'array-index)))
    (declare (type (simple-array array-index) v0 v1 v*))
    (dotimes (i (1+ y-len))
      (declare (type array-index i))
      (setf (aref v0 i) i))
    (dotimes (i x-len (aref v0 y-len))
      (declare (type array-index i))
      (setf (aref v1 0) (1+ i))
      (dotimes (j y-len)
    (declare (type array-index j))
    (let* ((x-i (char x i))
           (y-j (char y j))
           (cost (if (char= x-i y-j) 0 1)))
      (declare (type array-index cost))
      (setf (aref v1 (1+ j))
        (min (1+ (aref v1 j))
             (1+ (aref v0 (1+ j)))
             (+  (aref v0 j) cost)))
      (when (and (plusp i)
             (plusp j))
        (let ((x-i-1 (char x (1- i)))
          (yj-1 (char y (1- j)))
          (val (+ (aref v* (1- j)) cost)))
          (declare (type array-index val))
          (when (and (char= x-i yj-1)
             (char= x-i-1 y-j)
             (< val (aref v1 (1+ j))))
        (setf (aref v1 (1+ j)) val))))))
      (rotatef v* v0 v1))))

(defun norm-levenshtein (x y)
  "Return normalized Levenshtein distance between X and Y. Result is a real
number from 0 to 1, where 0 signifies no similarity between the strings,
while 1 means exact match."
  (let ((r (levenshtein x y)))
    (if (zerop r)
    1
    (- 1 (/ r
        (max (length x)
             (length y)))))))

(defun norm-damerau-levenshtein (x y)
  "Return normalized Damerau-Levenshtein distance between X and Y. Result is
a real number from 0 to 1, where 0 signifies no similarity between the
strings, while 1 means exact match."
  (let ((r (damerau-levenshtein x y)))
    (if (zerop r)
    1
    (- 1 (/ r
        (max (length x)
             (length y)))))))

(defun string-to-set (str)
  "Convert string STR into a set. This function is supposed to be inlined."
  (declare (type (simple-array character) str)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (let ((result (make-hash-table)))
    (dotimes (i (length str))
      (let ((ch (char str i)))
    (if (gethash ch result)
        (incf (the array-index (gethash ch result)))
        (setf (gethash ch result) 1))))
    result))

(defun intersection-length (x y)
  "Returns length of intersection of two strings X and Y. This function is
supposed to be inlined."
  (let ((result 0))
    (declare (type array-index result)
         (optimize (safety 0) (speed 3) (space 3)))
    (maphash (lambda (key x-val)
           (declare (type array-index x-val))
           (let ((y-val (gethash key y)))
         (declare (type (or array-index null) y-val))
         (when y-val
           (incf result (min x-val y-val)))))
         x)
    result))

(defun union-length (x y)
  "Returns length of union of two strings X and Y. This function is supposed
to be inlined."
  (let ((temp (make-hash-table))
    (result 0))
    (declare (type array-index result)
         (optimize (safety 0) (speed 3) (space 3)))
    (flet ((extract (h)
         (maphash (lambda (key val)
            (declare (type array-index val))
            (let ((t-val (gethash key temp)))
              (declare (type (or array-index null) t-val))
              (setf (gethash key temp)
                (if t-val
                    (max val t-val)
                    val))))
              h)))
      (extract x)
      (extract y)
      (maphash (lambda (key val)
         (declare (ignore key)
              (type array-index val))
         (incf result val))
           temp)
      result)))

(defun overlap (x y)
  "This function calculates overlap coefficient between two given strings X
and Y. Returned value is in range from 0 (no similarity) to 1 (exact
match)."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (/ (the array-index (intersection-length (string-to-set x)
                       (string-to-set y)))
     (min (length x)
      (length y))))

(defun jaccard (x y)
  "Calculate Jaccard similarity coefficient for two strings X and
Y. Returned value is in range from 0 (no similarity) to 1 (exact match)."
  (declare (type (simple-array character) x y)
       (optimize (safety 0) (speed 3) (space 3)))
  (let ((x (string-to-set x))
    (y (string-to-set y)))
    (if (and (zerop (hash-table-count x))
         (zerop (hash-table-count y)))
    1
    (/ (the array-index (intersection-length x y))
       (the array-index (union-length x y))))))

(defun fast-find (char str str-len &optional (start 0))
  "Check if CHAR is in STR. This function is supposed to be inlined."
  (declare (type character char)
       (type (simple-array character) str)
       (type array-index str-len start)
       (optimize (safety 0) (speed 3) (space 3)))
  (do ((i start (1+ i)))
      ((>= i str-len))
    (declare (type array-index i))
    (when (char= char (char str i))
      (return-from fast-find i))))

(defun jaro (x y)
  "Calculate Jaro distance between two strings X and Y. Returned value is in
range from 0 (no similarity) to 1 (exact match)."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 1) (space 3)))
  (let* ((x-len (length x))
     (y-len (length y))
     (d (if (and (>= x-len 2)
             (>= y-len 2))
        (- (floor (max x-len y-len) 2) 1)
        0))
     (m 0)
     (p 0)
     (pj 0))
    (declare (type array-index d m p pj))
    (dotimes (i x-len)
      (declare (type array-index i))
      (let ((ch (char x i)))
    (do ((j (fast-find ch y y-len 0)
        (fast-find ch y y-len (1+ j)))
         done)
        ((or (null j) done))
      (declare (type (or array-index null) j))
      (when (and j (<= (the array-index (abs (- i j)))
               d))
        (when (and (plusp pj)
               (< j pj))
          (incf p))
        (setf pj   j
          done t)
        (incf m)))))
    (if (zerop m)
    0
    (/ (+ (/ m x-len)
          (/ m y-len)
          (/ (- m p) m))
       3))))

(defun prefix-length (x y)
  "Calculate length of common prefix for strings X and Y."
  (declare (type (simple-array character) x y)
       (inline length)
       (optimize (safety 0) (speed 3) (space 3)))
  (let ((x-len (length x))
    (y-len (length y))
    (result 0))
    (declare (type array-index result))
    (dotimes (i x-len)
      (if (and (< i y-len)
           (char= (char x i)
              (char y i)))
      (incf result)
      (return-from prefix-length result)))
    result))

(defun jaro-winkler (x y)
  "Calculate Jaro-Winkler distance between two strings X and Y. Returned
value is in range from 0 (no similarity) to 1 (exact match)."
  (let ((jd (jaro x y))
    (l  (prefix-length x y)))
    (+ jd (* l 1/10 (- 1 jd)))))
