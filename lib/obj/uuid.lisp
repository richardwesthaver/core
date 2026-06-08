;;; obj/uuid.lisp --- UUIDs

;;

;;; Code:
(in-package :obj/uuid)

(defvar *clock-seq* 0
  "Holds the clock sequence. It is set when a version 1 uuid is
generated for the first time and remains unchanged during a whole
session.")

(defvar *node* nil
  "Holds the IEEE 802 MAC address or a random number when such is not
available")

(defvar *ticks-per-count* 1024
  "Holds the amount of ticks per count. The ticks per count determine
the number of possible version 1 uuids created for one time
interval. Common Lisp provides INTERNAL-TIME-UNITS-PER-SECOND which
gives the ticks per count for the current system so *ticks-per-count*
can be set to INTERNAL-TIME-UNITS-PER-SECOND")

(defparameter *uuid-random-state* nil
  "Holds the random state used for generation of random numbers")

(defclass uuid ()
  ((time-low               :initarg  :time-low
			   :type     (unsigned-byte 32)
			   :accessor time-low
			   :initform 0)
   (time-mid               :initarg  :time-mid
			   :type     (unsigned-byte 16)
			   :accessor time-mid
			   :initform 0)
   (time-high-and-version  :initarg  :time-high
			   :type     (unsigned-byte 16)
			   :accessor time-high
			   :initform 0)
   (clock-seq-and-reserved :initarg  :clock-seq-var
			   :type     (unsigned-byte 8)
			   :accessor clock-seq-var
			   :initform 0)
   (clock-seq-low          :initarg  :clock-seq-low
			   :type     (unsigned-byte 8)
			   :accessor clock-seq-low
			   :initform 0)
   (node                   :initarg  :node
			   :type     (unsigned-byte 48)
			   :accessor node
			   :initform 0))
  (:documentation "Lisp representation of a uuid."))

(labels ((parse-block (string start end)
           (parse-integer string :start start :end end :radix 16)))
  (defun uuid-from-string (str)
    "Parse a UUID without hyphen characters (32 chars in length)."
    (make-instance 'uuid
      :time-low (parse-block str 0 8)
      :time-mid (parse-block str 8 12)
      :time-high (parse-block str 12 16)
      :clock-seq-var (parse-block str 16 18)
      :clock-seq-low (parse-block str 18 20)
      :node (parse-block str 20 32)))
  (defun make-uuid-from-string (string)
    "Creates an uuid from the string representation of an uuid. (example input
string 6ba7b810-9dad-11d1-80b4-00c04fd430c8).

This function also accepts strings of length 32 where the hyphen separators
are skipped."
    (if (not (= (length string) 36))
        (if (not (= (length string) 32))
            (error "~@<Could not parse ~S as UUID: string representation ~
has invalid length (~D). A valid UUID string representation has 36 ~
characters.~@:>" string (length string))
            (return-from make-uuid-from-string (uuid-from-string string)))
        (progn
          (unless (and (eq (aref string  8) #\-)
	               (eq (aref string 13) #\-)
	               (eq (aref string 18) #\-)
	               (eq (aref string 23) #\-))
            (error "~@<Could not parse ~S as UUID: positions 8, ~
13, 18, 21 and 23 have to contain ~C (~A) characters.~@:>"
	           string #\- (char-name #\-)))
          (make-instance 'uuid
            :time-low      (parse-block string  0 8)
            :time-mid      (parse-block string  9 13)
            :time-high     (parse-block string 14 18)
            :clock-seq-var (parse-block string 19 21)
            :clock-seq-low (parse-block string 21 23)
            :node          (parse-block string 24 36))))))

(defparameter +namespace-dns+ (make-uuid-from-string "6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  "The DNS Namespace. Can be used for the generation of uuids version 3 and 5")
(defparameter +namespace-url+ (make-uuid-from-string "6ba7b811-9dad-11d1-80b4-00c04fd430c8")
  "The URL Namespace. Can be used for the generation of uuids version 3 and 5")
(defparameter +namespace-oid+ (make-uuid-from-string "6ba7b812-9dad-11d1-80b4-00c04fd430c8")
  "The OID Namespace. Can be used for the generation of uuids version 3 and 5")
(defparameter +namespace-x500+ (make-uuid-from-string "6ba7b814-9dad-11d1-80b4-00c04fd430c8")
  "The x500+ Namespace. Can be used for the generation of uuids version 3 and 5")

(defun get-node-id ()
  "Get MAC address of first ethernet device"
  (let ((node
	 #+linux
	  (let ((interface (first (remove "lo"
					  (mapcan (lambda (x) (last (pathname-directory x)))
						  (directory "/sys/class/net/*/address"))
					  :test #'equal))))
	    (when (not (null interface))
	      (with-open-file (address (make-pathname :directory
                                                      `(:absolute "sys" "class" "net" ,interface)
						      :name "address"))
		(parse-integer (remove #\: (read-line address)) :radix 16))))))
    (unless node
      (unless *uuid-random-state*
	(setf *uuid-random-state* (make-random-state t)))
      (setf node (dpb #b01 (byte 8 0) (random #xffffffffffff *uuid-random-state*))))
    node))

(let ((uuids-this-tick 0)
      (last-time 0))
  (defun get-timestamp ()
    "Get timestamp, compensate nanoseconds intervals"
    (tagbody
     restart
     (let ((time-now (+ (* (get-universal-time) 10000000) 100103040000000000)))
					;10010304000 is time between 1582-10-15 and 1900-01-01 in seconds
       (cond ((not (= last-time time-now))
	      (setf uuids-this-tick 0
		    last-time time-now)
	      (return-from get-timestamp time-now))
	     (T
	      (cond ((< uuids-this-tick *ticks-per-count*)
		     (incf uuids-this-tick)
		     (return-from get-timestamp (+ time-now uuids-this-tick)))
		    (T
		     (sleep 0.0001)
		     (go restart)))))))))

(defun format-v3or5-uuid (hash ver)
  "Helper function to format a version 3 or 5 uuid. Formatting means setting the appropriate version bytes."
  (check-type ver (or (eql 3) (eql 5)) "either 3 or 5.")

  (let ((result (octet-vector-to-uuid (subseq hash 0 16))))
    (setf (time-high result)     (dpb (ecase ver
					(3 #b0011)
					(5 #b0101))
				      (byte 4 12)
				      (logior (ash (aref hash 6) 8)
					      (aref hash 7)))
	  (clock-seq-var result) (dpb #b10 (byte 2 6) (aref hash 8)))
    result))

(defmethod print-object ((id uuid) stream)
  "Prints an uuid in the string represenation of an uuid. (example string 6ba7b810-9dad-11d1-80b4-00c04fd430c8)"
  (format stream "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~12,'0X"
	  (time-low id)
	  (time-mid id)
	  (time-high id)
	  (clock-seq-var id)
	  (clock-seq-low id)
	  (node id)))

(defun print-bytes (stream uuid)
  "Prints the raw bytes in hex form. (example output 6ba7b8109dad11d180b400c04fd430c8)"
  (format stream "~8,'0X~4,'0X~4,'0X~2,'0X~2,'0X~12,'0X"
	  (time-low uuid)
	  (time-mid uuid)
	  (time-high uuid)
	  (clock-seq-var uuid)
	  (clock-seq-low uuid)
	  (node uuid)))

(defun format-as-urn (stream uuid)
  "Prints the uuid as a urn"
   (format stream "urn:uuid:~(~A~)" uuid))

(defun make-null-uuid ()
  "Generates a NULL uuid (i.e 00000000-0000-0000-0000-000000000000)"
  (make-instance 'uuid))

(defun make-v1-uuid ()
  "Generates a version 1 (time-based) uuid."
  (unless *uuid-random-state*
    (setf *uuid-random-state* (make-random-state t)))
  (let ((timestamp (get-timestamp)))
    (when (zerop *clock-seq*)
      (setf *clock-seq* (random 10000 *uuid-random-state*)))
    (unless *node*
      (setf *node* (get-node-id)))
    (make-instance 'uuid
		   :time-low (ldb (byte 32 0) timestamp)
		   :time-mid (ldb (byte 16 32) timestamp)
		   :time-high (dpb #b0001 (byte 4 12) (ldb (byte 12 48) timestamp))
		   :clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 6 8) *clock-seq*))
		   :clock-seq-low (ldb (byte 8 0) *clock-seq*)
		   :node *node*)))

(defun make-v3-uuid (namespace name)
  "Generates a version 3 (named based MD5) uuid."
  (format-v3or5-uuid
   (digest-uuid :md5 (uuid-to-octet-vector namespace) name)
   3))

(defun make-v4-uuid ()
  "Generates a version 4 (random) uuid"
  (unless *uuid-random-state*
    (setf *uuid-random-state* (make-random-state t)))
  (make-instance 'uuid
		 :time-low (random #xffffffff *uuid-random-state*)
		 :time-mid (random #xffff *uuid-random-state*)
		 :time-high (dpb #b0100 (byte 4 12) (ldb (byte 12 0) (random #xffff *uuid-random-state*)))
		 :clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 8 0) (random #xff *uuid-random-state*)))
		 :clock-seq-low (random #xff *uuid-random-state*)
		 :node (random #xffffffffffff *uuid-random-state*)))

(defun make-v5-uuid (namespace name)
  "Generates a version 5 (name based SHA1) uuid."
  (format-v3or5-uuid
   (digest-uuid :sha1 (uuid-to-octet-vector namespace) name)
   5))

(defun uuid= (uuid1 uuid2)
  (or (eq uuid1 uuid2)
      (and (= (time-low uuid1) (time-low uuid2))
	   (= (time-mid uuid1) (time-mid uuid2))
	   (= (time-high uuid1) (time-high uuid2))
	   (= (clock-seq-var uuid1) (clock-seq-var uuid2))
	   (= (clock-seq-low uuid1) (clock-seq-low uuid2))
	   (= (node uuid1)(node uuid2)))))

(defun uuid-to-octet-vector (uuid)
  "Converts an uuid to byte-array"
  (let ((array (make-array 16 :element-type '(unsigned-byte 8))))
    (with-slots (time-low time-mid time-high-and-version clock-seq-and-reserved clock-seq-low node)
		uuid
		(loop for i from 3 downto 0
		      do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) time-low)))
		(loop for i from 5 downto 4
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) time-mid)))
		(loop for i from 7 downto 6
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) time-high-and-version)))
		(setf (aref array 8) (ldb (byte 8 0) clock-seq-and-reserved))
		(setf (aref array 9) (ldb (byte 8 0) clock-seq-low))
		(loop for i from 15 downto 10
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) node)))
    array)))

(defun uuid-to-string (uuid)
  (print-object uuid nil))

(defmacro arr-to-bytes (from to array)
  "Helper macro used in byte-array-to-uuid."
  `(loop for i from ,from to ,to
	 with res = 0
	 do (setf (ldb (byte 8 (* 8 (- ,to i))) res) (aref ,array i))
	 finally (return res)))

(defun octet-vector-to-uuid (array)
  "Converts a byte-array generated with uuid-to-byte-array to an uuid."
   (check-type array
	       (array (unsigned-byte 8) (16))
	       "Provided value is not an one-dimensional array with 16 elements of type (unsigned-byte 8)")
   (make-instance 'uuid
		  :time-low (arr-to-bytes 0 3 array)
		  :time-mid (arr-to-bytes 4 5 array)
		  :time-high (arr-to-bytes 6 7 array)
		  :clock-seq-var (aref array 8)
		  :clock-seq-low (aref array 9)
		  :node (arr-to-bytes 10 15 array)))

(defun digest-uuid (digest uuid name)
  "Helper function that produces a digest from a namespace (a byte array) and a string. Used for the
generation of version 3 and 5 uuids."
  (declare (ignorable digest uuid name))
  ;; FIX 2025-10-10: 
  (let ((digester (symbol-call :ironclad 'make-digest digest)))
    (symbol-call :ironclad 'update-digest digester uuid)
    (symbol-call :ironclad 'update-digest digester (sb-ext:string-to-octets name))
    (symbol-call :ironclad 'produce-digest digester)))
