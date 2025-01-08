;;; keyring.lisp --- Keyrings

;; 

;;; Commentary:

;; Current goal is to provide high-level API for linux kernel keyrings
;; (keyutils/keyctl)

;;; Code:
(in-package :cry/keyring)

(defclass keyring (id) ((keys :initform nil :initarg :keys :accessor keys)))

(defmethod initialize-instance ((self keyring) &key id)
  (let ((id (keyctl-get-keyring-id id 1)))
    (if (minusp id)
        (error "Unable to create keyring with ID: ~A" (id self))
        (setf (id self) id))))

(defgeneric make-keyring (self)
  (:method ((self number))
    (make-instance 'keyring :id self))
  (:method ((self (eql :user)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :thread)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :user-session)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :session)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :group)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :process)))
    (make-instance 'keyring :id (key-spec self)))
  (:method ((self (eql :reqkey-auth)))
    (make-instance 'keyring :id (key-spec self))))

(defvar *keyring-payload-size* 32)
(defvar *keyring-key-types* '("big_val" "user" "logon" "keyring"))
(defvar *keyring-key-type* "user")

(defmethods get-key 
  (((self keyring) (key t) &key (size *keyring-payload-size*))
   (get-key self (id key) :size size))
  (((self keyring) (key number) &key (size *keyring-payload-size*))
   (with-alien ((buf (* char) (make-alien char size)))
     (keyctl-read key buf size)
     (clone-octets-from-alien buf (make-octets size)))))

(defmethods put-key 
  (((self keyring) (key number) (val kv))
   (add-key "user" (kv-key val) (kv-val val) (length (kv-val val)) (id self)))
  (((self keyring) (key t) (val string))
   (add-key "user" (id key) val (length val) (id self)))
  (((self keyring) (key t) (val vector))
   (add-key "user" (id key) (octets-to-alien val) (length val) (id self))))

(defmethods delete-key
  (((self keyring) (key number) &key)
   (keyctl-invalidate key))
  (((self keyring) (key t) &key)
   (keyctl-invalidate (id key))))

(defun clear-keys (kr)
  (keyutils::keyctl-clear (id kr)))
