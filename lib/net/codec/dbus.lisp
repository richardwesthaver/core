;;; dbus.lisp --- DBUS Codec

;; DBUS Codec Definitions

;;; Code:
(in-package :net/codec/dbus)

;;; Conditions
(define-condition dbus-error (error)
  ())

(define-condition dbus-auth-error (dbus-error)
  ((command :initarg :command)
   (argument :initarg :argument))
  (:report (lambda (condition stream)
             (format stream "Authentication error, command ~S with argument ~S."
                     (slot-value condition 'command)
                     (slot-value condition 'argument)))))

(define-condition dbus-method-error (dbus-error)
  ((arguments :initarg :arguments))
  (:report (lambda (condition stream)
             (format stream "Method error: ~S."
                     (let ((all-args (slot-value condition 'arguments))
                           (first-arg (first (slot-value condition 'arguments))))
                       (if (stringp first-arg)
                           first-arg
                           all-args))))))

;;; DBUS IO
;; May just want to DEFINE-IO here
#+nil
(defclass dbus-type ()
  ((name :initarg :name :reader name)
   (signature :initarg :signature)
   (sigexp-formatter :initarg :sigexp-formatter)
   (signature-parser :initarg :signature-parser)
   (alignment :initarg :alignment)
   (ser :initarg :ser :reader ser)
   (de :initarg :de :reader de)
   (checker :initarg :checker)))

#+nil
(defclass dbus-type-table ()
  ((by-name :initform (make-hash-table))
   (by-signature :initform (make-hash-table))))
