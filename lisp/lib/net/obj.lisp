(in-package :net/core)

(deftype port () "Port number" '(integer 0 65535))
(deftype unprivileged-port () "Unprivileged port number" '(or (port 1024 65535) (port 0)))
(deftype privileged-port () "Privileged port number" '(port 1 1023))
(deftype ip-address () "IP Address specifier" '(or string (vector unsigned-byte) list))
(deftype socket-address () "A complete internet socket address specifier." '(cons ip-address port))

(defclass transport ()
  ())

(defclass connection ()
  ())

(defclass codec ()
  ())

(defclass protocol ()
  ())

(defclass client ()
  ())

(defclass server ()
  ())

(defclass peer ()
  ())

(defclass proxy ()
  ())

(defclass tunnel ()
  ())

(defgeneric connect (self &key &allow-other-keys))

(defgeneric disconnect (self &key &allow-other-keys))
