;;; kbd.lisp --- Keyboard-based IO

;; Keyboard-like devices and input

;;; Commentary:

;; refs:

;; - https://www.kernel.org/doc/Documentation/input/event-codes.txt

;; - https://github.com/xkbcommon/libxkbcommon/blob/master/tools/interactive-evdev.c

;; - https://gitlab.freedesktop.org/libevdev/libevdev/-/tree/master/tools

;;; Code:
(in-package :io/kbd)

(defun load-kbd-libs ()
  (load-xkbcommon)
  (load-evdev))

;;; Vars
(defconstant +evdev-offset+ 8)
(defconstant +long-bit+ (sb-alien:alien-size sb-alien:unsigned-long))
(defparameter *keyboards* nil)
(defvar *keysym-sets* nil
  "Alist of (NAME FIRST LAST).")
(defvar *character-keysym-table* (make-hash-table :test 'eql)
  "Table mapping Characters to KEYSYMs.")
(defvar *keysym-name-table* (make-hash-table))
(defvar *name-keysym-table* (make-hash-table :test #'equal))
(defvar *dead-keysym-name-table* (make-hash-table))

;;; Conditions
(define-condition kbd-error (error) ())
(deferror simple-kbd-error (simple-error kbd-error) () (:auto t))
;;; Keysym
(deftype keysym () '(unsigned-byte 32))

(definline keysym-name (code)
  (with-alien ((str (* unsigned-char) (make-alien unsigned-char 11)))
    (xkb::xkb-keysym-get-name code str 11)
    (cast str c-string)))

(definline keysym-code-name (code)
  (gethash code *keysym-name-table*))

(definline keysym-name-code (name)
  (gethash name *name-keysym-table*))

(defun load-xkb-keysyms (&rest codes)
  "Retrieve and map the names of the keysyms CODES which are all integers. Returns a table of INT->STRING."
  (dolist (c codes (values *keysym-name-table* *name-keysym-table*))
    (declare (fixnum c))
    (lety ((n (keysym-name c) :type string))
      (if (and (> (length n) 5) (string= "dead_" (subseq n 0 5)))
          (setf (gethash c *dead-keysym-name-table*) (subseq n 5))
          (setf (gethash c *keysym-name-table*) n
                (gethash n *name-keysym-table*) c)))))

(defun load-xkb-keysyms-file (file)
  (apply 'load-xkb-keysyms (read-lisp-file file)))

(defun keysym-set (name)
  (cdr (assoc name *keysym-sets*)))

(defun (setf keysym-set) (cons name)
  (destructuring-bind (first last) cons
    (declare (keyword name)
             (keysym first last))
    (when (> first last)
      (rotatef first last))
    (setq *keysym-sets* (delete name *keysym-sets* :key #'car))
    (dolist (set *keysym-sets*)
      (let ((first (second set))
            (last (third set)))
        (when (or (<= first first last)
                  (<= first last last))
          (error "Keysym range overlaps existing set ~s" set))))
    (push (cons name cons) *keysym-sets*)))

(defun keysym-set-name (code)
  "Return the character code set name of keysym."
  (declare (keysym code))
  (dolist (set *keysym-sets*)
    (let ((first (second set))
          (last (third set)))
      (when (<= first code last)
        (return (first set))))))

(defun keysym (key &rest bytes)
  "Build a 32-bit keysym. KEY is an integer or character and BYTES optionally
fill in the lower bytes."
  (declare (dynamic-extent bytes))
    (etypecase key
      (keysym
       (dolist (b bytes key) (setq key (+ (ash key 8) b))))
      (character
       (or (gethash key *character-keysym-table*)
           (error "~s isn't a keysym" key)))))

(defmacro define-keysym (obj keysym &key lower translate modifiers mask))

;;; Objects
(defstruct keyboard 
  path 
  (sap nil :type (or null (alien (* libevdev)))) ;; device
  (state nil)
  (compose-state nil)
  (keymap nil))

(defaccessor sap ((self keyboard)) (keyboard-sap self))

(defun evdev-bit-p (array bit)
  "Array elements should be unsigned-long."
  (let ((idx (/ bit +long-bit+)))
    ;; the literal 1 here is 1LL in C - there is potential to overflow a
    ;; singled long.
    (logand (aref array idx) (ash 1 (mod bit +long-bit+)))))

(defun new-device-from-path (path)
  ;; opening FD may fail if the user does not have read permissions. When
  ;; ERROR is non-nil (the default) this signals an error, else we return nil.
  (with-fd (fd path :flags sb-posix:o-rdonly :close nil)
    (sb-alien:with-alien ((dev (* evdev::libevdev)))
      (let ((ret (evdev:libevdev-new-from-fd fd (sb-alien:addr dev))))
        (if (minusp ret)
            (simple-kbd-error (sb-unix::strerror (abs ret)))
            dev)))))

;; (keysym-name 400) ; "0x00000190"
;; evdev::+ev-cnt+ evdev::+key-cnt+

(defun keyboard-device-p (path)
  "Read some input on device at PATH returning T if it appears to be a keyboard
device."
  (with-open-file (st path :element-type 'octet)
    (let ((evbits (make-array evdev::+ev-cnt+))
          (keybits (make-array evdev::+key-cnt+)))
      ;; (sb-posix:ioctl (fd path)
      (read-sequence evbits st)
      (read-sequence keybits st)
      ;; (cons evbits keybits)
      (loop for i from evdev::+key-reserved+ upto evdev::+key-min-interesting+
            when (evdev-bit-p keybits i)
            return t))))
      
(defun make-keyboard-from-dev (dev &rest args)
  "Return a KEYBOARD given a device, keymap, and compose table. Keyword argument
ERROR when non-nil (the default) causes an error to be signaled if the device
can't be opened, else returns nil."
  (apply 'make-keyboard :sap dev args))

(defun get-keyboards (&optional (dir "/dev/input/"))
  (let ((devices (directory-files dir))
        ret)
    (dolist (dev devices ret)
      (handler-case
          (progn
            (print-device-input-info dev)
            (push (make-keyboard-from-dev (new-device-from-path dev) :path dev)
                  ret))
        (sb-posix:syscall-error () nil)
        (simple-kbd-error () nil)))))

;; (xkb::xkb-consumed-mode :xkb)

(defun print-device-input-info (path)
  (when-let ((dev (new-device-from-path path)))
    (pprint 
     (list (evdev::libevdev-get-name dev) 
           (evdev::libevdev-get-id-bustype dev) 
           (evdev::libevdev-get-id-vendor dev)
           (evdev::libevdev-get-id-product dev)))))

(defun device-read-event (dev)
  (declare (optimize (speed 3) (safety 0)))
  (with-alien ((ev evdev/input:input-event))
    (when (evdev::libevdev-has-event-pending dev)
      (println "has event pending")
      (evdev::libevdev-next-event dev (libevdev-read-flag :normal) (addr ev)))
    (with-alien-slots ((* time) type (code evdev/input::code) (value evdev/input::value)) ev
      (values 
       (sb-posix::alien-timeval-sec time) 
       (the fixnum (* 1000 (sb-posix::alien-timeval-usec time)))
       (evdev::libevdev-event-type-get-name type) 
       (evdev::libevdev-event-code-get-name type code) 
       value))))

(defun device-read-events (dev count)
  (let (ret)
    (dotimes (i count ret)
      (multiple-value-bind (s ms type code val) (device-read-event dev)
        (push (list type code val (cons s ms)) ret)))))
        
(defmethod init ((self (eql :kbd)) &key (directory "/dev/input/"))
  (load-kbd-libs)
  (when directory (setq *keyboards* (get-keyboards directory))))
