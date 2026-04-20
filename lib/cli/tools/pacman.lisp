;;; pacman.lisp --- Arch Linux Packaging Tools

;;

;;; Commentary:

;; 

;;; Code:
(in-package :cli/tools/pacman)

(define-cli-tool :pacman (&rest args)
  (let ((proc (sb-ext:run-program *pacman* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (pacman-error "Pacman command failed: ~A ~A" *pacman* (sb-ext:process-error proc)))))

(defun pacman-upgrade ()
  (run-pacman "-Sy" "archlinux-keyring")
  (run-pacman "-Su"))

(defconfig pacman-config (ini:ini-document) 
  (options core extra))

(defmethod load-ast ((self pacman-config))
  (with-slots (options core extra ast) self
    (loop for x = (pop ast)
          until (null x)
          do (string-case ((name x) :default (return (error "Unrecognized pacman config section")))
               ("options" (setf options (cdr (ast x))))
               ("core" (setf core (cdr (ast x))))
               ("extra" (setf extra (cdr (ast x))))))
    self))

(defmethod deserialize (self (format (eql :pacman-config)) &key)
  (change-class (deserialize self :ini) 'pacman-config))

(defun load-pacman-config (&optional (path #p"/etc/pacman.conf"))
  (load-ast (deserialize path :pacman-config)))

(defmethod find-config ((self (eql :pacman)) &key (path #p"/etc/pacman.conf"))
  (load-pacman-config path))

(define-cli-tool :mkarchroot (&rest args)
  (let ((proc (sb-ext:run-program *mkarchroot* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (makepkg-error "MKARCHROOT command failed: ~A ~A" *mkarchroot* (sb-ext:process-error proc)))))

(define-cli-tool :makepkg (&rest args)
  (let ((proc (sb-ext:run-program *makepkg* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (makepkg-error "Pacman command failed: ~A ~A" *makepkg* (sb-ext:process-error proc)))))

;; TODO 2026-04-19: makepkg-template

(defconfig makepkg-config (ast) ())

(defun read-makepkg-string (stream &optional c)
  (let ((c (or c (peek-char t stream nil))))
    (if (char= c #\") 
        (read stream)
        (let ((e (read-char stream)))
          (concatenate
           'string
           (loop with c 
                 do (setf c (read-char stream))
                 until (char= c e)
                 collect c))))))

(defun read-makepkg-array (stream)
  (read-char stream nil) ;; (
  (let ((c (peek-char t stream nil))
        ret)
    (loop while (and c (not (char= c #\))))
          if (char= #\# c)
          do (progn (read-line stream nil) (setf c (peek-char t stream nil)))
          else if (whitespace-p c)
          do (progn (read-char stream nil) (setf c (peek-char t stream nil)))
          else if (or (char= c #\') (char= c #\"))
          do (progn (push (read-makepkg-string stream) ret) (setf c (peek-char t stream nil)))
          else do (push
                   (concatenate 'string
                                (loop while (and c (not (char= c #\))) (not (whitespace-p c)))
                                      collect (read-char stream nil)
                                      do (setf c (peek-char nil stream nil))))
                   ret))
    ;; and do (setf c (peek-char t stream nil))
    (read-char stream nil)
    (nreverse ret)))

(defun read-makepkg-value (stream)
  "Read a makepkg.conf value from STREAM which should be either a bash array or string."
  (skip-makepkg-junk stream)
  (let ((c (peek-char t stream nil)))
    (case c
      ((or #\" #\') (read-makepkg-string stream c))
      (#\( (read-makepkg-array stream)))))

(defun skip-makepkg-junk (stream)
  (when-let ((c (peek-char t stream nil)))
    (loop until (or (not c) (not (or (char= c #\#) (whitespace-p c))))
          do (read-line stream nil)
          do (setf c (peek-char t stream nil)))))

(defun read-makepkg-pair (stream)
  "Read a key/value pair from an makepkg.conf STREAM. Return the result as a cons."
  (skip-makepkg-junk stream)
  (when (peek-char t stream nil)
    (when-let ((k (loop with n
                        do (setf n (read-char stream))
                        until (char= n #\=)
                        collect n)))
      (cons (print (intern (substitute #\- #\_ (concatenate 'string k)))) (read-makepkg-value stream)))))

(defun load-makepkg-config (&optional (path #p"/etc/makepkg.conf"))
  (let ((ast (with-open-file (f path)
               (loop for l = (read-makepkg-pair f)
                     while l 
                     collect l))))
    (make-instance 'makepkg-config :ast ast)))

(defmethod find-config ((self (eql :makepkg)) &key (path #p"/etc/makepkg.conf"))
  (load-makepkg-config path))
