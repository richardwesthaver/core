;;; uring/uring.lisp --- top-level interface

;;

;;; Code:
(in-package :uring)

;; (defconstant +io-syscall-setup+ nr-io-uring-setup) ;425
;; (defconstant +io-syscall-register+ nr-io-uring-register) ;426
;; (defconstant +io-syscall-enter+ nr-io-uring-enter) ;427

(defstruct io-memory-map
  (sq-mmap nil :type mmapped-region)
  (sqe-mmap nil :type mmapped-region)
  (cq-mmap nil :type mmapped-region))

(defun parse-io-uring-params (params)
  "Parse IO-URING-PARAMS foreign struct, return an IO-PARAMS struct."
  (let ((res 0))
    (loop while params
          do (1+ res))
    res))

(defstruct io-params
  (sq-entries 0 :type fixnum)
  (cq-entries 0 :type fixnum)
  (flags 0 :type fixnum)
  (sq-thread-cpu 0 :type fixnum)
  (sq-thread-idle 0 :type fixnum)
  (features 0 :type fixnum)
  (wq-fd 0 :type fixnum)
  ;; resv
  (sq-off (make-submission-queue-offsets) :type submission-queue-offsets) ;; offsets are 40bytes each
  (cq-off (make-completion-queue-offsets) :type completion-queue-offsets))

(defmacro define-io-param-flag (name const)
  "Create a predicate method with NAME which checks for presence of flag
CONST in FLAGS slot of IO-PARAMS. A SETF expansion is also defined
which accepts a boolean value and automatically adjusts the slot."
  `(progn
     (defmethod ,name ((self io-params))
       (not (= 0 (logand (io-params-flags self) ,const))))
     (defmethod (setf ,name) (val (self io-params))
       (with-slots (flags) self
         (setf flags (logior flags ,const))))))

(defmacro define-io-param-feature (name const)
  "Create a predicate method with NAME which checks for presence of flag
CONST in FEATURES slot of IO-PARAMS. A SETF expansion is also defined
which accepts a boolean value and automatically adjust the slot."
  `(progn
     (defmethod ,name ((self io-params))
       (not (= 0 (logand (io-params-features self) ,const))))
     (defmethod (setf ,name) (val (self io-params))
       (with-slots (features) self
         (setf features (logior features ,const))))))

(define-io-param-flag setup-sqpoll-p ioring-setup-sqpoll)
(define-io-param-flag setup-iopoll-p ioring-setup-iopoll)
(define-io-param-flag setup-single-issuer-p ioring-setup-single-issuer)
(define-io-param-feature feat-single-mmap-p ioring-feat-single-mmap)
(define-io-param-feature feat-nodrop-p ioring-feat-nodrop)
(define-io-param-feature feat-submit-stable-p ioring-feat-submit-stable)
(define-io-param-feature feat-rw-cur-pos-p ioring-feat-rw-cur-pos)
(define-io-param-feature feat-cur-personality-p ioring-feat-cur-personality)
(define-io-param-feature feat-fast-poll-p ioring-feat-fast-poll)
(define-io-param-feature feat-poll-32bits-p ioring-feat-poll-32bits)
(define-io-param-feature feat-sqpoll-nonfixed-p ioring-feat-sqpoll-nonfixed)
(define-io-param-feature feat-ext-arg-p ioring-feat-ext-arg)
(define-io-param-feature feat-native-workers-p ioring-feat-native-workers)
(define-io-param-feature feat-rsrc-tags-p ioring-feat-rsrc-tags)
(define-io-param-feature feat-cqe-skip-p ioring-feat-cqe-skip)
(define-io-param-feature feat-linked-file-p ioring-feat-linked-file)

(defmethod build ((self io-params) &key &allow-other-keys)
  (with-slots (sq-entries cq-entries flags sq-thread-cpu sq-thread-idle features wq-fd sq-off cq-off) self
    (with-io-uring-params res ((sq-entries sq-entries) (cq-entries cq-entries) (flags flags)
                               (sq-thread-cpu sq-thread-cpu) (sq-thread-idle sq-thread-idle) (features features)
                               (wq-fd wq-fd) (sq-off sq-off) (cq-off cq-off))
      res)))

(define-alien-type nil
    (struct io-uring
            (sq (struct io-uring-sq))
            (cq (struct io-uring-cq))
            (flags unsigned-int)
            (ring-fd int)
            (features unsigned-int)
            (enter-ring-fd int)
            (int-flags char)
            (pad (array char 3))
            (pad2 unsigned-int)))

;; io-uring instance
(defstruct uring
  (sq nil :type submission-queue)
  (cq nil :type completion-queue)
  (fd nil :type sb-posix:file-descriptor) ;; owned fd
  (params nil :type io-params)
  (memory nil :type io-memory-map))

(defvar *default-io-params* (make-io-params))
(defstruct uring-builder
  (params *default-io-params* :type io-params)
  (dontfork nil :type boolean))

(defmethod build ((self uring-builder) &key (entries 256) &allow-other-keys) self)

(defun setup-queue (fd p)
  "Setup a URING struct given a reference to a FILE-DESCRIPTOR and IO-PARAMS.")

(defun make-queue (entries)
  "Create a new URING instance with default params. N is the size of the
queue, which must be a power of two."
  (build (make-uring-builder) :entries entries))

#+nil (make-queue 2)
(defmethod build-submitter ((self uring)))

;;; Syscalls
;; register, setup, enter

(define-alien-routine io-uring-get-probe-ring (* io-uring-probe) (ring (* (struct io-uring))))
(define-alien-routine io-uring-get-probe (* io-uring-probe))
(define-alien-routine io-uring-free-probe void (* io-uring-probe))
;;...

(defalien-int io-uring-queue-init (entries int) (ring (* (struct io-uring))) (flags unsigned))


;;...

(defalien-int io-uring-submit (ring (* (struct io-uring))))

(defalien-int io-uring-register
  (fd int)
  (opcode unsigned-int)
  (args (* t))
  (nr-args unsigned-int))

;;...
(defalien-int io-uring-register-buffers
  (ring (* (struct io-uring)))
  (iovecs (* (struct iovec)))
  (nr-iovecs unsigned-int))

;;...

(defalien-int io-uring-enable-rings (ring (* (struct io-uring))))
(defalien-int io-uring-sqring-wait (ring (* (struct io-uring))))

;;...
(defalien-int io-uring-setup
  (entries unsigned-int)
  (p (* (struct io-uring-params))))

(defalien-int io-uring-enter
  (fd int)
  (to-submit unsigned-int)
  (min-complete unsigned-int)
  (flags unsigned-int)
  (arg (* t))
  (size unsigned-long))

(define-alien-routine io-uring-setup-buf-ring (* (struct io-uring-buf-ring))
  (ring (* (struct io-uring)))
  (nentries unsigned-int)
  (bgid int)
  (flags unsigned-int)
  (ret (* int)))
(defalien-int io-uring-free-buf-ring
    (ring (* (struct io-uring)))
  (br (* (struct io-uring-buf-ring)))
  (nentries unsigned-int)
  (bgid int))
;;...

;; peek-cqe wait-cqe get-sqe
;; io-uring-buf-ring-init
