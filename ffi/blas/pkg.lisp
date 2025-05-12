;;; pkg.lisp --- BLAS packages

;; 

;;; Code:
(defpackage :blas
  (:use :cl :std :log :sb-alien)
  (:export :load-openblas :load-blas :load-lapack :load-lapacke :load-cblas
   :dgemm))

(in-package :blas)
(define-alien-loader openblas "/usr/lib/")
;; usually just points to libopenblas.so
(define-alien-loader blas "/usr/lib/")
(define-alien-loader cblas "/usr/lib/")
(define-alien-loader lapack "/usr/lib/")
(define-alien-loader lapacke "/usr/lib/")
;; these are part of CBLAS
(defar openblas-get-num-threads int)
(defar openblas-set-num-threads-local int (n int))
(defar openblas-get-num-procs int)
(defar openblas-get-config c-string)
(defar openblas-get-corename c-string)
(defar openblas-set-threads-callback-function void (* (function void)))
;; (defar openblas-setaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
;; (defar openblas-getaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
(defar openblas-get-parallel int)
(define-alien-enum (openblas-parallel int)
  :sequential 0
  :thread 1
  :openmp 2)
      
(defmacro blasfunc (sym ret &rest args)
  `(defar (,(concatenate 'string (string-downcase (symbol-name sym)) "_") ,sym) ,ret ,@args))
(defmacro blas5 (sym type ret &rest args)
  `(blasfunc ,sym ,ret (n int :copy) ,@args (x (* ,type)) (incx int :copy) (y (* ,type)) (incy int :copy)))
(defmacro blas5s (sym &rest args)
  `(blas5 ,sym float void ,@args))
(defmacro blas5d (sym &rest args)
  `(blas5 ,sym double void ,@args))
(defmacro blas5bf16 (sym &rest args)
  `(blas5 ,sym unsigned-short void ,@args))
(defmacro blas5c (sym &rest args)
  `(blas5 ,sym complex-float void ,@args))
(defmacro blas5z (sym &rest args)
  `(blas5 ,sym complex-double void ,@args))
(defmacro blas5q (sym &rest args)
  `(blas5 ,sym xdouble void ,@args))
(defmacro blas5x (sym &rest args)
  `(blas5 ,sym complex-xdouble void ,@args))
(defmacro blas3 (sym type ret)
  `(blasfunc ,sym ,ret (n int :copy) (x (* ,type)) (incx int :copy)))
(defmacro blas3s (sym ret &rest args)
  `(blas3 ,sym float ,ret ,@args))
(defmacro blas3d (sym ret &rest args)
  `(blas3 ,sym double ,ret ,@args))
(defmacro blas3bf16 (sym ret &rest args)
  `(blas3 ,sym unsigned-short ,ret ,@args))
(defmacro blas3c (sym ret &rest args)
  `(blas3 ,sym complex-float ,ret ,@args))
(defmacro blas3z (sym ret &rest args)
  `(blas3 ,sym complex-double ,ret ,@args))
(defmacro blas3q (sym ret &rest args)
  `(blas3 ,sym xdouble ,ret ,@args))
(defmacro blas3x (sym ret &rest args)
  `(blas3 ,sym complex-xdouble ,ret ,@args))
(defmacro blas7 (sym type)
  `(blasfunc ,sym void (n int :copy) (x (* ,type)) (incx int :copy) (y (* ,type)) (incy int :copy)
             (za (* ,type)) (zb (* ,type))))

(defmacro blas4 (sym type &rest args)
  `(blasfunc ,sym void (n int :copy) (a (* ,type)) (x (* ,type)) (incx int :copy)))

(defmacro blas4* (sym type)
  `(blasfunc ,sym void (a (* ,type)) (b (* ,type)) (c (* ,type)) (s (* ,type))))

(defmacro blas5* (sym type)
  `(blasfunc ,sym void (d1 (* ,type)) (d2 (* ,type)) (x (* ,type)) (y (* ,type)) (s (* ,type))))

(defmacro blas6 (sym type)
  `(blasfunc ,sym void (n int :copy) (x (* ,type)) (incx int :copy) (y (* ,type)) (incy int :copy) (s (* ,type))))
;; FLOATRET = float
;; blasint = int
;; BLASLONG = long
;; BLASULONG unsigned-long
;; xdouble double?
;; bfloat16 unsigned-short

(define-alien-type xdouble (array unsigned-long 2))
(define-alien-type complex-float (array float 2))
(define-alien-type complex-double (array double 2))
(define-alien-type complex-xdouble (array xdouble 2))

#|
 XERBLA  is an error handler for the LAPACK routines.
 It is called by an LAPACK routine if an input parameter has an
 invalid value.  A message is printed and execution stops.

 Installers may consider modifying the STOP statement in order to
 call system-specific exception-handling facilities.
|#
(blasfunc xerbla int
  (srname c-string)
  (info int :copy)
  (nout int))

;; exported by f77blas.h
;; this is defined literally (no BLASFUNC)
(define-alien-routine ("openblas_set_num_threads_" openblas-set-num-threads) void (n int :copy))

;; RETURN ON STACK (f77blas.h, libblas.so)
(define-alien-type openblas-dojob-callback 
  (function void int (* t) int))

(define-alien-type openblas-threads-callback
    (function void int (* openblas-dojob-callback) int size-t (* t) int))

;;; Level 1
(blas5 sdot float float)
(blas5 sdsdot float float (a (* float)))
(blas5 dsdot float double)
(blas5 ddot float double)
(blas5 qdot xdouble xdouble)
(blas5 sbdot unsigned-short float)

(blasfunc sbstobf16 void
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* unsigned-short))
  (incy int :copy))

(blasfunc sbdtobf16 void
  (n int :copy)
  (x (* double))
  (incx int :copy)
  (y (* unsigned-short))
  (incy int :copy))

(blasfunc sbf16tos void
  (n int :copy)
  (x (* unsigned-short))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc dbf16tod void
  (n int :copy)
  (x (* unsigned-short))
  (incx int :copy)
  (y (* double))
  (incy int :copy))

(blasfunc cdotu void
  (z (* complex-float))
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc cdotc void
  (z (* complex-float))
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc zdotu void
  (z (* complex-double))
  (n int :copy)
  (x (* double))
  (incx int :copy)
  (y (* double))
  (incy int :copy))

(blasfunc zdotc void
  (z (* complex-double))
  (n int :copy)
  (x (* double))
  (incx int :copy)
  (y (* double))
  (incy int :copy))

(blasfunc xdotu void
  (z (* complex-xdouble))
  (n int :copy)
  (x (* xdouble))
  (incx int :copy)
  (y (* xdouble))
  (incy int :copy))

(blasfunc xdotc void
  (z (* complex-xdouble))
  (n int :copy)
  (x (* xdouble))
  (incx int :copy)
  (y (* xdouble))
  (incy int :copy))

;; y = ax + y
(blas5s saxpy (a float :copy))
(blas5d daxpy (a double :copy))
(blas5q qaxpy (a xdouble :copy))
(blas5c caxpy (a complex-float :copy))
(blas5z zaxpy (a complex-double :copy))
(blas5x xaxpy (a complex-xdouble :copy))
(blas5s caxpyc (a float :copy))
(blas5d zaxpyc (a double :copy))
(blas5q xaxpyc (a xdouble :copy))
(blas5s scopy)
(blas5d dcopy)
(blas5q qcopy)
(blas5c ccopy)
(blas5z zcopy)
(blas5x xcopy)
(blas5s sswap)
(blas5d dswap)
(blas5q qswap)
(blas5c cswap)
(blas5z zswap)
(blas5x xswap)
(blas3s sasum float)
(blas3s scasum float)
(blas3d dasum double)
(blas3q qasum xdouble)
(blas3d dzasum double)
(blas3q qxasum xdouble)
(blas3s ssum float)
(blas3s scsum float)
(blas3d dsum double)
(blas3q qsum xdouble)
(blas3d dzsum double)
(blas3q qxsum xdouble)
(blas3s isamax int)
(blas3d idamax int)
(blas3q iqamax int)
(blas3s icamax int)
(blas3d izamax int)
(blas3q ixamax int)
(blas3s ismax int)
(blas3d idmax int)
(blas3q iqmax int)
(blas3s icmax int)
(blas3d izmax int)
(blas3q ixmax int)
(blas3s isamin int)
(blas3d idamin int)
(blas3q iqamin int)
(blas3s icamin int)
(blas3d izamin int)
(blas3q ixamin int)
(blas3s ismin int)
(blas3d idmin int)
(blas3q iqmin int)
(blas3s icmin int)
(blas3d izmin int)
(blas3q ixmin int)
(blas3s samax float)
(blas3d damax double)
(blas3q qamax xdouble)
(blas3s scamax float)
(blas3d dzamax double)
(blas3q qxamax xdouble)
(blas3s samin float)
(blas3d damin double)
(blas3q qamin xdouble)
(blas3s scamin float)
(blas3d dzamin double)
(blas3q qxamin xdouble)
(blas3s smax float)
(blas3d dmax double)
(blas3q qmax xdouble)
(blas3s scmax float)
(blas3d dzmax double)
(blas3q qxmax xdouble)
(blas3s smin float)
(blas3d dmin double)
(blas3q qmin xdouble)
(blas3s scmin float)
(blas3d dzmin double)
(blas3q qxmin xdouble)
(blas4 sscal float)
(blas4 dscal double)
(blas4 qscal xdouble)
(blas4 cscal float)
(blas4 zscal double)
(blas4 xscal xdouble)
(blas4 csscal float)
(blas4 zdscal double)
(blas4 xqscal xdouble)
(blas3s snrm2 float)
(blas3s scnrm2 float)
(blas3d dnrm2 double)
(blas3q qnrm2 xdouble)
(blas3d dznrm2 double)
(blas3q qxnrm2 xdouble)
(blas7 srot float) 
(blas7 drot double)
(blas7 qrot xdouble)
(blas7 csrot float)
(blas7 zdrot double)
(blas7 xqrot xdouble)
(blas4* srotg float)
(blas4* drotg double)
(blas4* qrotg xdouble)
(blas4* crotg float)
(blas4* zrotg double)
(blas4* xrotg xdouble)
(blas5* srotmg float)
(blas5* drotmg double)
(blas6 srotm float)
(blas6 drotm double)
(blas6 qrotm xdouble)

;;; Level 2
;; sger dger cgeru cgerc zgeru zgerc xgeru xgerc
;; sbgemv sgemv dgemv qgemv cgemv zgemv xgemv
;; strsv dtrsv qtrsv ctrsv ztrsv xtrsv
;; strmv dtrmv ctrmv ztrmv xtrmv
;; stpsv dtpsv qtpsv ctpsv ztpsv xtpsv
;; stpmv dtpmv qtpmv ctpmv ztpmv xtpmv
;; stbmv dtbmv qtbmv ctbmv ztbmv xtbmv
;; stbsv dtbsv qtbsv ctbsv ztbsv xtbsv
;; ssymv dsymv qsymv csymv zsymv xsymv
;; sspmv dspmv qspmv cspmv zspmv xspmv
;; ssyr dsyr qsyr csyr zsyr xsyr
;; ssyr2 dsyr2 qsyr2 csyr2 zsyr2 xsyr2
;; sspr dspr qspr cspr zspr xspr
;; sspr2 dspr2 qspr2 cspr2 zspr2 xspr2
;; cher zher xher
;; chpr zhpr xhpr
;; cher2 zher2 xher2
;; chpr2 zhpr2 xhpr2
;; chemv zhemv xhemv
;; chpmv zhpmv xhpmv
;; snorm dnorm cnorm znorm
;; sgbmv dgbmvqgbmv cgbmv zgbmv xgbmv
;; ssbmv dsbmv qsbmv csbmv zsbmv xsbmv
;; chbmv zhbmv xhbmv

;;; Level 3
;; sbgemm sgemm
(blasfunc dgemm void
  (transa char :copy)
  (transb char :copy)
  (m int :copy)
  (n int :copy)
  (k int :copy)
  (alpha double :copy)
  (a (* double))
  (lda int :copy)
  (b (* double))
  (ldb int :copy)
  (beta double :copy)
  (c (* double))
  (ldc int :copy))
;; qgemm cgemm zgemm xgemm
;; cgemm3m zgemm3m xgemm3m
;; sgemmt dgemmt cgemmt zgemmt
;; sge2mm dge2mm cge2mm zge2mm
;; strsm dtrsm qtrsm ctrsm ztrsm xtrsm
;; strmm dtrmm qtrmm ctrmm ztrmm xtrmm
;; ssymm dsymm qsymm csymm zsymm xsymm
;; csymm3m zsymm3m xsymm3m
;; ssyrk dsyrk zsyrk csyrk zsyrk xsyrk
;; ssyr2k dsyr2k qsyr2k csyr2k zsyr2k xsyr2k
;; chemm zhemm xhemm
;; chemm3m zhemm3m xhemm3m
;; cherk zherk xherk
;; cher2k zher2k xher2k
;; cher2m zher2m xher2m
;; sgemt dgemt cgemt zgemt
;; sgema dgema cgema zgema
;; sgems dgems cgems zgems
;; sgems dgems cgems zgems
;; sgemc dgemc qgemc cgemc zgemc xgemc

;;; Lapack routines
;; sgetf2 dgetf2 qgetf2 cgetf2 zgetf2 xgetf2
;; sgetrf dgetrf qgetrf cgetrf zgetrf xgetrf
;; slaswp dlaswp qlaswp claswp zlaswp xlaswp
;; sgetrs dgetrs qgetrs cgetrs zgetrs xgetrs
;; sgesv dgesv qgesv cgesv zgesv xgesv
;; spotf2 dpotf2 qpotf2 cpotf2 zpotf2 xpotf2
;; spotrf dpotrf qpotrf cpotrf zpotrf xpotrf
;; spotri dpotri qpotri cpotri zpotri xpotri
;; spotrs dpotrs cpotrs zpotrs xpotrs
;; slauu2 dlauu2 qlauu2 clauu2 zlauu2 xlauu2
;; slauum dlauum qlauum clauum zlauum xlauum
;; strti2 dtrti2 qtrti2 ctrti2 ztrti2 xrti2
;; strti dtrti qtrti ctrti ztrti xtrti
;; slamch dlamch qlamch
;; slamc3 dlamc3 qlamc3

;;; BLAS extensions
;; saxpby daxpby caxpby zaxpby
;; somatcopy domatcopy comatcopy zomatcopy
;; simatcopy dimatcopy cimatcopy zimatcopy
;; sgeadd dgeadd cgeadd zgeadd
