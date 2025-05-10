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
(define-alien-routine openblas-get-num-threads int)
(define-alien-routine openblas-set-num-threads-local int (n int))
(define-alien-routine openblas-get-num-procs int)
(define-alien-routine openblas-get-config c-string)
(define-alien-routine openblas-get-corename c-string)
(define-alien-routine openblas-set-threads-callback-function void (* (function void)))
;; (define-alien-routine openblas-setaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
;; (define-alien-routine openblas-getaffinity int (thread-idx int) (cpusetsize size-t) (cpu-set (* cpu-set-t)))
(define-alien-routine openblas-get-parallel int)
(define-alien-enum (openblas-parallel int)
  :sequential 0
  :thread 1
  :openmp 2)
      
(defmacro blasfunc (sym ret &rest args)
  `(defar (,(concatenate 'string (string-downcase (symbol-name sym)) "_") ,sym) ,ret ,@args))

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
(blasfunc sdot float
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc sdsdot float
  (n int :copy)
  (alpha (* float))
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc dsdot float
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc ddot float
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc qdot xdouble
  (n int :copy)
  (x (* xdouble))
  (incx int :copy)
  (y (* xdouble))
  (incy int :copy))

(blasfunc sbdot float
  (n int :copy)
  (x (* unsigned-short))
  (incx int :copy)
  (y (* unsigned-short))
  (incy int :copy))

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
  (ret complex-float :in-out)
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc cdotc void
  (ret complex-float :in-out)
  (n int :copy)
  (x (* float))
  (incx int :copy)
  (y (* float))
  (incy int :copy))

(blasfunc zdotu void
  (ret complex-double :in-out)
  (n int :copy)
  (x (* double))
  (incx int :copy)
  (y (* double))
  (incy int :copy))

(blasfunc zdotc void
  (ret complex-double :in-out)
  (n int :copy)
  (x (* double))
  (incx int :copy)
  (y (* double))
  (incy int :copy))

(blasfunc xdotu void
  (ret complex-xdouble :in-out)
  (n int :copy)
  (x (* xdouble))
  (incx int :copy)
  (y (* xdouble))
  (incy int :copy))

(blasfunc xdotc void
  (ret complex-xdouble :in-out)
  (n int :copy)
  (x (* xdouble))
  (incx int :copy)
  (y (* xdouble))
  (incy int :copy))

;; saxpy daxpy qaxpy caxpy zaxpy xaxpy
;; caxpyc zaxpyc xaxpyc
;; scopy dcopy qcopy ccopy zcopy xcopy
;; sswap dswap qswap cswap zswap xswap
;; sasum scasum dasum qasum dzasum qxasum
;; ssum scsum dsum qsum dzsum qxsum
;; isamax idamax iqamax icamax izamax ixamax
;; ismax idmax iqmax icmax izmax ixmax
;; isamin idamin iqamin icamin izamin ixamin
;; ismin idmin iqmin icmin izmin ixmin
;; samax damax qamax scamax dzamax qxamax
;; samin damin qamin scamin dzamin qxamin
;; smax dmax qmax scmax dzmax qxmax
;; smin dmin qmin scmin dzmin qxmin
;; sscal dscal qscal cscal zscal xscal csscal zdscal xqscal
;; snrm2 scnrm2
;; dnrm2 qnrm2 dznrm2 qxnrm2
;; srot drot qrot csrot zdrot xqrot
;; srotg drotg qrotg crotg zrotg xrotg
;; srotmg drotmg
;; srotm drotm qrotm

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
