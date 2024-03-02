;;; music.lisp --- Musical Lisp Systems
;; inspired by CLM (Stanford CCRMA)
;;
;; see also: http://www.titanmusic.com/papers/public/mips20010910.pdf
;; 
;; refs: https://openmusictheory.github.io/
;; https://mlochbaum.github.io/BQN-Musician/index.html

;;; Code:
(in-package :obj/music)

;;; unicode char support
(defvar *flat-char* #\MUSIC_FLAT_SIGN) ;; ♭
(defvar *sharp-char* #\MUSIC_SHARP_SIGN) ;; ♯
(defvar *natural-char* #\MUSIC_NATURAL_SIGN) ;; ♮

;;; amp/db/adb
(defmacro db-to-amp (db) 
  `(expt 10 (/ ,db 20)))

(defmacro amp-to-db (amp)
  `(* 20 (log ,amp 10)))

(defmacro vol-to-amp (v &key (max 1000))
  (let ((vol (gensym)))
    `(let ((,vol ,v))
       (if (<= ,vol 0) 0 (db-to-amp (* -10 (log (/ ,max ,vol) 2)))))))

;; the 96.3296 figure for max is from track-rms.ins
(defmacro adb-to-amp (adb &key (max 96.3296))
  (let ((db (gensym)))
    `(let ((,db ,adb))
       (if (<= ,db 0.0) 0.0 (db-to-amp (- (abs ,db) ,max))))))

(defmacro amp-to-adb (amp &key (max 96.3296))
  `(if (<= ,amp .00001526) 0.0 (+ ,max (amp-to-db ,amp))))

;;; Tones

;; reading more on this, tones can be simple or complex. Here we deal
;; with simple tones. A 'pitch' on the other hand, is the perceived
;; representation of a tone or complex tones. Multiple sets of tones
;; can share the same 'pitch'.

;; In CLM, pitches are based on C0, compared to A4
;; which is the norm. I think it makes quite a bit of sense from a
;; technical standpoint, but with notes that low it becomes very
;; difficult to hear the differences between tunings.

;; This is Lisp after all though, so the correct implementation should
;; support tuning by ear based on any note in the *PITCH-TABLE*.

;; Ideally we get smart with it.
;; NOTE: chroma,morph,chromamorph,genus equivalence across oct

(defvar *c0-default* 16.35160)

;;; Ideally pitch-sets are vectors with a lookup table for
;;; strings/symbols
(eval-always
  (defvar *pitch-table* (make-hash-table :test #'equal)))

;; (defmacro define-pitch (name octave interval &key (table *pitch-table*) (c0 *c0-default*))
;;   ;; TODO
;;   ;; (declare (ignore idx))
;;   `(let ((pitch (* ,c0 (expt 2.0 (+ ,octave (/ ,interval 12.0))))))
;;      (setf (gethash ,(symbol-name name) ,table) pitch)))

;; (define-pitch c0 0 0)
;; (define-pitch cs0 0 1)
;; (define-pitch df0 0 1)
;; (define-pitch d0 0 2) 
;; (define-pitch ds0 0 3)
;; (define-pitch ef0 0 3)
;; (define-pitch e0 0 4)
;; (define-pitch ff0 0 4)
;; (define-pitch f0 0 5)
;; (define-pitch es0 0 5)
;; (define-pitch fs0 0 6)
;; (define-pitch gf0 0 6)
;; (define-pitch g0 0 7) 
;; (define-pitch gs0 0 8)
;; (define-pitch af0 0 8)
;; (define-pitch a0 0 9) 
;; (define-pitch as0 0 10)
;; (define-pitch bf0 0 10)
;; (define-pitch b0 0 11) 
;; (define-pitch cf0 0 -1)
;; (define-pitch bs0 0 12) 

;; (define-pitch c1 1 0)
;; (define-pitch cs1 1 1)
;; (define-pitch df1 1 1)
;; (define-pitch d1 1 2) 
;; (define-pitch ds1 1 3)
;; (define-pitch ef1 1 3)
;; (define-pitch e1 1 4)
;; (define-pitch ff1 1 4)
;; (define-pitch f1 1 5)
;; (define-pitch es1 1 5)
;; (define-pitch fs1 1 6)
;; (define-pitch gf1 1 6)
;; (define-pitch g1 1 7) 
;; (define-pitch gs1 1 8)
;; (define-pitch af1 1 8)
;; (define-pitch a1 1 9) 
;; (define-pitch as1 1 10)
;; (define-pitch bf1 1 10)
;; (define-pitch b1 1 11) 
;; (define-pitch cf1 1 -1)
;; (define-pitch bs1 1 12) 

;; (define-pitch c2 2 0)
;; (define-pitch cs2 2 1)
;; (define-pitch df2 2 1)
;; (define-pitch d2 2 2) 
;; (define-pitch ds2 2 3)
;; (define-pitch ef2 2 3)
;; (define-pitch e2 2 4)
;; (define-pitch ff2 2 4)
;; (define-pitch f2 2 5)
;; (define-pitch es2 2 5)
;; (define-pitch fs2 2 6)
;; (define-pitch gf2 2 6)
;; (define-pitch g2 2 7) 
;; (define-pitch gs2 2 8)
;; (define-pitch af2 2 8)
;; (define-pitch a2 2 9) 
;; (define-pitch as2 2 10)
;; (define-pitch bf2 2 10)
;; (define-pitch b2 2 11) 
;; (define-pitch cf2 2 -1)
;; (define-pitch bs2 2 12) 

;; (define-pitch c3 3 0)
;; (define-pitch cs3 3 1)
;; (define-pitch df3 3 1)
;; (define-pitch d3 3 2) 
;; (define-pitch ds3 3 3)
;; (define-pitch ef3 3 3)
;; (define-pitch e3 3 4)
;; (define-pitch ff3 3 4)
;; (define-pitch f3 3 5)
;; (define-pitch es3 3 5)
;; (define-pitch fs3 3 6)
;; (define-pitch gf3 3 6)
;; (define-pitch g3 3 7) 
;; (define-pitch gs3 3 8)
;; (define-pitch af3 3 8)
;; (define-pitch a3 3 9) 
;; (define-pitch as3 3 10)
;; (define-pitch bf3 3 10)
;; (define-pitch b3 3 11) 
;; (define-pitch cf3 3 -1)
;; (define-pitch bs3 3 12) 

;; (define-pitch c4 4 0)
;; (define-pitch cs4 4 1)
;; (define-pitch df4 4 1)
;; (define-pitch d4 4 2) 
;; (define-pitch ds4 4 3)
;; (define-pitch ef4 4 3)
;; (define-pitch e4 4 4)
;; (define-pitch ff4 4 4)
;; (define-pitch f4 4 5)
;; (define-pitch es4 4 5)
;; (define-pitch fs4 4 6)
;; (define-pitch gf4 4 6)
;; (define-pitch g4 4 7) 
;; (define-pitch gs4 4 8)
;; (define-pitch af4 4 8)
;; (define-pitch a4 4 9) 
;; (define-pitch as4 4 10)
;; (define-pitch bf4 4 10)
;; (define-pitch b4 4 11) 
;; (define-pitch cf4 4 -1)
;; (define-pitch bs4 4 12) 

;; (define-pitch c5 5 0)
;; (define-pitch cs5 5 1)
;; (define-pitch df5 5 1)
;; (define-pitch d5 5 2) 
;; (define-pitch ds5 5 3)
;; (define-pitch ef5 5 3)
;; (define-pitch e5 5 4)
;; (define-pitch ff5 5 4)
;; (define-pitch f5 5 5)
;; (define-pitch es5 5 5)
;; (define-pitch fs5 5 6)
;; (define-pitch gf5 5 6)
;; (define-pitch g5 5 7) 
;; (define-pitch gs5 5 8)
;; (define-pitch af5 5 8)
;; (define-pitch a5 5 9) 
;; (define-pitch as5 5 10)
;; (define-pitch bf5 5 10)
;; (define-pitch b5 5 11) 
;; (define-pitch cf5 5 -1)
;; (define-pitch bs5 5 12) 

;; (define-pitch c6 6 0)
;; (define-pitch cs6 6 1)
;; (define-pitch df6 6 1)
;; (define-pitch d6 6 2) 
;; (define-pitch ds6 6 3)
;; (define-pitch ef6 6 3)
;; (define-pitch e6 6 4)
;; (define-pitch ff6 6 4)
;; (define-pitch f6 6 5)
;; (define-pitch es6 6 5)
;; (define-pitch fs6 6 6)
;; (define-pitch gf6 6 6)
;; (define-pitch g6 6 7) 
;; (define-pitch gs6 6 8)
;; (define-pitch af6 6 8)
;; (define-pitch a6 6 9) 
;; (define-pitch as6 6 10)
;; (define-pitch bf6 6 10)
;; (define-pitch b6 6 11) 
;; (define-pitch cf6 6 -1)
;; (define-pitch bs6 6 12) 

;; (define-pitch c7 7 0)
;; (define-pitch cs7 7 1)
;; (define-pitch df7 7 1)
;; (define-pitch d7 7 2) 
;; (define-pitch ds7 7 3)
;; (define-pitch ef7 7 3)
;; (define-pitch e7 7 4)
;; (define-pitch ff7 7 4)
;; (define-pitch f7 7 5)
;; (define-pitch es7 7 5)
;; (define-pitch fs7 7 6)
;; (define-pitch gf7 7 6)
;; (define-pitch g7 7 7) 
;; (define-pitch gs7 7 8)
;; (define-pitch af7 7 8)
;; (define-pitch a7 7 9) 
;; (define-pitch as7 7 10)
;; (define-pitch bf7 7 10)
;; (define-pitch b7 7 11) 
;; (define-pitch cf7 7 -1)
;; (define-pitch bs7 7 12) 

;; (define-pitch c8 8 0)
;; (define-pitch cs8 8 1)
;; (define-pitch df8 8 1)
;; (define-pitch d8 8 2) 
;; (define-pitch ds8 8 3)
;; (define-pitch ef8 8 3)
;; (define-pitch e8 8 4)
;; (define-pitch ff8 8 4)
;; (define-pitch f8 8 5)
;; (define-pitch es8 8 5)
;; (define-pitch fs8 8 6)
;; (define-pitch gf8 8 6)
;; (define-pitch g8 8 7) 
;; (define-pitch gs8 8 8)
;; (define-pitch af8 8 8)
;; (define-pitch a8 8 9) 
;; (define-pitch as8 8 10)
;; (define-pitch bf8 8 10)
;; (define-pitch b8 8 11) 
;; (define-pitch cf8 8 -1)
;; (define-pitch bs8 8 12) 

