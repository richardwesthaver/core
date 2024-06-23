;;; zstd/tests.lisp --- Zstd FFI tests

;;

;;; Code:
(defpackage :zstd/tests 
  (:use :cl :std :rt :zstd :sb-alien :log))

(in-package :zstd/tests)

(defsuite :zstd)
(in-suite :zstd)

(load-zstd)

(deftest sanity ()
  (mapc (lambda (x)
          (is (= (car x) (cdr x))))
        `((,+zstd-clevel-default+ . ,(zstd::zstd-defaultclevel))
          (,+zstd-magicnumber+ . -47205080)
          (,+zstd-magic-skippable-start+ . 407710288)
          (,+zstd-blocksizelog-max+ . 17)
          (,+zstd-blocksize-max+ . 131072)
          (,+zstd-contentsize-unknown+ . -1)
          (,+zstd-contentsize-error+ . -2)
          (,+zstd-max-input-size+ . -71777214294589696)
          (,+zstd-version-number+ . ,(zstd::zstd-versionnumber))
          (,+zstd-magic-dictionary+ . -332356553))))

(deftest simple ()
  "Test the Zstd Simple API functions - ZSTD-COMPRESS and ZSTD-DECOMPRESS."
  (let ((dst-capacity 8096)
        (src-size 4096))
    (with-alien ((dst (* (unsigned 8)) (make-alien (unsigned 8) dst-capacity))
                 (src (* (unsigned 8)) (make-alien (unsigned 8) src-size))
                 (clevel int (zstd-defaultclevel)))
      (let ((csize (zstd-compress dst dst-capacity src src-size clevel)))
        (is (zerop (zstd-iserror (zstd-decompress src src-size dst csize)))))))
  (let* ((octets (make-array 4000 :initial-element (random 255)))
         (compressed (zstd:zstdc octets)))
    (is (equalp (zstdd compressed) octets))))

(deftest streaming ()
  "Test the Zstd v1 Streaming API."
  (is (< (zstd-cstreaminsize) (zstd-cstreamoutsize)))
  (with-alien ((in (* zstd-inbuffer) (zstd::allocate-zstd-inbuffer))
               (out (* zstd-outbuffer) (zstd::allocate-zstd-outbuffer))
               (cst (* zstd-cstream) (zstd::zstd-createcstream))
               (dst (* zstd-dstream) (zstd::zstd-createdstream)))
    (let* ((str "this is a test yad ayd ay aya dayd ayd ada")
           (len (length str))
           (out-array (make-array len :adjustable t)))
      (setf (zstd::zstd-inbuffer-src in) (make-alien-string str)
            (zstd::zstd-inbuffer-size in) len)
      (with-zstd-cstream (cs cst)
        (is (zerop (zstd::zstd-initcstream cst (zstd-defaultclevel))))
        (with-zstd-dstream (ds dst)
          (is (zerop (zstd::zstd-initdstream dst)))
          ;; (setf (zstd::zstd-outbuffer-dst out) (make-alien-string str))
          (zstd-compressstream cst out in)
          (zstd::zstd-flushstream cst out)
          (zstd::zstd-endstream cst out)
          (clone-octets-from-alien (cast (zstd::zstd-outbuffer-dst out)
                                         (* (unsigned 8)))
                                   out-array
                                   (zstd::zstd-outbuffer-size out))
          ;; (is (zerop (zstd-compressstream2 cst out in (sb-alien::alien-value zstd::zstd-e-flush)))
          (zstd-decompressstream dst out in)
          (is (string-equal
               (cast (zstd::zstd-inbuffer-src in)
                     c-string)
               str)))))))

(deftest streaming2 ()
  "Test the Zstd v2 Streaming API.")
;; simple-dictionary
;; builk-dictionary
