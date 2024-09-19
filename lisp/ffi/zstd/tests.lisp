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
               (out (* zstd-outbuffer) (zstd::allocate-zstd-outbuffer)))
    (let* ((str "this is a test yad ayd ay aya dayd ayd ada"))
      (setf (zstd::zstd-inbuffer-src in) (make-alien-string str)
            (zstd::zstd-inbuffer-size in) (zstd-cstreaminsize))
      (with-zstd-cstream (cs)
        (is (zerop (zstd::zstd-initcstream cs (zstd-defaultclevel))))
        (with-zstd-dstream (ds)
          ;; (setf (zstd::zstd-outbuffer-dst out) (make-alien-string str))
          (setf (zstd::zstd-outbuffer-size out) (zstd-cstreamoutsize))
          (zstd-compressstream cs out in)
          (zstd::zstd-flushstream cs out)
          (zstd::zstd-endstream cs out)
          (zstd-decompressstream ds out in)
          (is (string-equal
               (cast (zstd::zstd-inbuffer-src in)
                     c-string)
               str)))))))

(deftest streaming2 ()
  "Test the Zstd v2 Streaming API."
  (let ((test "test 1 2 3"))
    (with-zstd-buffers (in out :src (make-alien-string test))
      (with-zstd-streams (cs ds)
        (zstd-compressstream2 cs out in 0)
        (zstd-compressstream2 cs out in 1)
        (is (zerop (zstd-iserror (zstd-compressstream2 cs out in 2))))
        (zstd::zstd-flushstream cs out)
        (is (zerop (zstd-iserror (zstd::zstd-endstream cs out))))
        (zstd-decompressstream ds out in)
        (is (string-equal 
             (cast (zstd::zstd-inbuffer-src in) c-string)
             test))))))

(deftest simple-dictionary ()
  (let ((test "test 1 2 3"))
    (with-alien ((dict (* t))
                 (dst (array (unsigned 8) 100)))
      (with-zstd-buffers (in out :src (cast (make-alien-string test) (* t)) :dst (cast dst (* t)) :dst-size 100)
        (is (= 100 (zstd::zstd-outbuffer-size out)))
        (with-zstd-streams (cs ds)
          (is 
           (zerop
            (zstd-iserror
             (zstd::zstd-compress-usingdict 
              cs 
              (zstd::zstd-outbuffer-dst out) (zstd::zstd-outbuffer-size out) 
              (zstd::zstd-inbuffer-src in) (zstd::zstd-inbuffer-size in)
              dict (length test) (zstd-defaultclevel)))))
          (is
           (zerop
            (zstd-iserror
             (zstd::zstd-decompress-usingdict 
              ds 
              (zstd::zstd-outbuffer-dst out) (zstd::zstd-outbuffer-size out) 
              (zstd::zstd-inbuffer-src in) (zstd::zstd-inbuffer-size in)
              dict (length test))))))))))

(deftest bulk-dictionary ()
  (with-zstd-ddict (dd :buffer #(1 2 3))
    (is (typep dd '(alien (* (struct zstd::zstd-ddict-s))))))
  (with-zstd-cdict (cd :buffer #(4 5 6))
    (is (typep cd '(alien (* (struct zstd::zstd-cdict-s)))))))
