;;; id3.lisp --- ID3 Metadata

;; ID3 Metadata container serialization

;;; Commentary:

;; https://en.wikipedia.org/wiki/ID3

#| 
ID3 is a metadata container most often used in conjunction with the MP3 audio
file format. It allows information such as the title, artist, album, track
number, and other information about the file to be stored in the file itself.

ID3 is a de facto standard for metadata in MP3 files; no standardization body
was involved in its creation nor has such an organization given it a formal
approval status.
|#
;;; Code:
(in-package :dat/id3)

(defun mp3-p (file)
  (string-equal "mp3" (pathname-type file)))

;; (simple-array (unsigned-byte 7) (4))
(deftype u28 () '(unsigned-byte 28))

(defstruct id3-header 
  (version 0 :type octet) 
  (revision 0 :type octet)
  (flags 0 :type octet)
  (size 0 :type u28))

(define-constant +id3-magic+ #(73 68 51) :test 'equalp)

;; FIX 2025-02-07: 
(defun decode-u28 (bytes)
  "Decode a sequence of 7-bit bytes as an ID3-compliant unsigned 28-bit integer."
  (declare ((array (unsigned-byte 7)) bytes))
  (octets-to-integer bytes))

(defun read-id3-header (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((magic (make-octets 3)))
      (read-sequence magic in)
      (assert (equalp magic +id3-magic+)))
    (let ((header (make-id3-header 
                   :version (read-byte in) 
                   :revision (read-byte in)
                   :flags (read-byte in))))
      (let ((size (make-array 4 :element-type '(unsigned-byte 7))))
        (read-sequence size in)
        (setf (id3-header-size header) (decode-u28 size))
        header))))

;; (read-id3-header "/mnt/z/music/05 - Clear.mp3")

(defun show-id3-header (file)
  (with-slots (major-version revision flags size) (read-id3-header file)
    (format t "ID3 ~d.~d ~8,'0b ~d bytes -- ~a~%"
            major-version revision flags size (enough-namestring file))))

(defun show-id3-headers (dir) 
  (std/path:walk-directory dir #'mp3-p (constantly t) #'show-id3-header))

(defun id3-p (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((magic (make-octets 3)))
      (read-sequence magic in)
      (equalp magic +id3-magic+))))

(defclass id3-frame (id)
  (data size))

(defun find-id3-frame-class (id)
  (declare (ignore id))
  'id3-frame)
