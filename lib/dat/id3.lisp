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

(deftype u28 () '(unsigned-byte 28))

(define-io :id3
  (u28
   (:read (in)
          (loop with val = 0
                for lbit downfrom 21 to 0 by 7 do
                (setf (ldb (byte 7 lbit) val) (read-byte in))
                finally (return val)))
   (:write (out val)
           (loop for lbit downfrom 21 to 0 by 7 do
                    (write-byte (ldb (byte 7 lbit) val) out)))))

(defstruct id3-header 
  (version 3 :type octet) 
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

(defun frame-compressed-p (flags) (logbitp 7 flags))

(defun frame-encrypted-p (flags) (logbitp 6 flags))

(defun frame-grouped-p (flags) (logbitp 5 flags))

(defun frame-name-member (id)
  (cond
    ((member id '("COM" "COMM") :test #'string=) "Comment")
    ((member id '("TAL" "TALB") :test #'string=) "Album")
    ((member id '("TCM" "TCOM") :test #'string=) "Composer")
    ((member id '("TCO" "TCON") :test #'string=) "Genre")
    ((member id '("TEN" "TENC") :test #'string=) "Encoding program")
    ((member id '("TP1" "TPE1") :test #'string=) "Artist")
    ((member id '("TPA" "TPOS") :test #'string=) "Part of set")
    ((member id '("TRK" "TRCK") :test #'string=) "Track")
    ((member id '("TT2" "TIT2") :test #'string=) "Song")
    ((member id '("TYE" "TYER") :test #'string=) "Year")
    (t id)))

(defclass id3-tag (id)
  ((extended-header-size)
   (extra-flags)
   (padding-size)
   (crc)
   (frames)))

(defun extended-p (flags) (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  (and (extended-p flags) (logbitp 15 extra-flags)))

(defun upto-null (string)
  (subseq string 0 (position (code-char 0) string)))

(defun find-frame (id3 ids)
  (find-if #'(lambda (x) (find (id x) ids :test #'string=)) (slot-value id3 'frames)))

(defun get-text-info (id3 &rest ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (information frame)))))

(defmethod information ((frame id3-frame))
  (with-output-to-string (s)
    (loop for byte across (data frame) do
          (format s "~2,'0x" byte))))

(defun album (id3) (get-text-info id3 "TAL" "TALB"))

(defun composer (id3) (get-text-info id3 "TCM" "TCOM"))

(defun genre (id3) (get-text-info id3 "TCO" "TCON"))

(defun encoding-program (id3) (get-text-info id3 "TEN" "TENC"))

(defun artist (id3) (get-text-info id3 "TP1" "TPE1"))

(defun part-of-set (id3) (get-text-info id3 "TPA" "TPOS"))

(defun track (id3) (get-text-info id3 "TRK" "TRCK"))

(defun song (id3) (get-text-info id3 "TT2" "TIT2"))

(defun year (id3) (get-text-info id3 "TYE" "TYER" "TDRC"))

(defun translated-genre (id3)
  (let ((genre (genre id3)))
    (if (and genre (char= #\( (schar genre 0)))
        (translate-v1-genre genre)
        genre)))

(defparameter *id3-v1-genres*
  #(
    ;; These are the official ID3v1 genres.
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instrumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychadelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"

    ;; These were made up by the authors of Winamp but backported into
    ;; the ID3 spec.
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"

    ;; These were also invented by the Winamp folks but ignored by the
    ;; ID3 authors.
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))


(defun translate-v1-genre (genre)
  (aref *id3-v1-genres* (parse-integer genre :start 1 :junk-allowed t)))
