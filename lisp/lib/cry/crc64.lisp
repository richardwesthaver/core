;;; Copyright (c) 2008, Rob Blackwell.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(in-package :cry/crc64)

;; The polynomial used in the original  SWISS / PROT.
(declaim (type (unsigned-byte 64) +polynomial+ +improved-polynomial+))
(defconstant +polynomial+ #xd800000000000000)

;; Improved calculation of CRC-64 values for protein sequences
;; By David T. Jones (dtj@cs.ucl.ac.uk)
(defconstant +improved-polynomial+ #x95AC9329AC4BC9B)

;; Lookup tables. 
;; We store high and low order bytes separately to benefit from 
;; 32 bit arithmentic performance.
(declaim (type (array (unsigned-byte 32) (256)) *crc-table-h* *crc-table-l*))
(defvar *crc-table-h* (make-array 256 :element-type '(unsigned-byte 32)))
(defvar *crc-table-l* (make-array 256 :element-type '(unsigned-byte 32)))

(defun init-crc64 (polynomial)
  "Computes lookup tables of CRC values for byte values 0 thru 255"
  (dotimes (i 256)
    (let ((part i))
      (dotimes (j 8)
	(if (eql (logand part 1) 1)
	    (setf part (logxor (ash part -1) polynomial))
	    (setf part (ash part -1))))
      (setf (aref *crc-table-h* i) (ash (logand part #xFFFFFFFF00000000) -32))
      (setf (aref *crc-table-l* i) (logand part #xFFFFFFFF)))))

(defun crc64-file (pathname)
  "Calculates the CRC64 of the file specified by pathname."
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (crc64-stream stream)))

(defun crc64-sequence (sequence &key (initial-crc 0) (start 0) 
		       (end (length sequence)))
  "Calculates the CRC64 from sequence, which is either a 
simple-string or a simple-array with element-type \(unsigned-byte 8)"
  (declare (type (simple-array * (*)) sequence)
	   (type fixnum start end initial-crc)
	   (optimize (speed 3) (space 0) (debug 0)))

  (let ((crch (logand (ash initial-crc -32) #xFFFFFFFF)) 
	(crcl (logand initial-crc #xFF)) 
	(table-index 0))
    (declare (type (unsigned-byte 32) crch)
	     (type (unsigned-byte 32) crcl)
	     (type (unsigned-byte 8) table-index))

    (etypecase sequence

      ((simple-array (unsigned-byte 8) (*))
       (locally
           (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
	 (loop for n from start below end do
	   (setf table-index (logand (logxor crcl (aref sequence n)) #xFF))
	      (setf crcl (logxor (logior (ash crcl -8) 
					 (ash (logand crch #xFF) 24))
				 (the (unsigned-byte 32)
				   (aref *crc-table-l* table-index))))
	      (setf crch (logxor (ash crch -8)  
				 (the (unsigned-byte 32) 
				   (aref *crc-table-h* table-index)))))))

      (simple-string
       (locally
	   (declare (type simple-string sequence))
	 (loop for n from start below end do
	      (setf table-index (logand (logxor crcl 
						(char-code (aref sequence n)))
					#xFF))
	      (setf crcl (logxor (logior (ash crcl -8) 
					 (ash (logand crch #xFF) 24))   
				 (the (unsigned-byte 32) 
				   (aref *crc-table-l* table-index))))
	      (setf crch (logxor (ash crch -8)  
				 (the (unsigned-byte 32) 
				   (aref *crc-table-h* table-index))))))))

    (+ (ash crch 32) crcl)))

(defun crc64-stream (stream &key (initial-crc 0))
  "Calculates the CRC64 on the given stream."
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type (unsigned-byte 64) initial-crc))
  (let ((crch (logand (ash initial-crc -32) #xFFFFFFFF)) 
	(crcl (logand initial-crc #xFF)) 
	(table-index 0) 
	(b 0))
    (declare (type (unsigned-byte 32) crch)
	     (type (unsigned-byte 32) crcl)
	     (type (unsigned-byte 8) table-index))
    (loop while (setf b (read-byte stream nil nil)) do
      (setf table-index (logand (logxor crcl b) #xFF))
      (setf crcl (logxor (logior (ash crcl -8) 
				 (ash (logand crch #xFF) 24))  
			 (the (unsigned-byte 32) 
			   (aref *crc-table-l* table-index))))
      (setf crch (logxor (ash crch -8)  
			 (the (unsigned-byte 32) 
			   (aref *crc-table-h* table-index)))))

    (+ (ash crch 32) crcl)))
