;;; obj/uri/domain.lisp --- URI Domains

;;

;;; Code:
(in-package :obj/uri)

(defun next-subdomain (hostname &optional (start 0))
  (let ((pos (position #\. hostname :start start)))
    (when pos
      (incf pos)
      (values (subseq hostname pos)
              pos))))

(defun make-subdomain-iter (hostname)
  (let ((current-pos 0)
        (first t))
    (lambda ()
      (block nil
        (when first
          (setq first nil)
          (return hostname))
        (multiple-value-bind (subdomain pos)
            (next-subdomain hostname current-pos)
          (when subdomain
            (setf current-pos pos)
            subdomain))))))

(defvar *etlds* nil)

(defun parse-domain (hostname)
  (dolist (tld (third *etlds*))
    (when (ends-with-subseq tld hostname)
      (if (= (length tld) (length hostname))
          (return-from parse-domain hostname)
          (when (char= (aref hostname (- (length hostname) (length tld) 1))
                       #\.)
            (return-from parse-domain
              (subseq hostname
                      (- (length hostname) (length tld))))))))
  (loop with iter = (make-subdomain-iter hostname)
        with pre-prev-subdomain = nil
        with prev-subdomain = nil
        for subdomain = (funcall iter)
        while subdomain
        if (gethash subdomain (second *etlds*)) do
          (return pre-prev-subdomain)
        else if (gethash subdomain (first *etlds*)) do
          (return (if (string= subdomain hostname)
                      nil
                      prev-subdomain))
        do (setf pre-prev-subdomain prev-subdomain
                 prev-subdomain subdomain)
        finally
           (let* ((pos (position #\. hostname :from-end t))
                  (pos (and pos
                            (position #\. hostname :from-end t :end pos))))
             (return
               (if pos
                   (subseq hostname (1+ pos))
                   hostname)))))

(defun uri-tld (uri)
  (let ((host (uri-host uri)))
    (when (and host
               (not (ip-addr-p host)))
      (let ((pos (position #\. host :from-end t)))
        (if pos
            (subseq host (1+ pos))
            host)))))

(defun uri-domain (uri)
  (let ((host (uri-host uri)))
    (when (and host
               (not (ip-addr-p host)))
      (parse-domain host))))

(defun ipv4-addr-p (host)
  (declare (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-type host string)
  (flet ((read-byte-string (string start)
           (declare (type fixnum start))
           (when (<= (length string) start)
             (return-from read-byte-string nil))
           (let* ((end (+ start 2))
                  (endp (<= (1- (length string)) end))
                  (end (if endp
                           (1- (length string))
                           end))
                  (res 0))
             (declare (type fixnum end res))
             (do ((i start (1+ i)))
                 ((< end i))
               (declare (type fixnum i))
               (unless (char<= #\0 (aref string i) #\9)
                 (return-from read-byte-string
                   (if (= i start)
                       nil
                       (values res i nil))))
               (setq res
                     (+ (* res 10)
                        (- (char-code (aref string i)) 48))))
             (cond
               (endp
                (values res end t))
               ((char= (aref string (1+ end)) #\.)
                (values res (1+ end) nil))))))
    (let ((start 0))
      (dotimes (i 4 t)
        (multiple-value-bind (byte pos endp)
            (read-byte-string host start)
          (unless (typep byte '(unsigned-byte 8))
            (return nil))
          (unless (xor endp (not (= i 3)))
            (return nil))
          (setq start (1+ pos)))))))

(defun trim-brackets (host)
  (if (char= (aref host 0) #\[)
      (if (char= (aref host (1- (length host))) #\])
          (subseq host 1 (1- (length host)))
          nil)
      host))

(defun ipv6-addr-p (host)
  (declare (optimize (speed 3) (safety 2))
           #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-type host string)
  (when (= (length host) 0)
    (return-from ipv6-addr-p nil))

  (labels ((read-section (string start &optional read-colons)
             (declare (type string string)
                      (type fixnum start))
             (when (<= (length string) start)
               (return-from read-section
                 (values start read-colons t)))
             (when (char= (aref string start) #\:)
               (cond
                 ((<= (length string) (1+ start))
                  (return-from read-section nil))
                 ((char= (aref string (1+ start)) #\:)
                  (if read-colons
                      (return-from read-section nil)
                      (return-from read-section (read-section string (+ 2 start) t))))
                 (t (incf start))))
             (let* ((end (+ start 4))
                    (endp (<= (length string) end))
                    (end (if endp
                             (length string)
                             end)))
               (declare (type fixnum end))

               (do ((i start (1+ i)))
                   ((= end i))
                 (let ((ch (aref string i)))
                   (cond
                     ((char= ch #\:)
                      (return-from read-section
                        (values i read-colons nil)))
                     ((or (char<= #\0 ch #\9)
                          (char<= #\a ch #\f)
                          (char<= #\A ch #\F)))
                     (t (return-from read-section nil)))))

               (if endp
                   (values end read-colons endp)
                   (if (char= (aref string end) #\:)
                       (values end read-colons endp)
                       nil)))))

    (setq host (trim-brackets host))
    (unless host
      (return-from ipv6-addr-p nil))

    (let ((start 0)
          (read-colons-p nil))
      (dotimes (i 8 t)
        (multiple-value-bind (e read-colons endp)
            (read-section host start read-colons-p)
          (unless e
            (return-from ipv6-addr-p nil))
          (when endp
            (when (and (not (= i 7))
                       (not read-colons))
              (return-from ipv6-addr-p nil))
            (return-from ipv6-addr-p t))
          (when (and (= i 7) (not endp))
            (return-from ipv6-addr-p nil))
          (setq start e
                read-colons-p read-colons))))))

(defun ip-addr-p (host)
  (or (ipv4-addr-p host)
      (ipv6-addr-p host)))

(defun ip-addr= (ip1 ip2)
  (flet ((parse-ipv6 (ip)
           (setq ip (trim-brackets ip))
           (cond
             ((char= (aref ip 0) #\:)
              (setq ip (concatenate 'string "0" ip)))
             ((char= (aref ip (1- (length ip))) #\:)
              (setq ip (concatenate 'string ip "0"))))
           (let* ((ip-parsed (split-sequence #\: ip))
                  (len (length ip-parsed)))
             (loop for section in ip-parsed
                   if (string= section "")
                     append (make-list (- 9 len) :initial-element 0)
                   else
                     collect (parse-integer section :radix 16)))))
    (cond
      ((ipv4-addr-p ip1)
       (string= ip1 ip2))
      ((ipv6-addr-p ip1)
       (and (ipv6-addr-p ip2)
            (equal (parse-ipv6 ip1)
                   (parse-ipv6 ip2)))))))

(defun cookie-domain-p (domain cookie-domain)
  (unless cookie-domain
    (return-from cookie-domain-p t))
  (if (ip-addr-p domain)
      (ip-addr= domain cookie-domain)
      (progn
        ;; ignore the preceding "."
        (when (char= (aref cookie-domain 0) #\.)
          (setq cookie-domain (subseq cookie-domain 1)))
        (when-let ((registered-domain (parse-domain domain)))
          (cond
            ((= (length registered-domain) (length cookie-domain))
             (string= registered-domain cookie-domain))
            ((= (length domain) (length cookie-domain))
             (string= domain cookie-domain))
            (t (and (ends-with-subseq domain cookie-domain)
                    (char= #\.
                           (aref cookie-domain (- (length cookie-domain)
                                                  (length registered-domain)))))))))))
