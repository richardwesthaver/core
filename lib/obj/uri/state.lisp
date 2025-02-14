;;; obj/uri/state.lisp --- Parser state

;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A note about parser naming conventions.
;; There are two types of functions, where <name> comes from the LHS
;; of the ABNF grammar:
;;  state-<name> :: scan and return values based on the parse. The
;;      first value is always the "next" index beyond the parse.
;;      The subsequent values are rule specific, and documented in
;;      the functions themselves.
;;  scan-<name>  :: scan for and return either nil or an index.  If
;;      there is match, return the "next" index beyond the match,
;;      and nil otherwise.
;;
;; Rules marked `TERMINAL' must check for `at-end-p', since they must
;; terminate the parse for the input to be valid.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :obj/uri)

(defun state-uri (string start end
                  &aux i scheme userinfo host port path query fragment
                       nid nss q-component f-component r-component i2
                       colon urn-scheme file-scheme)
  ;; rule 01: URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
  ;; --TERMINAL--
  ;; values: i scheme userinfo host port path query fragment
  (if* (and (multiple-value-setq (i scheme) (state-scheme string start end))
            (setq colon (looking-at #\: string i end))
            (not (setq urn-scheme (looking-at "urn" string start end t)))
            (not (setq file-scheme (looking-at "file" string start end t)))
            (multiple-value-setq (i2 userinfo host port path)
              (state-hier-part string (1+ i) end)))
     then ;; Have hier-part...
          (setq i i2)
          (when (at-end-p i end)
            (return-from state-uri
              (values i scheme userinfo host port path)))

          (when (looking-at #\? string i end)
            (if* (multiple-value-setq (i2 query)
                   (state-query string (incf i) end))
               then (setq i i2)
               else (setq query #.*uri-null-marker*)))

          (when (looking-at #\# string i end)
            (if* (multiple-value-setq (i2 fragment)
                   (state-fragment string (incf i) end))
               then (setq i i2)
               else (setq fragment #.*uri-null-marker*)))

          (when (at-end-p i end)
            (values i scheme userinfo host port path query fragment))
   elseif urn-scheme
     then ;; values: i "urn" nid r-component nil nss q-component f-component
          (when (multiple-value-setq (i nid nss q-component f-component
                                      r-component)
                  (state-urn-namestring string i end))
            (values i
                    scheme
                    r-component		;userinfo
                    nid			;host
                    nil			;port
                    nss			;path
                    q-component		;query
                    f-component		;fragment
                    ))
   elseif (and file-scheme
               (multiple-value-setq (i path)
                 (state-uri-file string colon end)))
     then (values i scheme nil nil nil path)
   elseif (and scheme colon)
     then ;; Something like "mailto:foo@bar.com".  Put the
          ;; the non-scheme part into the path
          (values end scheme nil nil nil (xsubseq colon end))))

;; called by parse-uri-string-rfc3986
(defun state-uri-reference (string start end
                            &aux i scheme userinfo host port path query
                                 fragment)
  ;; rule 02: URI-reference = URI / relative-ref
  ;; values: i scheme host userinfo port path query fragment
  (if* (multiple-value-setq (i scheme userinfo host port path query
                             fragment)
         (state-uri string start end))
     then (values i scheme userinfo host port path query fragment)
   elseif (multiple-value-setq (i userinfo host port path query fragment)
            (state-relative-ref string start end))
     then (values i nil userinfo host port path query fragment)))

;; called by parse-uri-string-rfc3986
(defun state-absolute-uri (string start end
                           &aux i scheme userinfo host port path query i2
                                colon urn-scheme file-scheme)
  ;; rule 03: absolute-URI  = scheme ":" hier-part [ "?" query ]
  ;; --TERMINAL--
  ;; values: i scheme userinfo host port path query
  (if* (and (multiple-value-setq (i scheme) (state-scheme string start end))
            (setq colon (looking-at #\: string i end))
            (not (setq urn-scheme (looking-at "urn" string start end t)))
            (not (setq file-scheme (looking-at "file" string start end t)))
            (multiple-value-setq (i2 userinfo host port path)
              (state-hier-part string colon end)))
     then ;; so far: scheme + ":" + hier-part
          (setq i i2)
          (if* (at-end-p i end)
             then (values i scheme userinfo host port path)
           elseif (and (looking-at #\? string i end)
                       (multiple-value-setq (i query)
                         (state-query string (incf i) end))
                       (at-end-p i end))
             then (values i scheme userinfo host port path query))
   elseif urn-scheme
     then ;; values: i "urn" nid r-component nil nss q-component f-component
          (multiple-value-bind (i3 nid nss q-component f-component r-component)
              (state-urn-namestring string (incf i) end)
            (when i3
              (values i3
                      scheme
                      r-component	;userinfo
                      nid		;host
                      nil		;port
                      nss		;path
                      q-component	;query
                      f-component	;fragment
                      )))
   elseif (and file-scheme
               (multiple-value-setq (i path)
                 (state-uri-file string colon end)))
     then (values i scheme nil nil nil path)
   elseif (and scheme colon)
     then  ;; Something like "mailto:foo@bar.com".  Put the
          ;; the non-scheme part into the path
          (values end scheme nil nil nil (xsubseq colon end))))

(defun state-hier-part (string start end &aux i userinfo host port
                                              path i2)
  ;; rule 04: hier-part = "//" authority path-abempty
  ;;                    / "//" path-absolute            ***NEW***
  ;;                    / path-absolute
  ;;                    / path-rootless
  ;;                    / path-empty
  ;; values: i userinfo host port path
  (if* (and (setq i (looking-at "//" string start end))
            (multiple-value-setq (i userinfo host port)
              (state-authority string i end)))
     then (if* (multiple-value-setq (i2 path) (state-path-abempty string i end))
             then (values i2 userinfo host port path)
             else (values i userinfo host port))
   elseif (and (setq i (looking-at "//" string start end))
               (multiple-value-setq (i path)
                 (state-path-absolute string i end)))
     then (values i nil nil nil path)
   elseif (or
           (multiple-value-setq (i path) (state-path-absolute string start end))
           (multiple-value-setq (i path) (state-path-rootless string start end))
           (multiple-value-setq (i path) (state-path-empty string start end)))
     then (values i nil nil nil path)))

(defun state-relative-ref (string start end &aux i2 query fragment)
  ;; rule 05: relative-ref = relative-part [ "?" query ] [ "#" fragment ]
  ;; --TERMINAL--
  ;; values: i userinfo host port path query fragment
  (multiple-value-bind (i userinfo host port path)
      (state-relative-part string start end)
    (when i
      (if* (at-end-p i end)
         then (values i userinfo host port path)
         else (when (looking-at #\? string i end)
                (if* (multiple-value-setq (i2 query)
                       (state-query string (incf i) end))
                   then (setq i i2)
                   else (setq query #.*uri-null-marker*)))

              (when (looking-at #\# string i end)
                (if* (multiple-value-setq (i2 fragment)
                       (state-fragment string (incf i) end))
                   then (setq i i2)
                   else (setq fragment #.*uri-null-marker*)))

              (when (at-end-p i end)
                (values i userinfo host port path query fragment))))))

(defun state-relative-part (string start end
                            &aux (i start) path userinfo host port i2)
  ;; rule 06: relative-part = "//" authority path-abempty
  ;;                        / path-absolute
  ;;                        / path-noscheme
  ;;                        / path-empty
  ;; values: i userinfo host port path
  (if* (and (setq i (looking-at "//" string i end))
            (multiple-value-setq (i userinfo host port)
              (state-authority string i end)))
     then (if* (multiple-value-setq (i2 path) (state-path-abempty string i end))
             then (values i2 userinfo host port path)
             else (values i userinfo host port))
   elseif (or
           (multiple-value-setq (i path) (state-path-absolute string start end))
           (multiple-value-setq (i path) (state-path-noscheme string start end))
           (multiple-value-setq (i path) (state-path-empty string start end)))
     then (values i nil nil nil path)))

(defun state-scheme (string start end &aux i scheme)
  ;; rule 07: scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
  ;; values: i scheme
  (when (looking-at *alpha-bitvector* string start end)
    (if* (setq i (scan-forward string (1+ start) end *scheme-bitvector*))
       then (setq scheme (xsubseq start i))
       else ;; just the one char
            (setq scheme (xsubseq start (setq i (1+ start)))))
    (values i scheme)))

(defun state-authority (string start end &aux i i2 userinfo host ipv6 zone-id
                                              port)
  ;; rule 08: authority = [ userinfo "@" ] host [ ":" port ]
  ;; values: i userinfo host port
  (cond
   ((and (multiple-value-setq (i userinfo) (state-userinfo string start end))
         (setq i (looking-at #\@ string i end))
         (multiple-value-setq (i host ipv6 zone-id)
           (state-host string i end)))
    ;; Somewhat of a hack, but I don't want to change all the functions
    ;; to expect even more multiple values:
    (when ipv6 (setq host (list host ipv6 zone-id)))

    ;; have: userinfo "@" host
    (if* (not (setq i2 (looking-at #\: string i end)))
       then ;; done, return what we have
            (values i userinfo host)
     elseif (multiple-value-setq (i port) (state-port string i2 end))
       then ;; found ":" and port
            (values i userinfo host port)
       else ;; found ":" and NO port
            (values i2 userinfo host)))

   ;; no userinfo, check for host
   ((multiple-value-setq (i host ipv6 zone-id) (state-host string start end))
    ;; Somewhat of a hack, but I don't want to change all the functions
    ;; to expect even more multiple values:
    (when ipv6 (setq host (list host ipv6 zone-id)))

    (if* (not (setq i2 (looking-at #\: string i end)))
       then (values i nil host)
     elseif (multiple-value-setq (i port) (state-port string i2 end))
       then (values i nil host port)
       else ;; found ":" and NO port
            (values i2 nil host)))))

(defun state-userinfo (string start end &aux i)
  ;; rule 09: userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
  ;; 
  ;; This one is more difficult, due to the alternation with
  ;; pct-encoded:
  ;;  *( unreserved / pct-encoded / sub-delims / ":" )
  ;; All the others are just characters, but pct-encoded is a
  ;; specific sequence of characters.
  (when (setq i (scan-forward string start end *userinfo-bitvector*
                              #'scan-pct-encoded))
    (values i (xsubseq start i))))

(defun state-port (string start end &aux i)
  ;; rule 11: port = *DIGIT
  (when (setq i (scan-forward string start end *digit-bitvector*))
    (values i (xsubseq start i))))

(defun state-host (string start end &aux i host ipv6 zone-id)
  ;; rule 10: host = IP-literal / IPv4address / reg-name
  ;; values: i host ipv6 zone-id
  (if* (multiple-value-setq (i ipv6 zone-id)
         (state-ip-literal string start end))
     then (values i nil ipv6 zone-id)
   elseif (or
           (multiple-value-setq (i host) (state-ipv4address string start end))
           (multiple-value-setq (i host) (state-reg-name string start end)))
     then (values i host)))

(defun state-ip-literal (string start end &aux ip-start i2 end-ip ip zone-id)
  ;; rule 12a: IP-literal = "[" ( IPv6addrz / IPvFuture  ) "]"
  ;; values: i ipaddr zone-id
  ;; NOTE: the [ and ] are not returned as part of the host.
  (when (and (setq ip-start (looking-at #\[ string start end))
             (or (multiple-value-setq (end-ip ip zone-id)
                   (state-ipv6addrz string ip-start end))
                 (multiple-value-setq (end-ip ip zone-id)
                   (state-ipvfuture string ip-start end)))
             (setq i2 (looking-at #\] string end-ip end)))
    (values i2 ip zone-id)))

(defun state-ipv6addrz (string start end &aux ip-end zone-start zone-end)
  ;; rule 12b: IPv6addrz = IPv6address [ "%25" ZoneID ]
  ;; values: i ipaddr zone-id
  (when (setq ip-end (scan-ipv6address string start end))
    (if* (and (setq zone-start (looking-at "%25" string ip-end end))
              (setq zone-end (scan-zone-id string zone-start end)))
       then (values zone-end
                    (xsubseq start ip-end)
                    (xsubseq zone-start zone-end))
       else (values ip-end (xsubseq start ip-end)))))

(defun scan-zone-id (string start end)
  ;; rule 12c: ZoneID  = 1*( unreserved / pct-encoded )
  (scan-forward string start end *unreserved-bitvector* #'scan-pct-encoded))

(defun state-ipvfuture (string start end &aux i)
  ;; rule 13:
  ;;    IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
  ;; values: i ipvfuture
  (when (and (setq i (looking-at #\v string start end))
             (setq i (scan-forward string i end *hexdig-bitvector*))
             (setq i (looking-at #\. string i end))
             (setq i (scan-forward string i end *ipvfuture-bitvector*)))
    (values i (xsubseq start i))))

(defun scan-ipv6address (string start end &aux (i start))
  ;; rule 14:
  ;;  IPv6address =                            6( h16 ":" ) ls32  [1]
  ;;              /                       "::" 5( h16 ":" ) ls32  [2]
  ;;              / [               h16 ] "::" 4( h16 ":" ) ls32  [3]
  ;;              / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32  [4]
  ;;              / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32  [5]
  ;;              / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32  [6]
  ;;              / [ *4( h16 ":" ) h16 ] "::"              ls32  [7]
  ;;              / [ *5( h16 ":" ) h16 ] "::"              h16   [8]
  ;;              / [ *6( h16 ":" ) h16 ] "::"                    [9]
  ;;              /                       "::"                    [10]
  (or
   (and (setq i (scan-h16-colon-pairs string start end 6 6)) ;; [1]
        (setq i (scan-ls32 string i end)))
   (and (setq i (looking-at "::" string start end))          ;; [2]
        (setq i (scan-h16-colon-pairs string i end 5 5))
        (setq i (scan-ls32 string i end)))
   (and (setq i (scan-h16 string start end))                 ;; [3]
        (setq i (looking-at "::" string i end))
        (setq i (scan-h16-colon-pairs string i end 4 4))
        (setq i (scan-ls32 string i end)))
   (setq i (scan-ipv6address-part4 string start end))        ;; [4]
   (setq i (scan-ipv6address-part5 string start end))        ;; [5]
   (setq i (scan-ipv6address-part6 string start end))        ;; [6]
   (setq i (scan-ipv6address-part7 string start end))        ;; [7]
   (setq i (scan-ipv6address-part8 string start end))        ;; [8]
   (and (setq i (scan-h16-colon-pairs string start end 0 6)) ;; [9]
        (setq i (scan-h16 string i end))
        (setq i (looking-at "::" string i end)))
   (setq i (looking-at "::" string start end))               ;; [10]
   ))

(defun scan-ipv6address-part4 (string start end &aux i)
  ;; rule: [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  (or (and (setq i (looking-at "::" string start end))
           (setq i (scan-h16-colon-pairs string i end 3 3))
           (setq i (scan-ls32 string i end)))

      (and (setq i (scan-h16-colon-pairs string start end 0 1))
           (setq i (scan-h16 string i end))
           (setq i (looking-at "::" string i end))
           (setq i (scan-h16-colon-pairs string i end 3 3))
           (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part5 (string start end &aux i)
  ;; rule: [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  (or (and (setq i (looking-at "::" string start end))
           (setq i (scan-h16-colon-pairs string i end 2 2))
           (setq i (scan-ls32 string i end)))

      (and (setq i (scan-h16-colon-pairs string start end 0 2))
           (setq i (scan-h16 string i end))
           (setq i (looking-at "::" string i end))
           (setq i (scan-h16-colon-pairs string i end 2 2))
           (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part6 (string start end &aux i)
  ;; rule: [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  (or (and (setq i (looking-at "::" string start end))
           (setq i (scan-h16 string i end))
           (setq i (looking-at #\: string i end))
           (setq i (scan-ls32 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 3))
           (setq i (scan-h16 string i end))
           (setq i (looking-at "::" string i end))
           (setq i (scan-h16 string i end))
           (setq i (looking-at #\: string i end))
           (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part7 (string start end &aux i)
  ;; rule: [ *4( h16 ":" ) h16 ] "::"              ls32
  (or (and (setq i (looking-at "::" string start end))
           (setq i (scan-ls32 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 4))
           (setq i (scan-h16 string i end))
           (setq i (looking-at "::" string i end))
           (setq i (scan-ls32 string i end)))))

(defun scan-ipv6address-part8 (string start end &aux i)
  ;; rule: [ *5( h16 ":" ) h16 ] "::"              h16
  (or (and (setq i (looking-at "::" string start end))
           (setq i (scan-h16 string i end)))
      (and (setq i (scan-h16-colon-pairs string start end 0 5))
           (setq i (scan-h16 string i end))
           (setq i (looking-at "::" string i end))
           (setq i (scan-h16 string i end)))))

(defun scan-h16-colon-pairs (string start end min max
                             &aux (i start)
                                  i2
                                  (nfound 0))
  ;; subrule: min*max( h16 ":" )
  ;; Scan from min to max pairs of: h16 + ":"
  ;; NOTE: this function needs to lookahead to make sure there isn't a ::
  ;;       after the h16.
  (loop while (and (< nfound max)
                   (setq i2 (scan-h16 string i end))
                   (setq i2 (looking-at #\: string i2 end))
                   (< i2 end)
                   (not (looking-at #\: string i2 end)))
              do 
                 (setq i i2)
                 (incf nfound))
  (when (<= min nfound max)
    i))

(defun scan-h16 (string start end &aux i)
  ;; rule 15: h16 = 1*4HEXDIG
  (when (null start) (error "start is null"))
  (when (and (setq i
               (scan-forward string start
                             ;; only look 5 ahead
                             (min end (+ start 5))
                             *hexdig-bitvector*))
             (<= 1 (the fixnum (- i start)) 4))
    i))

(defun scan-ls32 (string start end &aux i)
  ;; rule 16: ls32          = ( h16 ":" h16 ) / IPv4address
  (if* (and (setq i (scan-h16 string start end))
            (setq i (looking-at #\: string i end))
            (setq i (scan-h16 string i end)))
     then i
     else (scan-ipv4address string start end)))

(defun scan-ipv4address (string start end &aux i)
  ;; rule 17:
  ;;  IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
  ;; values: i
  (and (setq i (scan-dec-octet string start end))
       (setq i (looking-at #\. string i end))
       (setq i (scan-dec-octet string i end))
       (setq i (looking-at #\. string i end))
       (setq i (scan-dec-octet string i end))
       (setq i (looking-at #\. string i end))
       (scan-dec-octet string i end)))

(defun state-ipv4address (string start end &aux i)
  ;; values: i ipv4
  (when (setq i (scan-ipv4address string start end))
    (values i (xsubseq start i))))

(defun scan-dec-octet (string start end &aux i)
  ;; rule 18:
  ;;   dec-octet     = DIGIT                 ; 0-9
  ;;                 / %x31-39 DIGIT         ; 10-99
  ;;                 / "1" 2DIGIT            ; 100-199
  ;;                 / "2" %x30-34 DIGIT     ; 200-249
  ;;                 / "25" %x30-35          ; 250-255
  ;; Honestly, the above makes little sense to me.  The truth is,
  ;; "http://256.0.0.1/" is a valid URI because even though it doesn't
  ;; parse as a dec-octet, it does parse as a reg-name (rule 19).
  (when (and (setq i (scan-forward string start end *digit-bitvector*))
             (<= 1 (- i start) 3))
    i))

(defun state-reg-name (string start end &aux i)
  ;; rule 19: reg-name      = *( unreserved / pct-encoded / sub-delims )
  ;; values: i host
  (when (setq i (scan-forward string start end *reg-name-bitvector*
                              #'scan-pct-encoded))
    (values i (xsubseq start i))))

(defun state-path-abempty (string start end &aux i i2)
  ;; rule 21: path-abempty  = *( "/" *pchar )
  ;; values: i path
  ;; NOTE: if *strict-parse* is nil, we allow the leading "/" to be "//",
  ;;       because it is a common typo in HTML and sometimes fixing it is
  ;;       not under our control.  Browsers work fine with this
  ;;       non-conformance.
  (when (and (not *strict-parse*)
             (looking-at "//" string start end))
    ;; double leading slash is changed to a single leading slash.
    (incf start))
  (setq i start)
  (loop
    (setq i2 nil)
    (if* (looking-at #\/ string i end)
       then (if* (setq i2 (scan-pchar string (1+ i) end))
               then (setq i i2)
               else (incf i) ;; advance for the / we found
                    (return))
       else (return)))
  (when (> i start)
    (values i (xsubseq start i))))

(defun state-path-absolute (string start end &aux (i start) i2 have-slash)
  ;; rule 22: path-absolute = "/" [ 1*pchar *( "/" *pchar ) ]
  ;;   remember: [ foo ] means 0*1( foo )
  ;; values: i path
  (when (setq i (looking-at #\/ string i end))
    (when (setq i2 (scan-pchar string i end))
      ;; parse is good to here
      (setq i i2
            i2 nil)
      ;; Now, look for *( "/" *pchar )
      (loop while (and (setq have-slash (looking-at #\/ string i end))
                       (setq i2 (scan-pchar string have-slash end)))
            do (setq i i2))
      ;; If it ends with a /:
      (when (and have-slash (not i2)) (incf i)))
    (values i (xsubseq start i))))

(defun state-path-noscheme (string start end &aux (i start) i2 have-slash)
  ;; rule 23: path-noscheme = segment-nz-nc *( "/" *pchar )
  ;; values: i path
  (when (setq i (scan-segment-nz-nc string i end))
    (loop while (and (setq have-slash (looking-at #\/ string i end))
                     (setq i2 (scan-pchar string (1+ i) end)))
          do (setq i i2))
    (when (and have-slash (not i2))
      ;; for the slash we did see:
      (incf i))
    (values i (xsubseq start i))))

(defun state-path-rootless (string start end &aux (i start) i2)
  ;; rule 24: path-rootless = 1*pchar *( "/" *pchar )
  ;; values: i path
  (when (setq i (scan-pchar string i end))
    (loop while (and (looking-at #\/ string i end)
                     ;; The pchar after the slash is optional
                     (setq i2 (or (scan-pchar string (1+ i) end)
                                  (1+ i))))
          do (setq i i2))
    (values i (xsubseq start i))))

(defun state-path-empty (string start end)
  ;; rule 25: path-empty    = 0<pchar>
  ;; values: i path
  ;; NOTE: the RHS was updated in RFC 3986 errata to be "", but that is
  ;;       bogus. "" is very different the 0<pchar>.
  ;; Return nil when looking at a `pchar' and the null marker otherwise.
  (declare (optimize (safety 0))) 
  (if* (looking-at *pchar-bitvector* string start end)
     then nil
     else (values start #.*uri-null-marker*)))

(defun scan-segment-nz-nc (string start end)
  ;; rule 28: 1*( unreserved / pct-encoded / sub-delims / "@" )
  ;; In english: pchar without #\:
  (declare (optimize (safety 0))) 
  (scan-forward string start end *segment-nz-nc-bitvector* #'scan-pct-encoded))

(defun scan-pchar (string start end)
  ;; rule 29: pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
  (declare (optimize (safety 0))) 
  (scan-forward string start end *pchar-bitvector* #'scan-pct-encoded))

(defun state-query (string start end &aux i)
  ;; rule 30: *( pchar / "/" / "?" )
  ;; values: i query
  (when (setq i
          (scan-forward string start end
                        (if* *strict-parse*
                           then *query-bitvector-strict*
                           else *query-bitvector-non-strict*)
                        #'scan-pct-encoded))
    (values i (xsubseq start i))))

(defun state-fragment (string start end &aux i)
  ;; rule 31: *( pchar / "/" / "?" / "#" )
  ;;   NOTE: Allegro CL added "#" in non-strict mode
  ;; values: i fragment
  (when (setq i
          (scan-forward string start end
                        (if* *strict-parse*
                           then *fragment-bitvector-strict*
                           else *fragment-bitvector-non-strict*)
                        #'scan-pct-encoded))
    (values i (xsubseq start i))))

(defvar .pct-encoded. nil)

(defun scan-pct-encoded (string start end)
  ;; This scans a single percent encoded sequence. It does no conversion.
  ;; It also sets .pct-encoded., which is a boolean that says "this string
  ;; has some percent encoded characters in it."
  ;;
  ;; rule 32: pct-encoded   = "%" HEXDIG HEXDIG
  (declare (fixnum start end))
  (and (> (the fixnum (- end start)) 2) ;; ... at least 3 chars remaining
       (looking-at #\% string start end)
       (looking-at *hexdig-bitvector* string (incf start) end)
       (looking-at *hexdig-bitvector* string (incf start) end)
       (setq .pct-encoded. start)))

(defun state-uri-file (string start end &aux i)
  ;; rule: uri-file = "//" <anything>
  ;; --TERMINAL--
  ;; values: i path
  ;; It's not the job of the URI parser to validate file:// URIs.
  (when (setq i (looking-at "//" string start end))
    (values i (xsubseq i end))))

(defun state-urn-namestring (string start end
                  &aux (i start) i2 nid nss q-component f-component
                       r-component)
  ;; rule 50: namestring  = assigned-name
  ;;                      [ rq-components ]
  ;;                      [ "#" f-component ]
  ;; rule 58: f-component = fragment
  ;; START is just after "urn:".
  ;; values: i nid nss q-component f-component r-component
  (when (multiple-value-setq (i2 nid nss)
          (state-urn-assigned-name string start end))
    (when (at-end-p i2 end)
      (return-from state-urn-namestring (values i2 nid nss)))

    (setq i i2)
    (when (multiple-value-setq (i2 r-component q-component)
            (state-urn-rq-components string i end))
      (when (at-end-p i2 end)
        (return-from state-urn-namestring
          (values i2 nid nss q-component nil r-component)))
      (setq i i2)
      ;; more STRING to process...

      (when (looking-at #\# string i end)
        (if* (multiple-value-setq (i2 f-component)
               ;; Yes, the same fragment (RFC 8141 defines f-component in
               ;; terms of RFC 3986's fragment).
               (state-fragment string (incf i) end))
           then (setq i i2)
           else (setq f-component #.*uri-null-marker*)))

      (when (at-end-p i end)
        (values i2 nid nss q-component f-component r-component)))))

(defun state-urn-assigned-name (string start end &aux i i2 nid nss)
  ;; rule 51: assigned-name = "urn" ":" NID ":" NSS
  ;; START is just after "urn:".
  ;; values: i nid nss
  (when (and (multiple-value-setq (i2 nid) (state-urn-nid string start end))
             (looking-at #\: string i2 end)
             (setq i (1+ i2))
             (multiple-value-setq (i2 nss) (state-urn-nss string i end)))
    (values i2 nid nss)))

(defun state-urn-nid (string start end &aux (i start))
  ;; rule 52: NID = (alphanum) 0*30(ldh) (alphanum)
  ;; rule 53: ldh = alphanum / "-"
  ;; values: i nid
  (declare (fixnum start end i))
  (when (and (looking-at *alphanum-bitvector* string i end)
             (setq i (scan-forward string (1+ i) end *alphanum+-bitvector*))
             ;; Check for <= 32 chars, thus far
             (<= (the fixnum (- i start))
                 32)
             ;; If the last one was alphanum, then we're done.
             ;; If the last one was NOT alphanum, then:
             ;;   1. make sure we had 30 chars (not 31)
             ;;   2. look for another, single alphanum
             (or (looking-at *alphanum-bitvector* string (1- i) end)
                 (and (<= (the fixnum (- i start))
                          31)
                      (not (at-end-p i end))
                      (setq i
                        (scan-forward string i end *alphanum-bitvector*)))))
    (values i (xsubseq start i))))

(defun state-urn-nss (string start end &aux i i2)
  ;; rule 54: NSS = pchar *(pchar / "/")
  ;; values: i nss
  (when (setq i (scan-pchar string start (1+ start)))
    (if* (setq i2 (scan-forward
                   string i end
                   ;; See the definition of *urn-nss-chars* for
                   ;; why we don't use *pchar/-bitvector* here.
                   *urn-nss-bitvector*
                   #'scan-pct-encoded))
       then (values i2 (xsubseq start i2))
       else (values i (xsubseq start i)))))

(defun state-urn-rq-components (string start end
                                &aux i ri qi r-component q-component)
  ;; rule 55: rq-components = [ "?+" r-component ]
  ;;                          [ "?=" q-component ]
  ;; values: i r-component q-component
  (when (and (setq i (looking-at #\? string start end))
             (not (at-end-p i end))
             (or (setq ri (looking-at #\+ string i end))
                 (setq qi (looking-at #\= string i end)))
             (not (at-end-p (or ri qi) end)))
    (when (and ri (multiple-value-setq (i r-component)
                    (state-urn-r-component string ri end)))
      (when (at-end-p i end)
        (return-from state-urn-rq-components
          (values i r-component)))

      (if* (setq qi (looking-at #\? string i end))
         then (when (and (not (at-end-p qi end))
                         (setq qi (looking-at #\= string qi end))
                         (not (at-end-p qi end)))
                (when (multiple-value-setq (i q-component)
                        (state-urn-q-component string qi end))
                  (return-from state-urn-rq-components
                    (values i r-component q-component))))
         else (return-from state-urn-rq-components (values i r-component))))
    ;; The r-component branch didn't yield anything, check for q-component

    (when (and qi (multiple-value-setq (i q-component)
                    (state-urn-q-component string qi end)))
      (return-from state-urn-rq-components
        (values i nil q-component)))))

(defun scan-q-component-or-pct-encoded (string i end &aux i2)
  ;; Do what scan-pct-encoded does, BUT STOP scanning if we see "?=",
  ;; because that is the start of the q-component.
  ;;
  ;; This function is called by SCAN-FORWARD at each character position in
  ;; STRING.

  (when (setq i2 (scan-pct-encoded string i end))
    (return-from scan-q-component-or-pct-encoded i2))

  (when (setq i2 (looking-at #\? string i end))
    (if* (and
          ;; at least 2 chars remaining (for 1 char after ?=)
          (> (- end i2) 1)
          (looking-at #\= string i2 end))
       then ;; stop scanning
            (return-from scan-q-component-or-pct-encoded nil)
       else ;; return the index after the ?
            (return-from scan-q-component-or-pct-encoded i2))))

(defun state-urn-r-component (string start end &aux i i2)
  ;; rule 56: r-component   = pchar *( pchar / "/" / "?" )
  ;; values: i r-component
  (when (setq i (scan-pchar string start end))
    (when (at-end-p i end)
      (return-from state-urn-r-component
        (values i (xsubseq start i))))
    (cond
     ((setq i2
        (scan-forward
         string i end
         ;; NOTE: we don't use *query-bitvector-strict* because we need
         ;;       to handle #\? specially (see the next argument).
         *urn-query-bitvector*
         ;; NOTE: Because r-component can contain "?" without percent
         ;;       encoding, when processing the r-component we need to
         ;;       look ahead to make sure there is no #\= after each
         ;;       #\? (since that means we have a q-component).
         #'scan-q-component-or-pct-encoded))
      (values i2 (xsubseq start i2)))

     ;; We immediately ran into ?=, so return what we found so far:
     (t (values i (xsubseq start i))))))

(defun state-urn-q-component (string start end &aux i)
  ;; rule 57: q-component   = pchar *( pchar / "/" / "?" )
  ;; values: i q-component
  (when (setq i (looking-at *pchar-bitvector* string start end))
    (when (at-end-p i end)
      (return-from state-urn-q-component
        (values i (xsubseq start i))))
    (when (setq i
            (scan-forward string i end *query-bitvector-strict*
                          #'scan-pct-encoded))
      (values i (xsubseq start i)))))
