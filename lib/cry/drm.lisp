;;; drm.lisp --- Utilities for working with DRM content

;; 

;;; Commentary:

#|

The unauthorized distribution, sharing, and modification of digital content
are covered by copyright laws, but monitoring the internet to prevent illegal
activity is a challenging task. DRM addresses this by putting barriers in
place to prevent digital content from being stolen.

DRM typically involves the use of codes that prohibit content copying or limit
the number of devices a product can be accessed from. Content creators can
also use applications to restrict what users can do with their material or
encrypt digital media, which can then only be accessed by anyone with the
decryption key.

This enables content creators and copyright holders to: 

- Prevent or restrict users from editing or saving, sharing or forwarding,
  printing, or taking screenshots or screengrabs of their content or products

- Set expiry dates on media, which prevents access to users beyond that date
  or limits the number of times they can access it

- Limit media access to specific devices, Internet Protocol (IP) addresses, or
  locations, such as limiting content to people in the U.S. only

- Watermark documents and images to assert ownership and identity of content

|#
;; ref: https://en.wikipedia.org/wiki/Digital_rights_management

;;;; Widevine:

#|
Widevine DRM is Google’s content protection system for premium media and is
used by major partners globally.
|#

;; ref: https://developers.google.com/widevine

;; ref: https://github.com/DevLARLEY/WidevineProxy2

;; ref: remote-cdm exmaple https://remote-cdm.cdrm-project.com/remote_cdm

;; ref: https://forum.videohelp.com/forums/48-Video-Streaming-Downloading

;;;; PlayReady:

;; ref: https://learn.microsoft.com/en-us/playready/

;;;; FairPlay:

;; ref: https://developer.apple.com/streaming/fps/

;;; Code:
(in-package :cry/drm)
