(defpackage :gui/x11
  (:shadowing-import-from :std/type :array-index)
  (:use :cl :std :gui/core :xlib)
  (:export
   #:display-extensions
   :open-default-display
   #:display-fonts
   #:init-x11
   #:*x11-display*))

(in-package :gui/x11)

(defvar *display* nil)
(defvar *screen* nil)
(defvar *window* nil)
(defvar *colormap* nil)
(defvar *font* nil)
(defvar *gcontext* nil)
(defvar *background* nil)
(defvar *palette* nil)
(defvar *black* nil)
(defvar *white* nil)

(defun init-x11 ()
  (setf *display* (xlib:open-default-display)
        *screen* (xlib:display-default-screen *display*)
        *colormap* (xlib:screen-default-colormap *screen*)
        *font* (xlib:open-font *display* "fixed")))

(defun init-window ()
  (setf
   *window* (xlib:create-window 
             :parent (xlib:screen-root *screen*) 
             :x 512 :y 512
             :width 200 :height 200
	     :background (xlib:alloc-color *colormap*
					   (xlib:lookup-color *colormap*
							      "midnightblue")))
   *gcontext* (xlib:create-gcontext 
               :drawable *window*
	       :background (xlib:screen-white-pixel *screen*)
	       :foreground (xlib:alloc-color *colormap*
					     (xlib:lookup-color
					      *colormap*
					      "yellow"))
	       :font *font*)
   *background* (xlib:create-gcontext
                 :drawable *window*
                 :fill-style :solid
                 :background (xlib:screen-white-pixel *screen*)
                 :foreground (xlib:alloc-color *colormap*
			                       (xlib:lookup-color *colormap*
						                  "midnightblue"))
                 :font *font*)
   *palette* nil
   *black* (xlib:screen-black-pixel *screen*)))
   
(xlib:map-window *window*)               

(defun display-fonts (&optional display (pattern "*"))
  (xlib:list-fonts (or display *default-display*) pattern))

(defun display-extensions (&optional display (result-type 'list))
  (xlib:list-extensions (or display *default-display*) :result-type result-type))
