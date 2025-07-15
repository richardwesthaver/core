(defpackage :gui
  (:shadowing-import-from :std/type :array-index)
  (:shadowing-import-from :xlib :draw-line)
  (:use :cl :std :gui/core :xlib)
  (:export
   #:display-extensions
   :open-default-display
   #:display-fonts
   #:init-xlib
   #:*xlib-display*))

(in-package :gui)

;; *palette*
(declaim (special *display* *screen* *window* *colormap* *font* *gcontext* *background* *black* *white*))

(defun init-xlib ()
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
   
(defun display-fonts (&optional display (pattern "*"))
  (xlib:list-fonts (or display *display*) pattern))

(defun display-extensions (&optional display (result-type 'list))
  (xlib:list-extensions (or display *display*) :result-type result-type))
