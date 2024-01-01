;;; lib/cli/ansi.lisp --- ANSI X3.64 Control Sequences

;; based on https://github.com/McParen/croatoan/tree/master

;; the tests for the original source are at the bottom of the file
;; (interactive).

;;; Code:
(in-package :cli/ansi)

;;; Basic terminal control functions based on 7bit escape sequences
;;; according to ANSI X3.64 / ECMA 48 / ISO/IEC 6429 / VT10X / XTerm

;; ECMA-6: 7bit character set 0-127
;; ECMA-35: Bit notation 01/07
;; ECMA-48: ANSI escape sequences

;; 1-char 7bit controls C0
;; 1-char 8bit controls C1
;; escape sequences
;; 7bit CSI sequences
;; 8bit CSI sequences

;; Acronym Character Decimal Octal  Hexadecimal Code
;; DEL     #\rubout  127     #o177  #x7f        07/15
;; ESC     #\esc      27     #o33   #x1b        01/11
;; SP      #\space    32     #o40   #x20        02/00

;; code x/y = column/line
;; 7bit code table = x-column 0-7 / y-line 0-15

;; x/y:        x       y
;; Bit:    7 6 5 4 3 2 1
;; Weight: 4 2 1 8 4 2 1

;; 200530 add a stream argument to every function
;; add windows as gray streams

;;(defmacro define-control-function ())
;;(defmacro define-control-sequence (name args))

;; ESC [ Pn1 ; Pn2 H
;; CSI Pn1 ; Pn2 H
;; CSI n ; m H
;; CUP
;; cursor-position

;; TODO 200530 write csi in terms of esc?
;; no because CSI params are separated with ; while esc params arent separated

;; See 5.4 for the overall format of control sequences

;; Set:        C1
;; Section:    8.3.16
;; Name:       Control Sequence Introducer
;; Mnemonic:   CSI
;; 7bit Chars: ESC [
;; 7bit Byte:  01/11 05/11
;; 8bit Byte:  09/11 (not used here)
(defparameter *csi* (coerce (list #\esc #\[) 'string)
  "A two-character string representing the 7bit control sequence introducer CSI.")

(defun esc (&rest params)
  "Write an ESC control sequence. The parameters are not separated."
  (format t "~A~{~A~}" #\esc params))

(defun csi (final-char &rest params)
  "Write a CSI control sequence. The params are separated by a semicolon."
  ;; only the params are separated with ; the other chars are not separated.
  ;; ~^; = add ; to every list item except the last
  (format t "~A~{~A~^;~}~A" *csi* params final-char))

;; Sequence Syntax
;; C   A single character
;; Ps  A single numeric parameter
;; Pm  Several numeric parameters Ps separated by a semicolon ;

;;; ESC sequences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Name:       Reset to initial state
;; Mnemonic:   RIS
;; Final char: c
;; Final byte: 06/03
;; Sequence:   ESC c
;; Parameters: none
;; Default:    none
;; Reference:  ANSI 5.72, ECMA 8.3.105
(defun reset-to-initial-state ()
  "Reset the terminal to its initial state.

In particular, turn on cooked and echo modes and newline translation,
turn off raw and cbreak modes, reset any unset special characters.

A reset is useful after a program crashes and leaves the terminal in
an undefined, unusable state."
  (esc "c"))

(setf (fdefinition '.ris) #'reset-to-initial-state)

;;; CSI sequences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Cursor control functions

;; Name:       Cursor up
;; Mnemonic:   CUU
;; Final char: A
;; Final byte: 04/01
;; Sequence:   CSI Pn A
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.17, ECMA 8.3.22
(defun cursor-up (&optional (m 1))
  "Move the cursor m lines up."
  (csi "A" m))

(setf (fdefinition '.cuu) #'cursor-up)

;; Name:       Cursor down
;; Mnemonic:   CUD
;; Final char: B
;; Final byte: 04/02
;; Sequence:   CSI Pn B
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.14, ECMA 8.3.19
(defun cursor-down (&optional (m 1))
  "Move the cursor m lines down."
  (csi "B" m))

(setf (fdefinition '.cud) #'cursor-down)

;; Name:       Cursor forward
;; Mnemonic:   CUF
;; Final char: C
;; Final byte: 04/03
;; Sequence:   CSI Pn C
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.15, ECMA 8.3.20
;; Notice:     ECMA name: Cursor right
(defun cursor-forward (&optional (n 1))
  "Move the cursor n columns in the forward direction (to the right)."
  (csi "C" n))

(setf (fdefinition '.cuf) #'cursor-forward)

;; Name:       Cursor backward
;; Mnemonic:   CUB
;; Final char: D
;; Final byte: 04/04
;; Sequence:   CSI Pn D
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.13, ECMA 8.3.18
;; Notice:     ECMA name: Cursor left
(defun cursor-backward (&optional (n 1))
  "Move the cursor n columns in the backward direction (to the left)."
  (csi "D" n))

(setf (fdefinition '.cub) #'cursor-backward)

;; Name:       Cursor next line
;; Mnemonic:   CNL
;; Final char: E
;; Final byte: 04/05
;; Sequence:   CSI Pn E
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.7, ECMA 8.3.12
(defun cursor-next-line (&optional (m 1))
  "Move the cursor m columns down to column 1."
  (csi "E" m))

(setf (fdefinition '.cnl) #'cursor-next-line)

;; Name:       Cursor preceding line
;; Mnemonic:   CPL
;; Final char: F
;; Final byte: 04/06
;; Sequence:   CSI Pn F
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.8, ECMA 8.3.13
(defun cursor-preceding-line (&optional (m 1))
  "Move the cursor m columns up to column 1."
  (csi "F" m))

(setf (fdefinition '.cpl) #'cursor-preceding-line)

;; Name:       Cursor horizontal absolute
;; Mnemonic:   CHA
;; Final char: G
;; Final byte: 04/07
;; Sequence:   CSI Pn G
;; Parameters: Pn = n
;; Default:    Pn = 1
;; Reference:  ANSI 5.5, ECMA 8.3.9
;; Notice:     ECMA name: Cursor character absolute
(defun cursor-horizontal-absolute (&optional (n 1))
  "Set the cursor horizontal position to the n-th column in the current line."
  (csi "G" n))

(setf (fdefinition '.cha) #'cursor-horizontal-absolute)

;; Name:       Cursor position
;; Mnemonic:   CUP
;; Final char: H
;; Final byte: 04/08
;; Sequence:   CSI Pn1 ; Pn2 H
;; Parameters: Pn1 = m line, Pn2 = n column
;; Defaults:   Pn1 = 1; Pn2 = 1
;; Reference:  ANSI 5.16, ECMA 8.3.21
(defun cursor-position (&optional (line 1) (column 1))
  "Move the cursor to m-th line and n-th column of the screen.

The line and column numbering is one-based.

Without arguments, the cursor is placed in the home position (1 1),
the top left corner."
  (csi "H" line column))

(setf (fdefinition '.cup) #'cursor-position)

;; Name:       Vertical position absolute
;; Mnemonic:   VPA
;; Final char: d
;; Final byte: 06/04
;; Sequence:   CSI Pn d
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.96, ECMA 8.3.158
;; Notice:     ECMA name: Line position absolute
(defun vertical-position-absolute (&optional (m 1))
  "Set the cursor vertical position to the m-th line in the current column."
  (csi "d" m))

(setf (fdefinition '.vpa) #'vertical-position-absolute)

;; Name:       Vertical position relative
;; Mnemonic:   VPR
;; Final char: e
;; Final byte: 06/05
;; Sequence:   CSI Pn e
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ANSI 5.97, ECMA 8.3.160
;; Notice:     ECMA name: Line position forward
(defun vertical-position-relative (&optional (m 1))
  "Move the cursor vertical position down by m lines in the current column.

This has the same effect as cursor-down (cud)."
  (csi "e" m))

(setf (fdefinition '.vpr) #'vertical-position-relative)

;; Name:       Vertical position backward
;; Mnemonic:   VPB
;; Final char: k
;; Final byte: 06/11
;; Sequence:   CSI Pn k
;; Parameters: Pn = m
;; Default:    Pn = 1
;; Reference:  ECMA 8.3.159
;; Notice:     ECMA name: Line position backward
(defun vertical-position-backward (&optional (m 1))
  "Move the cursor vertical position up by m lines in the current column.

This has the same effect as cursor-up (cuu)."
  (csi "k" m))

(setf (fdefinition '.vpb) #'vertical-position-backward)

(defun save-cursor-position ()
  "Save cursor position. Move cursor to the saved position using restore-cursor-position."
  (csi "s"))

(setf (fdefinition '.scosc) #'save-cursor-position)

(defun restore-cursor-position ()
  "Move cursor to the position saved using save-cursor-position."
  (csi "u"))

(setf (fdefinition '.scorc) #'restore-cursor-position)

;; Name:       Erase in display
;; Mnemonic:   ED
;; Final char: J
;; Final byte: 04/10
;; Sequence:   CSI Ps J
;; Parameters: Ps = mode
;; Defaults:   Ps = 0
;; Reference:  ANSI 5.29, ECMA 8.3.39
;; Notice:     ECMA name: Erase in page
(defun erase-in-display (&optional (mode 0))
  "Erase some or all characters on the screen depending on the selected mode.

Mode 0 (erase-below, default) erases all characters from the cursor to
the end of the screen.

Mode 1 (erase-above) erases all characters from the beginning of the
screen to the cursor.

Mode 2 (erase) erases all characters on the screen.

Mode 3 (erase-saved-lines, xterm) erases all characters on the screen
including the scrollback buffer."
  (csi "J" mode))

(setf (fdefinition '.ed) #'erase-in-display)

(defun erase-below ()
  "Erases all characters from the cursor to the end of the screen."
  (erase-in-display 0))

(defun erase-above ()
  "Erases all characters from the beginning of the screen to the cursor."
  (erase-in-display 1))

(defun erase ()
  "Erase all characters on the screen."
  (erase-in-display 2))

(defun erase-saved-lines ()
  "Erase all characters on the screen including the scrollback buffer."
  (erase-in-display 3))

;; Name:       Erase in line
;; Mnemonic:   EL
;; Final char: K
;; Final byte: 04/11
;; Sequence:   CSI Ps K
;; Parameters: Ps = mode
;; Defaults:   Ps = 0
;; Reference:  ANSI 5.31, ECMA 8.3.41
(defun erase-in-line (&optional (mode 0))
  "Erase some or all characters on the current line depending on the selected mode.

Mode 0 (erase-right, default) erases all characters from the cursor to
the end of the line.

Mode 1 (erase-left) erases all characters from the beginning of the
line to the cursor.

Mode 2 (erase-line) erases all characters on the line."
  (csi "K" mode))

(setf (fdefinition '.el) #'erase-in-line)

(defun erase-right ()
  "Erases all characters from the cursor to the end of the line."
  (erase-in-line 0))

(defun erase-left ()
  "Erases all characters from the beginning of the line to the cursor."
  (erase-in-line 1))

(defun erase-line ()
  "Erases all characters on the current line."
  (erase-in-line 2))

;; Name:        Select Graphic Rendition
;; Mnemonic:    SGR
;; Final char:  m
;, Final byte:  06/13
;; Sequence:    CSI Pm m
;; Parameters:  See documentation string.
;; Defaults:    Pm = 0
;; Reference:   ANSI 5.77, ECMA 8.3.117
(defun select-graphic-rendition (&rest params)
  "Set character attributes and foreground and background colors.

 0  turn off all previous attributes, set normal, default rendition

 1  bold, increased intensity
 2  faint, dim, decreased intensity
 3  italic, standout
 4  single underline
 5  slow blinking
 6  rapid blinking
 7  negative, reverse image
 8  invisible, hidden, concealed
 9  crossed-out
21  double underline

22  turn off bold and faint/dim, set normal intensity
23  turn off italic, standout
24  turn off single, double underline
25  turn off blinking
27  turn off negative, reverse image
28  turn off hidden, invisible
29  turn off crossed-out

Foreground colors:

30  black
31  red
32  green
33  yellow
34  blue
35  magenta
36  cyan
37  white
39  default foreground color

38 5 n      set the color n from a default 256-color palette
38 2 r g b  set the color by directly giving its RGB components

Background colors:

40  black
41  red
42  green
43  yellow
44  blue
45  magenta
46  cyan
47  white
49  default background color

48 5 n      set the color n from a default 256-color palette
48 2 r g b  set the color by directly giving its RGB components"
  (apply #'csi "m" params))

(setf (fdefinition '.sgr) #'select-graphic-rendition)

;; Name:        Device Status Report
;; Mnemonic:    DSR
;; Final char:  n
;, Final byte:  06/14
;; Sequence:    CSI Ps n
;; Parameters:  Ps = status command to send to the terminal
;; Defaults:    n = 6
;; Reference:   ECMA 8.3.35
(defun device-status-report (&optional (n 6))
  "The terminal responds by sending a Cursor Position Report (CPR) to the standard input
as if we read it through read-line from the user."
  (csi "n" n))

(setf (fdefinition '.dsr) #'device-status-report)

;; Name:        Cursor Position Report
;; Mnemonic:    CPR
;; Final char:  R
;, Final byte:  05/02
;; Sequence:    CSI Pm ; Pn R
;; Parameters:  Pm = line, Pn = column
;; Defaults:    Pm = 1, Pn = 1
;; Reference:   ECMA 8.3.14
;; Description: Response of the terminal to a Device Status Report (DSR)
;;              sent to be read from the standard input.

;;; DEC private mode

;; Set (enable, turn on)

(defun dec-private-mode-set (mode)
  "Set (turn on, enable) a DEC private mode.

Implemented modes:

  25 show or hide the cursor
1047 alternate or normal screen buffer"
  (csi "h" "?" mode))

(setf (fdefinition '.decset) #'dec-private-mode-set)

(defun show-cursor ()
  (dec-private-mode-set 25))

(defun use-alternate-screen-buffer ()
  (dec-private-mode-set 1047))

;; Reset (disable, turn off)

(defun dec-private-mode-reset (mode)
  "Reset (turn off, disable) a DEC private mode."
  (csi "l" "?" mode))

(setf (fdefinition '.decrst) #'dec-private-mode-reset)

(defun hide-cursor ()
  (dec-private-mode-reset 25))

(defun use-normal-screen-buffer ()
  (dec-private-mode-reset 1047))

;;; Common
(defun home ()
  "Move the cursor to the home position, the top left corner."
  (cursor-position))

(defun clear ()
  "Erase the whole screen, then move the cursor to the home position."
  (erase)
  (home))

;;; STTY
#|

From /usr/include/x86_64-linux-gnu/bits/termios.h

typedef unsigned char   cc_t;
typedef unsigned int    speed_t;
typedef unsigned int    tcflag_t;

#define NCCS 32

struct termios
  {
    tcflag_t c_iflag;           /* input mode flags */
    tcflag_t c_oflag;           /* output mode flags */
    tcflag_t c_cflag;           /* control mode flags */
    tcflag_t c_lflag;           /* local mode flags */
    cc_t c_line;                /* line discipline */
    cc_t c_cc[NCCS];            /* control characters */
    speed_t c_ispeed;           /* input speed */
    speed_t c_ospeed;           /* output speed */
  };

|#

(defun mode-type (mode)
  "Return the keyword designating the type of the terminal mode:

:input, :output, :control, :local, :character, :combination."
  (let ((iflags
          '(:IGNBRK :BRKINT :IGNPAR :PARMRK :INPCK :ISTRIP :INLCR :IGNCR
            :ICRNL :IUCLC :IXON :IXANY :IXOFF :IMAXBEL :IUTF8))
        (oflags
          '(:OPOST :OLCUC :ONLCR :OCRNL :ONOCR :ONLRET :OFILL :OFDEL :NLDLY
            :NL0 :NL1 :CRDLY :CR0 :CR1 :CR2 :CR3 :TABDLY :TAB0 :TAB1 :TAB2
            :TAB3 :BSDLY :BS0 :BS1 :FFDLY :FF0 :FF1 :VTDLY :VT0 :VT1 :XTABS))
        (cflags
          '(:CBAUD :B0 :B50 :B75 :B110 :B134 :B150 :B200 :B300 :B600 :B1200
            :B1800 :B2400 :B4800 :B9600 :B19200 :B38400 :CSIZE :CS5 :CS6
            :CS7 :CS8 :CSTOPB :CREAD :PARENB :PARODD :HUPCL :CLOCAL :CBAUDEX
            :B57600 :B115200 :B230400 :B460800 :B500000 :B576000 :B921600
            :B1000000 :B1152000 :B1500000 :B2000000 :B2500000 :B3000000
            :B3500000 :B4000000 :CIBAUD :CMSPAR :CRTSCTS))
        (lflags
          '(:ISIG :ICANON :XCASE :ECHO :ECHOE :ECHOK :ECHONL :NOFLSH :TOSTOP
            :ECHOCTL :ECHOPRT :ECHOKE :FLUSHO :PENDIN :IEXTEN :EXTPROC))
        (cc
          '(:VINTR :VQUIT :VERASE :VKILL :VEOF :VTIME :VMIN :VSWTC :VSTART
            :VSTOP :VSUSP :VEOL :VREPRINT :VDISCARD :VWERASE :VLNEXT :VEOL2))
        (combination
          '(:COOKED :RAW)))
    (cond ((member mode iflags) :iflag)
          ((member mode oflags) :oflag)
          ((member mode cflags) :cflag)
          ((member mode lflags) :lflag)
          ((member mode cc) :cc)
          ((member mode combination) :combination)
          (t nil))))

(defun mode-accessor (mode)
  "Return the appropriate accessor depending on the mode type."
  (case (mode-type mode)
    (:iflag 'sb-posix:termios-iflag)
    (:oflag 'sb-posix:termios-oflag)
    (:cflag 'sb-posix:termios-cflag)
    (:lflag 'sb-posix:termios-lflag)
    (:cc    'sb-posix:termios-cc)
    (t nil)))

(defun stream-fd (stream)
  "Return the posix file descriptor associated with the lisp stream."
  (let ((stream (typecase stream
                  ;; *standard-input*, *standard-output*, *terminal-io*, etc.
                  (synonym-stream (symbol-value (synonym-stream-symbol stream)))
                  ;; sb-sys:*stdin*, *stdout*, *tty*, etc.
                  (sb-sys:fd-stream stream))))
    ;; requires a fd-stream, not a synonym-stream
    (sb-posix:file-descriptor stream)))

;; ncurses:
;; cooked: ixon brkint parmrk
;; raw =   -cooked -icanon -isig -iexten
;; noraw =  cooked  icanon  isig  iexten 

;; stty:
;; raw    = -ignbrk -brkint -ignpar -parmrk  -inpck  -istrip  -inlcr -igncr  -icrnl  -ixon  -ixoff -icanon -opost -isig -iuclc -ixany -imaxbel -xcase min 1 time 0
;; cooked =          brkint  ignpar                   istrip                  icrnl   ixon          icanon  opost  isig                               eof ^D eol 0

(defparameter *combinations*
  '(((:raw t)
     :ignbrk nil :brkint nil :ignpar nil :parmrk nil :inpck nil :istrip nil
     :inlcr nil :igncr nil :icrnl nil :ixon nil :ixoff nil :icanon nil
     :opost nil :isig nil
     ;; not available in sb-posix:
     ;;:iuclc nil :ixany nil :imaxbel nil :xcase nil
     :iexten nil :csize nil :parenb nil :vmin 1 :vtime 0)
    ((:raw nil)
     :brkint t :ignpar t :istrip t :icrnl t :ixon t :icanon t :opost t
     :isig t :veol 0)
    ((:cooked t)
     :raw nil)
    ((:cooked nil)
     :raw t)))

(defun mode-combination (mode value)
  "If mode is a combination, return its contents as a plist."
  (cdr (assoc (list mode value) *combinations* :test #'equal)))

(defun set-termios-flag (termios mode value)
  "Take a termios struct, a flag and a value, update the termios struct in place."
  (let* (;; get the appropriate accessor for the flag
         (read-flag (fdefinition (mode-accessor mode)))
         (write-flag (fdefinition (list 'setf (mode-accessor mode))))         
         ;; get the current bitmask
         (old-flag (funcall read-flag termios))
         ;; get the new mode bitmask from the constants in sb-posix
         ;; TODO 200609: what to do with constants not available in sb-posix?
         (new-flag (symbol-value (find-symbol (symbol-name mode) 'sb-posix))))
    ;; write the new values to the termios struct
    ;; (funcall #'(setf acc) val obj) = (setf (acc obj) val)
    (funcall write-flag
             ;; the value for a flag can be t or nil
             (if value
                 ;; if t, add new flag to old flag
                 (logior old-flag new-flag)
                 ;; if nil, remove new flag from old
                 (logand old-flag (lognot new-flag)))
             termios)))

(defun set-termios-param (termios mode value)
  "Take a termios struct, a cc key and a value, update the termios struct in place."
  ;; the mode flags are 32bit unsigned integers
  ;; get the cc array
  (let ((cc-array (sb-posix:termios-cc termios))
        ;; the param name translates to an array index
        (cc-param (symbol-value (find-symbol (symbol-name mode) 'sb-posix))))
    (setf (aref cc-array cc-param) value)))

(defun update-termios (termios modes)
  "Update the settings in the termios struct in place with the values in modes plist."
  (loop for (mode value) on modes by #'cddr do
    (case (mode-type mode)
      (:combination
       (update-termios termios (mode-combination mode value)))
      ((:iflag :oflag :cflag :lflag)
       (set-termios-flag termios mode value))
      (:cc
       (set-termios-param termios mode value)))))

;; Examples: t06, t07
(defun set-tty-mode (stream &rest modes)
  "Enable or disable one or more tty modes."
  (let* ((stream (if (eq stream t) *standard-input* stream))
         (fd (stream-fd stream))
         ;; get the current attributes in a termios object
         (termios (sb-posix:tcgetattr fd)))
    ;; Update the termios struct in place.
    (update-termios termios modes)
    ;; write the new termios struct to the fd of the tty now.
    (sb-posix:tcsetattr fd sb-posix:tcsanow termios)))

#|
(defun t01 ()
  (erase)
  (cursor-position 0 0)
  (princ "0")
  (cursor-position 2 2)
  (princ "1")
  (cursor-position 5 15)
  (princ "test")
  (cursor-position 10 15)
  (force-output)
  (let ((a (read-line)))
    (cursor-position 12 15)
    (princ a)
    (force-output)))

(defun t02 ()
  (print "normal")
  (sgr 1)
  (print "bold")
  (sgr 4)
  (print "bold underline")
  (sgr 7)
  (print "bold underline reverse")
  (sgr 22)
  (print "underline reverse")
  (sgr 24)
  (print "reverse")
  (sgr 27)
  (print "normal")
  (sgr 1 4 7)
  (print "bold underline reverse")
  (sgr 0)
  (print "normal")
  (force-output))

(defun t03 ()
  "Display the 256 color palette."
  (clear)
  (loop for i from 0 to 255 do
    (sgr 48 5 i)
    (princ #\space))
  (terpri)
  (sgr 0)
  (loop for i from 0 to 255 do
    (sgr 38 5 i)
    (princ "X"))
  (sgr 0)
  (force-output)
  (sleep 3)
  (ris)
  (force-output))

(defun t04 ()
  "Hide and show the cursor."
  (princ "Cursor visible:")
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor invisible:")
  (hide-cursor)
  (force-output)
  (sleep 2)
  (terpri)
  (princ "Cursor visible:")
  (show-cursor)
  (force-output)
  (sleep 2))

(defun t05 ()
  "Switch to and back from the alternate screen buffer."
  (princ "Normal screen buffer. ")
  (force-output)
  (sleep 2)

  (save-cursor-position)
  (use-alternate-screen-buffer)
  (clear)
  (princ "Alternate screen buffer.")
  (force-output)
  (sleep 2)

  (use-normal-screen-buffer)
  (restore-cursor-position)
  (princ "Back to Normal screen buffer.")
  (force-output)
  (sleep 1))

(defun t06 ()
  "Set individual termios flags to enable raw and disable echo mode.

Enabling raw mode allows read-char to return immediately after a key is pressed.

In the default cooked mode, the entry has to be confirmed by pressing enter."
  (set-tty-mode t :ignbrk nil
                  :brkint nil
                  :parmrk nil
                  :istrip nil
                  :inlcr  nil
                  :igncr  nil
                  :icrnl  nil
                  :ixon   nil
                  :opost  nil
                  :echo   nil
                  :echonl nil
                  :icanon nil
                  :isig   nil
                  :iexten nil
                  :csize  nil
                  :parenb nil
                  :vmin 1
                  :vtime 0)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 10 5)
    (princ a)
    (force-output))

  (set-tty-mode t :echo t
                  :brkint t
                  :ignpar t
                  :istrip t
                  :icrnl t
                  :ixon t
                  :opost t
                  :isig t
                  :icanon t
                  :veol 0))

(defun t07 ()
  "Use combination modes that consist of several individual flags.

Cooked and raw are opposite modes. Enabling cooked disbles raw and vice versa."
  (set-tty-mode t :cooked nil)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 3 1)
    (princ a)
    (force-output))
  (set-tty-mode t :raw nil))

(defun t08 ()
  "Why doesnt calling the stty utility work?"
  (uiop:run-program "stty raw -echo" :ignore-error-status t)
  (erase)
  (cursor-position 1 1)
  (force-output)
  (let ((a (read-char)))
    (cursor-position 2 1)
    (princ a)
    (force-output))
  (uiop:run-program "stty -raw echo" :ignore-error-status t))

(defun t09 ()
  "Query terminal size with ANSI escape sequences."
  ;; Put the terminal into raw mode so we can read the "user input"
  ;; of the reply char by char
  ;; Turn off the echo or the sequence will be displayed
  (set-tty-mode t :cooked nil :echo nil)
  (save-cursor-position)
  ;; Go to the bottom right corner of the terminal by attempting
  ;; to go to some high value of row and column
  (cursor-position 999 999)
  (let (chars)
    ;; The terminal returns an escape sequence to the standard input
    (device-status-report)
    (force-output)
    ;; The reply isnt immediately available, the terminal does need
    ;; some time to answer
    (sleep 0.1)
    ;; The reply has to be read as if the user typed an escape sequence
    (loop for i = (read-char-no-hang *standard-input* nil)
          until (null i)
          do (push i chars))
    ;; Put the terminal back into its initial cooked state
    (set-tty-mode t :raw nil :echo t)
    (restore-cursor-position)
    ;; Return the read sequence as a list of characters.
    (nreverse chars)))
|#
