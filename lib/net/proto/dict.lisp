;;; dict.lisp --- Dictionary Server Support

;; Query dictionary servers

;;; Commentary:

;; RFC2229

;; 1024-char buffer limit

;; default-port = 2628

;; utf-8 encoded (usually)

;; commands are NOT case-sensitive

#|
 Commands consist of a command word followed by zero or more
   parameters.  Commands with parameters must separate the parameters
   from each other and from the command by one or more space or tab
   characters.  Command lines must be complete with all required
   parameters, and may not contain more than one command.

   Each command line must be terminated by a CRLF.

   The grammar for commands is:

             command     = cmd-word *<WS cmd-param>
             cmd-word    = atom
             cmd-param   = database / strategy / word
             database    = atom
             strategy    = atom
|#

;; responses are either status or textual.

;;; Code:
(in-package :net/proto/dict)

(defclass dictionary-connection (connection) ())

(defclass dictionary-request (obj:request) ())

(defclass dictionary-response (obj:response) ())

(defclass dictionary-client (tcp-client) ())
