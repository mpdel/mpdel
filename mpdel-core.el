;; -*- lexical-binding: t; -*-
;;; mpdel-core.el --- A simple MPD client for Emacs
;;
;; Copyright (C) 2013 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/mpdel
;; GIT: https://github.com/DamienCassou/mpdel
;; Version: 0.1
;; Created: 2013-05-23
;; Keywords: emacs package elisp mpd musicpd music
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; commentary:
;;
;;; code:
;;


(require 'time-stamp)
(require 'tq)

(defvar mpdel-connection nil
  "Keep track of the current connection to the MPD server.
The logs of this connection are accessible in the *mpd* buffer.")

(defvar mpdel-connection-queue nil
  "Interface with the `tq' library")

;; Copy-paste from emms-player-mpd.el
;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
;; Author: Michael Olson <mwolson@gnu.org>
;; Fix: add .+ after " MPD "
;; Fix: don't match end of buffer at the end of regexp
(defvar mpdel-status-regexp
  "^\\(\\(OK\\( MPD .+\\)?\\)\\|\\(ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\\)\n+"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defvar mpdel-msghandlers nil
  "Keep track of the current commands sent to the server.
Each element in the queue is of the form (COMMAND . HANDLER).

COMMAND is the string representing the query sent to the server.
Even though this information is not necessary, it is useful to
better understand the log.

HANDLER is a function that will be executed when the answers to
COMMAND comes back. The function must accept one
parameter (usually named MESSAGE) that will contain the answer.

An invariant of this MPD client is that there is always an IDLE
command sent to the server (and its corresponding handler in this
variable). This means our client is always registered to
notifications in the server. When we want to send a command to
the server (for example to change the current song), we always
have to cancel the IDLE first (with a NO IDLE command), to send the
command we want, and to finally send the IDLE command again.
Cancelling the current IDLE command is done in
`mpdel-send-command'. Sending IDLE again is done in the
handler for IDLE that will be triggered when the empty answer for
the cancelled IDLE arrives.

Because MPD answers in the order the commands are sent, we know
that the first handler of the queue is the one to execute when we
receive a message from the server.")

(defvar mpdel-changehandlers nil
  "Functions to call when a server notification is received.
Each function will be passed a list of symbols as parameter. Each
symbol in this list represents something that changed in the
server.

All possible symbols are listed on
http://www.musicpd.org/doc/protocol/ch03.html as the description
of the IDLE command.")

(defun mpdel-connect ()
  "Creates a new connection with the MPD server."
  ;; the *mpd* buffer will contain all the communication logs
  (with-current-buffer (get-buffer-create "*mpd*")
    (erase-buffer))
  (setq mpdel-connection
        (open-network-stream
         "mpd" "*mpd*"
         ;; should use defcustom for that at some point :-)
         "localhost" 6600
         :type 'plain))
  (setq mpdel-connection-queue (tq-create mpdel-connection))
  (set-process-coding-system mpdel-connection
                             'utf-8-unix 'utf-8-unix)
  ;; As an invariant of the MPD client, there is always an IDLE
  ;; command sent to the server. This acts like a registration to the
  ;; server's n otifications. See `mpdel-msghandlers' for more
  ;; information.
  (mpdel-raw-send-command "idle\n")
  ;; We have to specify the first 2 handlers
  (setq mpdel-msghandlers
        (list
         ;; the server always starts by saying hello, so we add a
         ;; handler for that.
         (cons "welcome" #'mpdel-msghandler-welcome)
         ;; then, we need the handler to answer to the IDLE command we
         ;; just sent.
         (cons "idle" #'mpdel-msghandler-status))))

(defun mpdel-ensure-connection ()
  "Make sure there is an active connection to the server."
  (when (or (not (processp mpdel-connection))
            (not (process-live-p mpdel-connection)))
    (mpdel-connect)))

(defun mpdel-disconnect ()
  "Make sure there is no active connection to the server."
  (when (not (null mpdel-connection))
    (tq-close mpdel-connection-queue)
    (delete-process mpdel-connection))
  (setq mpdel-connection-queue nil)
  (setq mpdel-connection nil))

(defun mpdel-raw-send-command (command)
  "Send COMMAND to the server and log that."
  (mpdel-log command "->")
  (tq-enqueue
   mpdel-connection-queue
   command
   mpdel-status-regexp
   nil
   #'mpdel-message-filter))


(defun mpdel-message-filter (server message)
  "Take care of the MESSAGE sent by the SERVER.
SERVER is ignored. MESSAGE contains a string representing the
answer from the server."
  ;; Because errors in handlers are not raised by Emacs, we log them.
  (condition-case error
      (progn
        ;; because answers arrive in the same order we sent the
        ;; commands, we are sure that the first handler is the one to
        ;; use.
        (let* ((data (pop mpdel-msghandlers))
               ;; COMMAND is the original query sent to the server
               (command (car data))
               ;; HANDLER is the function to call with MESSAGE are argument
               (handler (cdr data)))
          (mpdel-log (format "%s (as answer to %s)" message command)
                     "<-")
          ;; if answer is a ACK, then there was a problem. We log it as such.
          (if (string= (substring message 0 3) "ACK")
              (mpdel-log "ACK message" "ko")
            ;; else, we just execute the handler.
            (funcall handler message))))
    ;; in case of an error in one handler, we log it.
    (error (mpdel-log error "ko"))))

(defun mpdel-send-command (command &optional handler)
  "Send COMMAND to server and register HANDLER for the answer."
  (mpdel-ensure-connection)
  ;; if current command is IDLE, we have to cancel it. See
  ;; `mpdel-msghandlers' for more information.
  (when (eql (cdr (car (last mpdel-msghandlers)))
             #'mpdel-msghandler-status)
    (mpdel-log "Sending noidle to handle next command" " i")
    (mpdel-raw-send-command "noidle\n"))
  (setq mpdel-msghandlers
        (append
         mpdel-msghandlers
         (list
          (cons
           command
           (or handler #'mpdel-msghandler-ignore)))))
  (mpdel-raw-send-command (format "%s\n" command)))

(defun mpdel-send-command-ignore-result (string &optional objects)
  (mpdel-send-command
   (format string objects)
   (lambda (message))))

(defun mpdel-msghandler-ignore (message)
  (mpdel-log message "ig"))

(defun mpdel-msghandler-status (message)
  (setq mpdel-msghandlers
        (append mpdel-msghandlers
                (list (cons "idle" #'mpdel-msghandler-status))))
  (mpdel-raw-send-command "idle\n")
  (let ((changes (mapcar
                  #'mpdel-changed-field
                  (mpdel-extract-data message))))
    (mpdel-dispatch-status-update changes)))

(defun mpdel-dispatch-status-update (changes)
  (mapc (lambda (handler) (funcall handler changes))
        mpdel-changehandlers))

(defun mpdel-changehandler-message (changes)
  (message "[%s] Handler: %s" (time-stamp-string) changes))

(defun mpdel-add-changehandler (fn)
  (add-to-list 'mpdel-changehandlers fn))

(defun mpdel-msghandler-welcome (message)
  (mpdel-log message "hi"))

(defvar mpdel-general-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "space") #'mpdel-toggle-play-pause)
    map))

(defun mpdel-toggle-play-pause ()
  )

(defun mpdel-log (string direction)
  (when (buffer-live-p (process-buffer mpdel-connection))
    (with-current-buffer (process-buffer mpdel-connection)
      (let ((moving (= (point) (process-mark mpdel-connection))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark mpdel-connection))
          (insert "-------------------------\n")
          (insert (format "%s [%s] \"%s\"\n" direction (time-stamp-string) string))
          (set-marker (process-mark mpdel-connection) (point)))
        (if moving (goto-char (process-mark mpdel-connection)))))))

(defun mpdel-add-entities-to-playlist (terms entities)
  (mpdel-send-command
   (format "list file %s" (mpdel-conc-to-query terms entities))
   #'mpdel-try))

(defun mpdel-try (message)
  (mpdel-add-files-to-playlist
   (mapcar #'mpdel-file-field (mpdel-extract-data message))))

(defun mpdel-add-files-to-playlist (files)
  (dolist (file files)
    (mpdel-send-command-ignore-result
     "add \"%s\"" file)))

(defun mpdel-list-query (type terms entities)
  (format
   "list %s %s"
   type (mpdel-conc-to-query terms entities)))

(defun mpdel-conc-to-query (terms entities)
  (with-output-to-string
    (loop
     for term in terms
     for entity in entities
     for index from 1
     do (progn (when (> index 1)
                 (princ " "))
               (princ term)
               (princ " \"")
               (princ entity)
               (princ "\"")))))

(defun mpdel-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun mpdel-string<-noncase (str1 str2)
  (let ((comp (compare-strings str1 nil nil str2 nil nil t)))
    (or (eq comp t) (< comp 0))))

(defconst mpdel-msgfield-regexp "^\\([^:]*\\): \\(.*\\)$")
(defun mpdel-extract-data (message)
  (with-temp-buffer
    (insert message)
    (goto-char (point-min))
    (when (re-search-forward mpdel-msgfield-regexp nil t)
      (let ((first-field-type (match-string 1)))
        (goto-char (point-min))
        (loop
         until (looking-at "OK")
         collect (loop
                  while (re-search-forward mpdel-msgfield-regexp nil t)
                  for field-type = (match-string 1)
                  for field-value = (match-string 2)
                  collect (cons (intern field-type) field-value)
                  do (forward-line 1)
                  until (looking-at first-field-type)))))))

(defun mpdel-extract-data1 (message)
  (car (mpdel-extract-data message)))

(defun mpdel-format-time (seconds)
  (let ((seconds (string-to-number seconds)))
    (format
     "%02d:%02d"
     (floor (/ seconds 60))
     (round (mod seconds 60)))))

(defun mpdel-data-field (data field)
  (cdr (assoc field data)))

(defun mpdel-artist-field (data)
  (mpdel-data-field data 'Artist))

(defun mpdel-file-field (data)
  (mpdel-data-field data 'file))

(defun mpdel-id-field (data)
  (mpdel-data-field data 'Id))

(defun mpdel-album-field (data)
  (mpdel-data-field data 'Album))

(defun mpdel-title-field (data)
  (mpdel-data-field data 'Title))

(defun mpdel-changed-field (data)
  (intern (mpdel-data-field data 'changed)))

(defun mpdel-state-field (data)
  (intern (mpdel-data-field data 'state)))

(defun mpdel-elapsed-field (data)
  (mpdel-data-field data 'elapsed))

(defun mpdel-time-field (data)
  (mpdel-data-field data 'Time))

(provide 'mpdel-core)
