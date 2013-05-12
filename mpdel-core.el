;; -*- lexical-binding: t; -*-
;;; mpdel-core.el --- TODO
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
;;
;;; code:
;;
;;; http://www.musicpd.org/doc/protocol/ch01s03.html


(require 'time-stamp)

;; Some examples of commands:
;; (mpdel-send-command "rescan")
;; (mpdel-send-command "status")
;; (mpdel-send-command "idle")
;; (mpdel-send-command "noidle")
;; (mpdel-send-command "currentsong")
;; (mpdel-send-command "setvol 50")
;; (mpdel-send-command "play")
;; (mpdel-send-command "play 259")
;; (mpdel-send-command "pause 1")
;; (mpdel-send-command "pause 0")
;; (mpdel-send-command "next")
;; (mpdel-send-command "playlistinfo")
;; (mpdel-send-command "count")
;; (mpdel-send-command "list file artist \"Eloy\"")
;; (mpdel-send-command "list track artist \"Eloy\" album \"Dawn\"")
;; (mpdel-send-command "searchadd track artist \"Eloy\" album \"Dawn\"")
;; (mpdel-send-command "list album")
;; (mpdel-send-command "list artist")
;; (mpdel-send-command "tagtypes")
;; (mpdel-send-command "count album \"\"")
;; (mpdel-send-command "search album \"\"")

(defvar mpdel-connection nil)
(defvar mpdel-connection-queue nil)

;; Copy-paste from emms-player-mpd.el
;; Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
;; Author: Michael Olson <mwolson@gnu.org>
;; Fix: add .+ after " MPD "
;; Fix: don't match end of buffer at the end of regexp
(defvar mpdel-status-regexp
  "^\\(\\(OK\\( MPD .+\\)?\\)\\|\\(ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\\)\n+"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun mpdel-connect ()
  (setq mpdel-msghandlers
        (list #'mpdel-msghandler-welcome
              #'mpdel-msghandler-status))
  (with-current-buffer (get-buffer-create "*mpd*")
    (erase-buffer))
  (setq mpdel-connection
        (open-network-stream
         "mpd" "*mpd*"
         "localhost" 6600
         :type 'plain))
  ;; Because Emacs is single-threaded, Emacs will only take care of
  ;; server's answers when he is done with everything else (this
  ;; includes executing all the call stack).
  ;; (set-process-filter mpdel-connection 'mpdel-message-filter)
  (setq mpdel-connection-queue (tq-create mpdel-connection))
  (set-process-coding-system mpdel-connection
                             'utf-8-unix 'utf-8-unix)
  (mpdel-raw-send-command "idle\n"))

(defun mpdel-raw-send-command (command)
  (mpdel-log command "->")
  (tq-enqueue
   mpdel-connection-queue
   command
   mpdel-status-regexp
   nil
   #'mpdel-message-filter))

(defun mpdel-ensure-connection ()
  (when (or (not (processp mpdel-connection))
            (not (process-live-p mpdel-connection)))
    (mpdel-connect)))

(defun mpdel-disconnect ()
  (when (not (null mpdel-connection))
    (tq-close mpdel-connection-queue)
    (delete-process mpdel-connection))
  (setq mpdel-connection-queue nil)
  (setq mpdel-connection nil))

(defun mpdel-message-filter (proc message)
  "Automatically called when the server sends a message."
  (mpdel-log message "<-")
  (if (string= (substring message 0 3) "ACK")
      (mpdel-log "ACK message" "ko")
    (mpdel-msghandlers-call message)))

(defvar mpdel-msghandlers nil)

(defun mpdel-msghandlers-call (message)
  "Call the first handler in `mpdel-msghandlers'."
  (if (null mpdel-msghandlers)
      (mpdel-log
       (format"mpdel: no handler for new message: %s" message)
       "ko")
    (funcall (pop mpdel-msghandlers) message)))

(defun mpdel-send-command (command &optional handler)
  (mpdel-ensure-connection)
  (setq mpdel-msghandlers
        (append
         mpdel-msghandlers
         (list
          (or handler #'mpdel-msghandler-ignore))))
  (mpdel-raw-send-command "noidle\n")
  (mpdel-raw-send-command (format "%s\n" command)))

(defun mpdel-send-command-ignore-result (string objects)
  (mpdel-send-command
   (format string objects)
   (lambda (message))))

(defun mpdel-msghandler-ignore (message)
  (mpdel-log message "ig"))

(defun mpdel-msghandler-status (message)
  (setq mpdel-msghandlers
        (append mpdel-msghandlers
                (list #'mpdel-msghandler-status)))
  (mpdel-raw-send-command "idle\n")
  (mpdel-log message "st"))

(defun mpdel-msghandler-welcome (message)
  (mpdel-log message "hi"))

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
   (mpdel-extractor message "file: ")))

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

(defun mpdel-data-field (data field)
  (cdr (assoc field data)))

(defun mpdel-artist-field (data)
  (mpdel-data-field data 'Artist))

(defun mpdel-album-field (data)
  (mpdel-data-field data 'Album))

(defun mpdel-title-field (data)
  (mpdel-data-field data 'Title))

(provide 'mpdel-core)
