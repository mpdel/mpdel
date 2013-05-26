;; -*- lexical-binding: t; -*-
;;; mpdel-playlist.el --- TODO
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


(require 'mpdel-core)

(defvar mpdel-playlist-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<tab>") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "g") #'mpdel-playlist-refresh)
    (define-key map (kbd "o") #'mpdel-navigator)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "q") #'mpdel-kill-buffer)
    map))

(defvar mpdel-playlist-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'mpdel-playlist-play)
    (define-key map (kbd "k")   #'mpdel-playlist-delete)
    map))

(define-derived-mode mpdel-playlist-mode nil "MPDel"
  "..."
  (setq buffer-read-only t))

(define-button-type 'mpdel-playlist-button
  'keymap mpdel-playlist-button-map)

(defun mpdel-playlist ()
  (interactive)
  (mpdel-send-command
   "playlistinfo"
   (lambda (message)
     (mpdel-playlist-create-buffer message))))

(defun mpdel-playlist-play ()
  (interactive)
  (let* ((button (button-at (point)))
         (id (button-get button 'Id)))
    (mpdel-send-command-ignore-result "playid %s" id)))

(defun mpdel-playlist-refresh ()
  (interactive)
  (mpdel-playlist))

(defun mpdel-playlist-changehandler-refresh (changes)
  (when (member 'playlist changes)
    (message "Playlist updated")
    (mpdel-playlist-refresh)))

(mpdel-add-changehandler #'mpdel-playlist-changehandler-refresh)

(defun mpdel-playlist-create-buffer (message)
  (with-current-buffer (get-buffer-create "*mpdel-playlist*")
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (dolist (title (mpdel-extract-data message))
        (apply
         'insert-text-button
         (append
          (list (mpdel-playlist-title-label title)
                'type 'mpdel-playlist-button)
          (mpdel-flatten-data title)))
        (insert "\n"))
      (mpdel-playlist-mode)
      (when (require 'mpdel-header nil t)
        (mpdel-header-add-buffer (current-buffer))))
    (switch-to-buffer "*mpdel-playlist*")))

(defun mpdel-flatten-data (data)
  (loop
   for (field-type . field-value) in data
   append (list field-type field-value)))

(defun mpdel-playlist-title-label (title)
  (format "%s - %s"
          (mpdel-artist-field title)
          (mpdel-title-field title)))

(provide 'mpdel-playlist)
