;; -*- lexical-binding: t; -*-
;;; mpdel-header.el --- TODO
;;
;; Copyright (C) 2013 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Url: https://github.com/DamienCassou/mpdel
;; GIT: https://github.com/DamienCassou/mpdel
;; Version: 0.1
;; Created: 2013-05-25
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

(defvar mpdel-header-current-song nil)
(defvar mpdel-header-status nil)
(defvar mpdel-header-buffers nil)

(defun mpdel-header-changehandler-update (changes)
  (when (member 'player changes)
    (mpdel-header-fetch-data)))

(defun mpdel-header-fetch-data ()
  (mpdel-send-command
   "currentsong"
   (lambda (message)
     (setq mpdel-header-current-song
           (mpdel-extract-data1 message))))
  (mpdel-send-command
   "status"
   (lambda (message)
     (setq mpdel-header-status
           (mpdel-extract-data1 message))
     (mpdel-header-update-buffers))))

(mpdel-add-changehandler #'mpdel-header-changehandler-update)

(defun mpdel-header-content ()
  (if (null mpdel-header-current-song)
      (progn (mpdel-header-fetch-data)
             nil)
    (case (mpdel-state-field mpdel-header-status)
      (stop "Stopped")
      (pause "Paused")
      (play
       (format "Currently playing: %s - %s"
               (mpdel-artist-field mpdel-header-current-song)
               (mpdel-title-field mpdel-header-current-song))))))

(defun mpdel-header-update-buffers ()
  (dolist (buffer mpdel-header-buffers)
    (mpdel-header-set-headerline buffer)))

(defun mpdel-header-set-headerline (buffer)
  (with-current-buffer buffer
    (setq header-line-format (mpdel-header-content))))

(defun mpdel-header-add-buffer (buffer)
  (add-to-list 'mpdel-header-buffers buffer)
  (mpdel-header-set-headerline buffer))

(provide 'mpdel-header)
