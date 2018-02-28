;;; mpdel.el --- Play and control your MPD music  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; MPDel is an Emacs client for Music Player Daemon (MPD), a flexible,
;; powerful, server-side application for playing music.  This file
;; makes features (e.g., playlist, navigation and playback) provided
;; by other files in the project accessible globally through the
;; global minor mode `mpdel-mode'.  This file is also responsible for
;; integrating the features together to provide a seamless experience
;; (see `mpdel-setup').

;;; Code:

(require 'libmpdel)
(require 'mpdel-core)
(require 'mpdel-playlist)
(require 'mpdel-song)

(defgroup mpdel nil
  "Configure mpdel's global minor mode."
  :group 'libmpdel)

(defcustom mpdel-prefix-key (kbd "C-x Z")
  "Prefix key to all global mpdel keybindings."
  :type 'key-sequence)

(defun mpdel-playlist-open-song-at-point ()
  "Open buffer displaying information about song at point."
  (interactive)
  (mpdel-song-open (mpdel-playlist-song-at-point)))

(define-key mpdel-core-map (kbd "l") #'mpdel-playlist-open)
(define-key mpdel-core-map (kbd "L") #'mpdel-playlist-open-stored-playlist)
(define-key mpdel-core-map (kbd "v") #'mpdel-song-open)
(define-key mpdel-playlist-mode-map (kbd "V") #'mpdel-playlist-open-song-at-point)

(defvar mpdel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map mpdel-prefix-key 'mpdel-core-map)
    map)
  "Keymap activating variable `mpdel-core-map'.")

(define-minor-mode mpdel-mode
  "Activate keybindings to play and control your MPD server.

\\{mpdel-mode-map}"
  :global t
  :require 'mpdel
  :lighter " MPDel")

(provide 'mpdel)
;;; mpdel.el ends here

;;; LocalWords:  Mpdel mpdel
