;;; mpdel-nav.el --- Navigate your MPD database      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 0.3.0

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

;; User interface to browse the MPD database and add songs to
;; playlists.

;;; Code:

(require 'libmpdel)

(require 'mpdel-core)


;;; Helpers

;; The default entity is a dumb one representing all artists.
(defvar-local mpdel-nav--entity 'artists
  "Stores entity (e.g., album) associated to current buffer.")

(defun mpdel-nav--buffer ()
  "Return buffer for mpdel-nav."
  (get-buffer-create "*MPDEL Navigator*"))

(defun mpdel-nav--entity-to-list-entry (entity)
  "Convert ENTITY to a format suitable for the tabulated list."
  (list entity
        (vector (libmpdel-entity-name entity))))

(defun mpdel-nav--open (entity)
  "Open a navigator buffer displaying children of ENTITY."
  (with-current-buffer (mpdel-nav--buffer)
    (mpdel-nav-mode)
    (setq mpdel-nav--entity entity)
    (mpdel-nav-refresh)
    (switch-to-buffer (current-buffer))))


;;; Public functions

(defun mpdel-nav-entity-at-point (&optional pos)
  "Return entity at POS, current point if nil."
  (tabulated-list-get-id pos))

(defun mpdel-nav-selected-entities ()
  "Return entities within active region or at point."
  (mpdel-core--selected-entities #'mpdel-nav-entity-at-point))

(defun mpdel-nav-refresh ()
  "Refresh buffer."
  (interactive)
  (libmpdel-list
   mpdel-nav--entity
   (lambda (entities)
     (with-current-buffer (mpdel-nav--buffer)
       (setq tabulated-list-entries (mapcar #'mpdel-nav--entity-to-list-entry entities))
       (tabulated-list-print)))))

(cl-defgeneric mpdel-nav-dive (entity)
  "Refresh navigator buffer to display content of ENTITY."
  (setq mpdel-nav--entity entity)
  (mpdel-nav-refresh))

(defun mpdel-nav-add-to-current-playlist ()
  "Add entity at point to current playlist."
  (interactive)
  (libmpdel-current-playlist-add (mpdel-nav-entity-at-point)))

(defun mpdel-nav-add-to-stored-playlist ()
  "Add entity at point to a stored playlist."
  (interactive)
  (libmpdel-stored-playlist-add (mpdel-nav-entity-at-point)))

(defun mpdel-nav-replace-current-playlist ()
  "Replace current playlist with entity at point."
  (interactive)
  (libmpdel-current-playlist-replace (mpdel-nav-entity-at-point)))

(defun mpdel-nav-replace-stored-playlist ()
  "Replace a stored playlist with entity at point."
  (interactive)
  (libmpdel-stored-playlist-replace (mpdel-nav-entity-at-point)))

;;;###autoload
(defun mpdel-nav-open-artists ()
  "Display all artists in the MPD database."
  (interactive)
  (mpdel-nav--open 'artists))

;;;###autoload
(defun mpdel-nav-open-stored-playlists ()
  "Display all stored playlists in the MPD database."
  (interactive)
  (mpdel-nav--open 'stored-playlists))

;;;###autoload
(defun mpdel-nav-search-by-artist (name)
  "Display all songs whose artist's name match NAME.
Interactively, ask for NAME."
  (interactive (list (read-from-minibuffer "Search with artist: ")))
  (mpdel-nav--open (libmpdel-search-criteria-create :type "artist" :what name)))

;;;###autoload
(defun mpdel-nav-search-by-album (name)
  "Display all songs whose album's name match NAME.
Interactively, ask for NAME."
  (interactive (list (read-from-minibuffer "Search with album: ")))
  (mpdel-nav--open (libmpdel-search-criteria-create :type "album" :what name)))

;;;###autoload
(defun mpdel-nav-search-by-title (title)
  "Display all songs matching TITLE.
Interactively, ask for TITLE."
  (interactive (list (read-from-minibuffer "Search with title: ")))
  (mpdel-nav--open (libmpdel-search-criteria-create :type "title" :what title)))

(defmacro mpdel-nav--apply (function)
  "Return a command applying FUNCTION to entity at point."
  `(lambda ()
     (interactive)
     (mapcar ,function (mpdel-nav-selected-entities))))

(defvar mpdel-nav-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from both `mpdel-core-map' and
    ;; `tabulated-list-mode-map':
    (set-keymap-parent
     map
     (make-composed-keymap mpdel-core-map tabulated-list-mode-map))
    (define-key map (kbd "g") #'mpdel-nav-refresh)
    (define-key map (kbd "RET") (mpdel-nav--apply #'mpdel-nav-dive))
    (define-key map (kbd "a") (mpdel-nav--apply #'libmpdel-current-playlist-add))
    (define-key map (kbd "A") (mpdel-nav--apply #'libmpdel-stored-playlist-add))
    (define-key map (kbd "r") (mpdel-nav--apply #'libmpdel-current-playlist-replace))
    (define-key map (kbd "R") (mpdel-nav--apply #'libmpdel-stored-playlist-replace))
    map)
  "Keybindings for `mpdel-nav-mode'.")

(define-derived-mode mpdel-nav-mode tabulated-list-mode "MPD Navigator"
  "Abstract major mode to list part of the MPD database."
  (setq tabulated-list-format (vector (list "Name" 0 t)))
  (tabulated-list-init-header))

(provide 'mpdel-nav)
;;; mpdel-nav.el ends here
