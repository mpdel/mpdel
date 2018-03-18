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

(cl-defgeneric mpdel-nav--entity-to-list-entry (entity)
  "Convert ENTITY to a format suitable for the tabulated list."
  (list entity
        (vector (libmpdel-entity-name entity))))

(defun mpdel-nav--default-tabulated-list-format ()
  "Return `tabulated-list-format' value for any non-specific entity."
  (vector (list "Name" 0 t)))

(cl-defmethod mpdel-nav--entity-to-list-entry ((entity libmpdel-album))
  (list entity
        (vector (libmpdel-entity-name entity)
                (libmpdel-artist-name entity))))

(defun mpdel-nav--album-tabulated-list-format ()
  "Return `tabulated-list-format' value for albums."
  (vector (list "Name" 40 t)
          (list "Artist" 0 t)))

(cl-defmethod mpdel-nav--entity-to-list-entry ((entity libmpdel-song))
  (list entity
        (vector (libmpdel-entity-name entity)
                (libmpdel-album-name entity)
                (libmpdel-artist-name entity))))

(defun mpdel-nav--song-tabulated-list-format ()
  "Return `tabulated-list-format' value for songs."
  (vector (list "Name" 30 t)
          (list "Album" 30 t)
          (list "Artist" 0 t)))

(cl-defgeneric mpdel-nav--tabulated-list-format (entity)
  "Return `tabulated-list-format' value for children of ENTITY.")

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity (eql artists)))
  (mpdel-nav--default-tabulated-list-format))

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity (eql stored-playlists)))
  (mpdel-nav--default-tabulated-list-format))

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity libmpdel-stored-playlist))
  (mpdel-nav--song-tabulated-list-format))

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity libmpdel-artist))
  (mpdel-nav--album-tabulated-list-format))

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity libmpdel-album))
  (mpdel-nav--song-tabulated-list-format))

(cl-defmethod mpdel-nav--tabulated-list-format ((_entity libmpdel-search-criteria))
  (mpdel-nav--song-tabulated-list-format))

(defun mpdel-nav--open (entity)
  "Open a navigator buffer displaying children of ENTITY."
  (with-current-buffer (mpdel-nav--buffer)
    (mpdel-nav-mode)
    (setq mpdel-nav--entity entity)
    (setq tabulated-list-format (mpdel-nav--tabulated-list-format entity))
    (tabulated-list-init-header)
    (mpdel-nav-refresh)
    (switch-to-buffer (current-buffer))))


;;; Public functions

(defun mpdel-nav-refresh ()
  "Refresh buffer."
  (interactive)
  (libmpdel-list
   mpdel-nav--entity
   (lambda (entities)
     (with-current-buffer (mpdel-nav--buffer)
       (setq tabulated-list-entries (mapcar #'mpdel-nav--entity-to-list-entry entities))
       (tabulated-list-print)))))

(defun mpdel-nav-open-entity-parent-at-point (&optional entity)
  "Refresh navigator to display parent of ENTITY among its siblings.
Use entity at point if ENTITY is nil."
  (interactive)
  (let* ((entity (or entity (mpdel-core-entity-at-point)))
         (parent (libmpdel-entity-parent entity))
         (ancestor (and parent (libmpdel-entity-parent parent))))
    (if (and ancestor (libmpdel-equal parent mpdel-nav--entity))
        (mpdel-nav--open ancestor)
      (when parent
        (mpdel-nav--open parent)))))

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

(defvar mpdel-nav-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from both `mpdel-core-map' and
    ;; `tabulated-list-mode-map':
    (set-keymap-parent
     map
     (make-composed-keymap mpdel-core-map tabulated-list-mode-map))
    (define-key map (kbd "g") #'mpdel-nav-refresh)
    (define-key map (kbd "^") #'mpdel-nav-open-entity-parent-at-point)
    map)
  "Keybindings for `mpdel-nav-mode'.")

(define-derived-mode mpdel-nav-mode tabulated-list-mode "MPD Navigator"
  "Abstract major mode to list part of the MPD database.")

(provide 'mpdel-nav)
;;; mpdel-nav.el ends here
