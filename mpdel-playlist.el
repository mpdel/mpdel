;;; mpdel-playlist.el --- Display and manipulate MPD playlists  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 0.4.0

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

;; Let the user view and manipulate MPD's current playlist and stored
;; playlists.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(require 'libmpdel)
(require 'mpdel-core)


;;; Customization

(defgroup mpdel-playlist nil
  "Display and manipulate MPD playlists."
  :group 'libmpdel)

(defface mpdel-playlist--current-song-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face to highlight current song in playlist."
  :group 'mpdel-playlist)


;;; Private variables
(defvar-local mpdel-playlist-playlist nil
  "Playlist displayed in the buffer.")


;;; Helper functions

(cl-defgeneric mpdel-playlist--buffer (playlist)
  "Return buffer displaying PLAYLIST.")

(cl-defmethod mpdel-playlist--buffer ((_ (eql current-playlist)))
  (get-buffer-create "*MPDEL Current Playlist*"))

(cl-defmethod mpdel-playlist--buffer ((stored-playlist libmpdel-stored-playlist))
  (get-buffer-create (format "*MPDEL Playlist: %s*"
                             (libmpdel-entity-name stored-playlist))))

(defun mpdel-playlist--song-to-list-entry (song)
  "Convert SONG to a format suitable for the tabulated list."
  (list song
        (vector
         (or (libmpdel-entity-name song) "")
         (or (libmpdel-song-track song) "")
         (or (libmpdel-album-name song) "")
         (or (libmpdel-song-disk song) "")
         (or (libmpdel-entity-date song) "")
         (or (libmpdel-artist-name song) ""))))

(defun mpdel-playlist-go-to-song (&optional song)
  "Move point to SONG, currently-played song if nil.
Return non-nil if SONG is found, nil otherwise."
  (mpdel-core-go-to-entity (or song (libmpdel-current-song))))

(defun mpdel-playlist-highlight-song (&optional song)
  "Highlight SONG, current song if nil."
  (save-excursion
    (when (mpdel-playlist-go-to-song song)
      (let ((inhibit-read-only t))
        (put-text-property (line-beginning-position) (line-end-position)
                           'face 'mpdel-playlist--current-song-face)))))

(defun mpdel-playlist--save-playlist-status ()
  "Return an object representing selection.
Restore selection with `mpdel-playlist--restore-playlist-status'."
  (cons
   (mpdel-core-entity-at-point (point))
   (mpdel-core-entity-at-point (mark t))))

(defun mpdel-playlist--restore-playlist-status (status)
  "Restore playlist selection STATUS.
STATUS has been returned by `mpdel-playlist--save-playlist-status'."
  (when (cdr status)
    (mpdel-playlist-go-to-song (cdr status))
    (push-mark nil t))
  (when (car status)
    (mpdel-playlist-go-to-song (car status))))

(defun mpdel-playlist--imenu-prev-index-position ()
  "Move point to previous line in playlist buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

(defun mpdel-playlist--imenu-extract-index-name ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (let ((song (mpdel-core-entity-at-point)))
    (format "%s/%s/%s"
            (or (libmpdel-artist-name song) "??")
            (or (libmpdel-album-name song) "??")
            (libmpdel-entity-name song))))


;;; Commands

(defun mpdel-playlist-refresh (&optional buffer)
  "Clear and re-populate the playlist BUFFER.
Use current buffer if BUFFER is nil."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (libmpdel-list
         mpdel-playlist-playlist
         (lambda (songs)
           (let ((playlist-status (mpdel-playlist--save-playlist-status)))
             (setq tabulated-list-entries (mapcar #'mpdel-playlist--song-to-list-entry songs))
             (tabulated-list-print)
             (mpdel-playlist--restore-playlist-status playlist-status)
             (when (and (not (libmpdel-stopped-p)) (libmpdel-current-playlist-p mpdel-playlist-playlist))
               (mpdel-playlist-highlight-song)))))))))

(defun mpdel-playlist-delete ()
  "Delete selected songs from current playlist."
  (interactive)
  (let ((songs (mpdel-core-selected-entities)))
    (when songs
      (libmpdel-playlist-delete songs mpdel-playlist-playlist)
      ;; Move point to the closest non-deleted song
      (forward-line 1)
      (when (= (point) (point-max))
        (forward-line -2)))))

(defun mpdel-playlist-play ()
  "Start playing the song at point."
  (interactive)
  (if (libmpdel-current-playlist-p mpdel-playlist-playlist)
      (libmpdel-play-song (mpdel-core-entity-at-point))
    (mpdel-core-insert-current-playlist)))

(defun mpdel-playlist-move-up ()
  "Move selected songs up in the current playlist."
  (interactive)
  (let ((songs (mpdel-core-selected-entities)))
    (when songs
      (libmpdel-playlist-move-up songs))))

(defun mpdel-playlist-move-down ()
  "Move selected songs down in the current playlist."
  (interactive)
  (let ((songs (mpdel-core-selected-entities)))
    (when songs
      (libmpdel-playlist-move-down songs))))

(defun mpdel-playlist-save ()
  "Save current playlist into a new stored playlist.
Ask for stored playlist name."
  (interactive)
  (if (libmpdel-current-playlist-p mpdel-playlist-playlist)
      (call-interactively #'libmpdel-playlist-save)
    (user-error "You can only save from the current playlist")))

(defun mpdel-playlist--register-to-hooks (buffer)
  "Register to several hooks to refresh BUFFER."
  (with-current-buffer buffer
    (let* ((refresh-fn (lambda () (mpdel-playlist-refresh buffer)))
           (playlist mpdel-playlist-playlist)
           (hooks (if (libmpdel-stored-playlist-p playlist)
                      '(libmpdel-stored-playlist-changed-hook)
                    '(libmpdel-current-playlist-changed-hook
                      libmpdel-current-song-changed-hook
                      libmpdel-player-changed-hook))))
      (mapc (lambda (hook) (add-hook hook refresh-fn)) hooks)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (mapc (lambda (hook) (remove-hook hook refresh-fn)) hooks))
                nil t))))

;;;###autoload
(defun mpdel-playlist-open (&optional playlist)
  "Open a buffer with PLAYLIST, current playlist if nil."
  (interactive)
  (let* ((playlist (or playlist 'current-playlist))
         (buffer (mpdel-playlist--buffer playlist)))
    (with-current-buffer buffer
      (mpdel-playlist-mode)
      (setq mpdel-playlist-playlist playlist)
      (mpdel-playlist-refresh buffer))
    (switch-to-buffer buffer)
    (mpdel-playlist--register-to-hooks buffer)))

;;;###autoload
(defun mpdel-playlist-open-stored-playlist ()
  "Ask for a stored playlist and open it."
  (interactive)
  (libmpdel-funcall-on-stored-playlist #'mpdel-playlist-open))


;;; Major mode

(defvar mpdel-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from both `mpdel-core-map' and
    ;; `tabulated-list-mode-map':
    (set-keymap-parent
     map
     (make-composed-keymap mpdel-core-map tabulated-list-mode-map))
    (define-key map (kbd "g") #'mpdel-playlist-refresh)
    (define-key map (kbd "k") #'mpdel-playlist-delete)
    (define-key map (kbd "p") #'mpdel-playlist-play)
    (define-key map (kbd "<M-up>") #'mpdel-playlist-move-up)
    (define-key map (kbd "<M-down>") #'mpdel-playlist-move-down)
    (define-key map (kbd "C-x C-s") #'mpdel-playlist-save)
    map))

(define-derived-mode mpdel-playlist-mode tabulated-list-mode "Playlist"
  "Display and manipulate the current MPD playlist."
  (setq tabulated-list-format
        (vector (list "Title" 30 nil)
                (list "#" 6 nil)
                (list "Album" 30 nil)
                (list "Disk" 4 nil)
                (list "Date" 5 nil)
                (list "Artist" 0 nil)))
  (tabulated-list-init-header)
  (setq imenu-prev-index-position-function #'mpdel-playlist--imenu-prev-index-position)
  (setq imenu-extract-index-name-function #'mpdel-playlist--imenu-extract-index-name))

(provide 'mpdel-playlist)
;;; mpdel-playlist.el ends here
