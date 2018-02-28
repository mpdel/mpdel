;;; mpdel-playlist.el --- Display and manipulate MPD playlists  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

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
(defvar-local mpdel-playlist--songs nil
  "Hashtable mapping identifiers to songs.")

(defvar-local mpdel-playlist-playlist nil
  "Playlist displayed in the buffer.")


;;; Helper functions

(cl-defgeneric mpdel-playlist--buffer (playlist)
  "Return buffer displaying PLAYLIST.")

(cl-defmethod mpdel-playlist--buffer ((_ libmpdel-current-playlist))
  (get-buffer-create "*MPDEL Current Playlist*"))

(cl-defmethod mpdel-playlist--buffer ((stored-playlist libmpdel-stored-playlist))
  (get-buffer-create (format "*MPDEL Playlist: %s*"
                             (libmpdel-entity-name stored-playlist))))

(defun mpdel-playlist--song-to-list-entry (song-id)
  "Convert SONG-ID to a format suitable for the tabulated list."
  (let ((song (gethash song-id mpdel-playlist--songs)))
    (list song-id
          (vector
           (libmpdel-entity-name song)
           (libmpdel-song-track song)
           (or (libmpdel-album-name song) "")
           (or (libmpdel-artist-name song) "")))))

(defun mpdel-playlist--populate-hashtable (songs)
  "Save SONGS in `mpdel-playlist--songs'."
  (setq mpdel-playlist--songs (make-hash-table :test #'equal))
  (let ((use-song-id (not (cl-find-if-not #'libmpdel-song-id songs)))
        (fallback-id 1))
    (mapc
     (lambda (song)
       (puthash (if use-song-id (libmpdel-song-id song) (cl-incf fallback-id))
                song
                mpdel-playlist--songs))
     songs)))

(defun mpdel-playlist--song-id-at-point (&optional pos)
  "Return song ID at POS, point if nil."
  (let* ((pos (or pos (point))))
    (tabulated-list-get-id pos)))

(defun mpdel-playlist-song-at-point (&optional pos)
  "Return song at POS, point if nil."
  (let* ((song-id (mpdel-playlist--song-id-at-point pos)))
    (gethash song-id mpdel-playlist--songs)))

(defun mpdel-playlist--points-in-region (start end)
  "Return a list of points for lines between START and END."
  (save-excursion
    (let (points)
      (goto-char start)
      (while (and (<= (point) end) (< (point) (point-max)))
        (push (line-beginning-position) points)
        (forward-line 1))
      (reverse points))))

(defun mpdel-playlist--selected-songs ()
  "Return songs within active region or the song at point."
  (cond
   ((use-region-p)
    (mapcar #'mpdel-playlist-song-at-point (mpdel-playlist--points-in-region (region-beginning) (region-end))))
   ((= (point) (point-max)) nil)
   (t (list (mpdel-playlist-song-at-point)))))

(defun mpdel-playlist-go-to-song (&optional song-id)
  "Move point to SONG-ID, current song if nil.
Return non-nil if SONG-ID is found, nil otherwise."
  (let ((song-id (or song-id (libmpdel-song-id (libmpdel-current-song)))))
    (goto-char (point-min))
    (while (and (not (= (point) (point-max)))
                (not (equal (libmpdel-song-id (mpdel-playlist-song-at-point)) song-id)))
      (forward-line 1))
    (not (= (point) (point-max)))))

(defun mpdel-playlist-highlight-song (&optional song-id)
  "Highlight SONG-ID, current song if nil."
  (save-excursion
    (when (mpdel-playlist-go-to-song song-id)
      (let ((inhibit-read-only t))
        (put-text-property (line-beginning-position) (line-end-position)
                           'face 'mpdel-playlist--current-song-face)))))

(defun mpdel-playlist--save-playlist-status ()
  "Return an object representing selection.
Restore selection with `mpdel-playlist--restore-playlist-status'."
  (cons
   (mpdel-playlist--song-id-at-point (point))
   (mpdel-playlist--song-id-at-point (mark t))))

(defun mpdel-playlist--restore-playlist-status (status)
  "Restore playlist selection STATUS.
STATUS has been returned by `mpdel-playlist--save-playlist-status'."
  (when (cdr status)
    (mpdel-playlist-go-to-song (cdr status))
    (push-mark))
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
  (let ((song (mpdel-playlist-song-at-point)))
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
             (mpdel-playlist--populate-hashtable songs)
             (setq tabulated-list-entries (mapcar #'mpdel-playlist--song-to-list-entry (hash-table-keys mpdel-playlist--songs)))
             (tabulated-list-print)
             (mpdel-playlist--restore-playlist-status playlist-status)
             (when (and (not (libmpdel-stopped-p)) (libmpdel-current-playlist-p mpdel-playlist-playlist))
               (mpdel-playlist-highlight-song)))))))))

(defun mpdel-playlist-delete ()
  "Delete selected songs from current playlist."
  (interactive)
  (let ((songs (mpdel-playlist--selected-songs)))
    (when songs
      (libmpdel-playlist-delete songs mpdel-playlist-playlist)
      ;; Move point to the closest non-deleted song
      (forward-line 1)
      (when (= (point) (point-max))
        (forward-line -2)))))

(defun mpdel-playlist-play ()
  "Start playing the song at point."
  (interactive)
  (libmpdel-play-song (mpdel-playlist-song-at-point)))

(defun mpdel-playlist-move-up ()
  "Move selected songs up in the current playlist."
  (interactive)
  (let ((songs (mpdel-playlist--selected-songs)))
    (when songs
      (libmpdel-playlist-move-up songs))))

(defun mpdel-playlist-move-down ()
  "Move selected songs down in the current playlist."
  (interactive)
  (let ((songs (mpdel-playlist--selected-songs)))
    (when songs
      (libmpdel-playlist-move-down songs))))

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

(defun mpdel-playlist-dired ()
  "Open `dired' on song at point."
  (interactive)
  (libmpdel-dired (mpdel-playlist-song-at-point)))

;;;###autoload
(defun mpdel-playlist-open (&optional playlist)
  "Open a buffer with PLAYLIST, current playlist if nil."
  (interactive)
  (let* ((playlist (or playlist (libmpdel-current-playlist)))
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
  (libmpdel-list
   'stored-playlists
   (lambda (playlists)
     (let ((playlist (libmpdel-completing-read "Playlist: " playlists)))
       (mpdel-playlist-open playlist)))))


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
    (define-key map (kbd "RET") #'mpdel-playlist-play)
    (define-key map (kbd "<M-up>") #'mpdel-playlist-move-up)
    (define-key map (kbd "<M-down>") #'mpdel-playlist-move-down)
    (define-key map (kbd "C-x C-j") #'mpdel-playlist-dired)
    map))

(define-derived-mode mpdel-playlist-mode tabulated-list-mode "Playlist"
  "Display and manipulate the current MPD playlist."
  (setq tabulated-list-format
        (vector (list "Title" 30 t)
                (list "#" 6 t)
                (list "Album" 30 t)
                (list "Artist" 20 t)))
  (tabulated-list-init-header)
  (setq imenu-prev-index-position-function #'mpdel-playlist--imenu-prev-index-position)
  (setq imenu-extract-index-name-function #'mpdel-playlist--imenu-extract-index-name))

(provide 'mpdel-playlist)
;;; mpdel-playlist.el ends here
