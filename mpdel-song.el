;;; mpdel-song.el --- Let the user control current song  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://github.com/DamienCassou/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 0.1.0

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

;; Executing `mpdel-song' opens a small buffer letting the user get
;; information and control the current song.

;;; Code:
(require 'map)

(require 'libmpdel)


;;; Customization

(defgroup mpdel-song nil
  "Control MPD's current song."
  :group 'libmpdel)

(defcustom mpdel-song-small-increment "+2"
  "Time to seek to slowly move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-small-decrement "-2"
  "Time to seek to slowly move backward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-normal-increment "+10"
  "Time to seek to move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-normal-decrement "-10"
  "Time to seek to move backward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-large-increment "+30"
  "Time to seek to quickly move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-large-decrement "-30"
  "Time to seek to quickly move backward."
  :type 'string
  :group 'mpdel-song)


;;; Helper functions

(defvar mpdel-song--timer nil
  "Store timer to refresh the seek buffer.")

(defun mpdel-song--start-timer ()
  "Start refresh timer."
  (unless mpdel-song--timer
    (setq mpdel-song--timer (run-at-time t 1 #'mpdel-song-refresh (current-buffer)))))

(defun mpdel-song--stop-timer ()
  "Stop refresh timer."
  (when mpdel-song--timer
    (cancel-timer mpdel-song--timer)
    (setq mpdel-song--timer nil)))

(defun mpdel-song--seek (time)
  "Seek TIME within current song and refresh current buffer."
  (libmpdel-playback-seek
   time
   (lambda ()
     (message "Seek feedback!")
     (mpdel-song-refresh))))

(defun mpdel-song--display-play-state ()
  "Give information about current play state and control timer."
  (pcase (libmpdel-play-state)
    ('play (insert "Currently playing\n")
           (mpdel-song--start-timer))
    ('pause (insert "Currently paused\n")
            (mpdel-song--stop-timer))
    ('stop (insert "Currently stopped\n")
           (mpdel-song--stop-timer))))

(defun mpdel-song--display-play-time (data)
  "Give information about current play time in DATA."
  (insert
   (format "%s / %s"
           (libmpdel-time-to-string (map-elt data 'elapsed))
           (libmpdel-time-to-string (map-elt data 'duration)))))

(defun mpdel-song--display-metadata ()
  "Give information about current song metadata."
  (let* ((song (libmpdel-current-song))
         (title (libmpdel-entity-name song))
         (album (libmpdel-album-name song))
         (artist (libmpdel-artist-name song)))
    (insert (format "Title: %s\nArtist: %s\nAlbum: %s\n"
                    title
                    artist
                    album))))

(defun mpdel-song--refresh-from-data (data &optional buffer)
  "Write DATA to BUFFER, current buffer if nil.

DATA is an alist returned by MPD server as answer to 'status'.
In particular, it must contain key symbol `elapsed' and symbol
`duration'."
  (let ((buffer (or buffer (current-buffer)))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (mpdel-song--display-play-state)
      (mpdel-song--display-metadata)
      (mpdel-song--display-play-time data))))


;;; Public interface

(defun mpdel-song-refresh (&optional buffer)
  "Refresh MPDEL seek BUFFER, current buffer if nil."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (libmpdel-send-command "status" (lambda (data) (mpdel-song--refresh-from-data data buffer)))))

(defun mpdel-song-open ()
  "Open a buffer to guide the user seeking inside current song."
  (interactive)
  (let ((buffer (get-buffer-create "*MPDEL song*")))
    (with-current-buffer buffer
      (mpdel-song-refresh)
      (mpdel-song-mode)
      (add-hook 'libmpdel-player-changed-hook (apply-partially #'mpdel-song-refresh buffer))
      (add-hook 'kill-buffer-hook #'mpdel-song--stop-timer nil t)
      (pop-to-buffer (current-buffer)))))

(defmacro mpdel-song--seek-command (time)
  "Return a command seeking TIME in current song."
  `(lambda ()
     (interactive)
     (mpdel-song--seek ,time)))

(defmacro mpdel-song--call-refresh-command (function)
  "Return a command calling FUNCTION and refreshing buffer."
  `(lambda ()
     (interactive)
     (funcall ,function)
     (mpdel-song-refresh)))

(defvar mpdel-song-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'mpdel-song-refresh)
    ;; force kill instead of burrying to stop the timer:
    (define-key map (kbd "q") (lambda () (interactive) (quit-window t)))
    (define-key map (kbd "F") (mpdel-song--seek-command mpdel-song-small-increment))
    (define-key map (kbd "f") (mpdel-song--seek-command mpdel-song-normal-increment))
    (define-key map (kbd "M-f") (mpdel-song--seek-command mpdel-song-large-increment))
    (define-key map (kbd "B") (mpdel-song--seek-command mpdel-song-small-decrement))
    (define-key map (kbd "b") (mpdel-song--seek-command mpdel-song-normal-decrement))
    (define-key map (kbd "M-b") (mpdel-song--seek-command mpdel-song-large-decrement))
    (define-key map (kbd "SPC") (mpdel-song--call-refresh-command #'libmpdel-playback-play-pause))
    (define-key map (kbd "]") (mpdel-song--call-refresh-command #'libmpdel-playback-next))
    (define-key map (kbd "[") (mpdel-song--call-refresh-command #'libmpdel-playback-previous))
    map))

(define-derived-mode mpdel-song-mode special-mode "MPDEL song"
  "Guide the user to seek inside current song.")

(provide 'mpdel-song)
;;; mpdel-song.el ends here
