;;; mpdel-core.el --- Provide code to be reused by mpdel modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitlab.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
;; Version: 0.5.0

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

;; This file provides some common ground for all MPDel
;; user-interfaces.

;;; Code:
(require 'tabulated-list)

(require 'libmpdel)


;;; Helper functions

(defun mpdel-core--points-in-region (start end)
  "Return a list of points for lines between START and END."
  (save-excursion
    (let (points)
      (goto-char start)
      (while (and (<= (point) end) (< (point) (point-max)))
        (push (line-beginning-position) points)
        (forward-line 1))
      (reverse points))))

(cl-defgeneric mpdel-core--entity-at-point (_pos _mode)
  "Return entity at POS.
MODE is the current buffer's major mode."
  (message "No entity at point."))

(cl-defmethod mpdel-core--entity-at-point (pos (_mode (derived-mode tabulated-list-mode)))
  (tabulated-list-get-id pos))

(defun mpdel-core-selected-entities ()
  "Return entities within active region or at point."
  (cond
   ((use-region-p)
    (mapcar (lambda (pos) (mpdel-core--entity-at-point pos major-mode))
            (mpdel-core--points-in-region (region-beginning) (region-end))))
   ((= (point) (point-max)) nil)
   (t (list (mpdel-core--entity-at-point (point) major-mode)))))

(defun mpdel-core-entity-at-point (&optional pos buffer)
  "Return entity at POS in BUFFER.
Use point if POS is nil and use current buffer if BUFFER is nil."
  (with-current-buffer (or buffer (current-buffer))
    (mpdel-core--entity-at-point (or pos (point)) major-mode)))

(cl-defgeneric mpdel-core--open-entity (entity &optional target)
  "Open buffer displaying information about ENTITY.

If TARGET is non-nil and is in buffer, move point to it.")

(defun mpdel-core-go-to-entity (entity &optional buffer)
  "Move point to ENTITY in BUFFER, current one if nil.
Return non-nil if ENTITY is found, nil otherwise."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (while (and (not (= (point) (point-max)))
                (not (libmpdel-equal (mpdel-core-entity-at-point) entity)))
      (forward-line 1))
    (not (= (point) (point-max)))))


;;; Commands

(defun mpdel-core-add-to-current-playlist ()
  "Add selected entities to current playlist."
  (interactive)
  (libmpdel-current-playlist-add (mpdel-core-selected-entities)))

(defun mpdel-core-add-to-stored-playlist ()
  "Add selected entities to a stored playlist."
  (interactive)
  (libmpdel-stored-playlist-add (mpdel-core-selected-entities)))

(defun mpdel-core-replace-current-playlist ()
  "Replace current playlist with selected entities."
  (interactive)
  (libmpdel-current-playlist-replace (mpdel-core-selected-entities)))

(defun mpdel-core-replace-stored-playlist ()
  "Replace a stored playlist with selected entities."
  (interactive)
  (libmpdel-stored-playlist-replace (mpdel-core-selected-entities)))

(defun mpdel-core-insert-current-playlist ()
  "Insert selected entities after currently-played song."
  (interactive)
  (libmpdel-current-playlist-insert (mpdel-core-selected-entities)))

(defun mpdel-core-dired (&optional pos)
  "Open dired on the entity at POS, point if nil."
  (interactive)
  (libmpdel-dired (mpdel-core-entity-at-point pos)))

(defun mpdel-core-open-entity-at-point (&optional pos)
  "Open buffer displaying information about entity at POS.
Use point if POS is nil."
  (interactive)
  (mpdel-core--open-entity (mpdel-core-entity-at-point (or pos (point)))))

(defun mpdel-core-open-entity-parent-at-point (&optional pos)
  "Open a navigator showing the parent of entity at POS.
If POS is nil, use point.

For example, if point is on a song, open a navigator on its
album."
  (interactive)
  (let ((entity (mpdel-core-entity-at-point pos)))
    (mpdel-core--open-entity (libmpdel-entity-parent entity) entity)))


;;; Define the mpdel shared map

(defvar mpdel-core-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'libmpdel-playback-play-pause)
    (define-key map (kbd "M-n") #'libmpdel-playback-next)
    (define-key map (kbd "M-p") #'libmpdel-playback-previous)
    (define-key map (kbd "a") #'mpdel-core-add-to-current-playlist)
    (define-key map (kbd "A") #'mpdel-core-add-to-stored-playlist)
    (define-key map (kbd "r") #'mpdel-core-replace-current-playlist)
    (define-key map (kbd "R") #'mpdel-core-replace-stored-playlist)
    (define-key map (kbd "p") #'mpdel-core-insert-current-playlist)
    (define-key map (kbd "C-x C-j") #'mpdel-core-dired)
    (define-key map (kbd "RET") #'mpdel-core-open-entity-at-point)
    (define-key map (kbd "^") #'mpdel-core-open-entity-parent-at-point)
    map)
  "Keymap for all mpdel buffers.")

;; Make it possible to activate `mpdel-core-map' from a keybinding:
(fset 'mpdel-core-map mpdel-core-map)

(provide 'mpdel-core)
;;; mpdel-core.el ends here
