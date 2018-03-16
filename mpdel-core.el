;;; mpdel-core.el --- Provide code to be reused by mpdel modes  -*- lexical-binding: t; -*-

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

;; This file provides some common ground for all MPDel
;; user-interfaces.

;;; Code:

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

(defun mpdel-core--selected-entities (point-to-entity)
  "Apply POINT-TO-ENTITY for each line within active region or at point."
  (cond
   ((use-region-p)
    (mapcar point-to-entity (mpdel-core--points-in-region (region-beginning) (region-end))))
   ((= (point) (point-max)) nil)
   (t (list (funcall point-to-entity (point))))))

(defvar mpdel-core-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'libmpdel-playback-play-pause)
    (define-key map (kbd "M-n") #'libmpdel-playback-next)
    (define-key map (kbd "M-p") #'libmpdel-playback-previous)
    map)
  "Keymap for all mpdel buffers.")

;; Make it possible to activate `mpdel-core-map' from a keybinding:
(fset 'mpdel-core-map mpdel-core-map)

(provide 'mpdel-core)
;;; mpdel-core.el ends here
