;; -*- lexical-binding: t; -*-
;;; mpdel-nav.el --- TODO
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

(defvar mpdel-nav-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'mpdel-kill-buffer)
    (define-key map (kbd "<tab>") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "u") #'mpdel-nav-up)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "o") #'mpdel-playlist)
    (define-key map (kbd "p") #'previous-line)
    map))

(defvar mpdel-nav-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'mpdel-nav-push-for-play)
    (define-key map (kbd "RET") #'mpdel-nav-push-for-browse)
    map))

(define-button-type 'mpdel-nav-button
  'keymap mpdel-nav-button-map)

(defvar mpdel-nav-buffer-properties nil)

(defun mpdel-nav-get-buffer-property (prop)
  (plist-get mpdel-nav-buffer-properties prop))

(defun mpdel-nav-add-buffer-properties (&rest props)
  (loop
   for key in props by #'cddr
   for value in (cdr props) by #'cddr
   do (setq mpdel-nav-buffer-properties
            (plist-put mpdel-nav-buffer-properties key value))))

(define-derived-mode mpdel-nav-mode nil "MPDel"
  "..."
  (setq buffer-read-only t)
  (make-local-variable 'mpdel-nav-buffer-properties))

(defun mpdel-nav-button-data ()
  (let* ((button (button-at (point)))
         (terms (and button (button-get button 'terms)))
         (entities (and button (button-get button 'entities))))
    (cl-values terms entities)))

(defun mpdel-nav-buffer-data ()
  (cl-values
   (mpdel-nav-get-buffer-property 'currentTerm)
   (mpdel-nav-get-buffer-property 'ancestorTerms)
   (mpdel-nav-get-buffer-property 'ancestorEntities)))

(let ((mpdel-nav-browse-order '("title" "album" "artist")))
  (defun mpdel-nav-push-for-browse ()
    (interactive)
    (multiple-value-bind (terms entities)
        (mpdel-nav-button-data)
      (let ((pos (position (car terms) mpdel-nav-browse-order
                           :test #'string=)))
        (when (> pos 0)
          (let ((current (nth (1- pos) mpdel-nav-browse-order)))
            (mpdel-nav-create-buffer current terms entities)))))))

(defun mpdel-nav-up ()
  (interactive)
  (multiple-value-bind (currentTerm ancestorTerms ancestorEntities)
      (mpdel-nav-buffer-data)
    (when ancestorTerms
      (mpdel-nav-restore-position)
      (mpdel-nav-create-buffer
       (car ancestorTerms)
       (cdr ancestorTerms)
       (cdr ancestorEntities)
       (car ancestorEntities)))))

(defun mpdel-nav-push-for-play ()
  (interactive)
  (multiple-value-bind (terms entities)
      (mpdel-nav-button-data)
    (mpdel-add-entities-to-playlist terms entities)))

(defun mpdel-navigator ()
  (interactive)
  (setq mpdel-nav-position-history '(1))
  (mpdel-nav-create-buffer "artist" nil nil))

(defun mpdel-nav-create-buffer (currentTerm
                                ancestorTerms
                                ancestorEntities
                                &optional gotoEntity)
  (mpdel-send-command
   (mpdel-list-query currentTerm ancestorTerms ancestorEntities)
   (lambda (message)
     (mpdel-nav-create-buffer-ready message currentTerm
      ancestorTerms ancestorEntities gotoEntity))))

(defun mpdel-nav-create-buffer-ready (message
                                      currentTerm
                                      ancestorTerms
                                      ancestorEntities
                                      &optional gotoEntity)
  (with-current-buffer (get-buffer-create "*mpd-database*")
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (dolist (entity (mpdel-nav-sorted-entities message))
          (insert-text-button
           entity ;; label
           'type 'mpdel-nav-button
           'terms (cons currentTerm ancestorTerms)
           'entities (cons entity ancestorEntities))
        (newline 1)))
    (goto-char (point-min))
    (when gotoEntity
      (search-forward gotoEntity nil t)
      (move-beginning-of-line nil))
    (mpdel-nav-mode)
    (when (require 'mpdel-header nil t)
      (mpdel-header-add-buffer (current-buffer)))
    (mpdel-nav-add-buffer-properties
     'currentTerm currentTerm
     'ancestorTerms ancestorTerms
     'ancestorEntities ancestorEntities))
  (switch-to-buffer "*mpd-database*"))

(defun mpdel-nav-sorted-entities (message)
  (sort (mapcar #'cdar (mpdel-extract-data message))
        #'mpdel-string<-noncase))

(provide 'mpdel-nav)
