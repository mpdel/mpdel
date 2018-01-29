;;; ivy-mpdel.el --- Ivy interface to navigate MPD     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://github.com/DamienCassou/mpdel
;; Package-requires: ((emacs "24.3"))
;; Keywords: multimedia
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

;; Ivy interface to mpdel.  Provide `ivy-mpdel-artists' as entry point
;; command.  Bind that to a convenient binding.

;;; Code:

(require 'ivy)

(require 'libmpdel)


;;; Helper functions

(defun ivy-mpdel--wrap (entity)
  "Wrap ENTITY into an object suitable for `ivy-read'."
  (cons (substring-no-properties (libmpdel-entity-name entity)) entity))

(defun ivy-mpdel--wrap-all (entities)
  "Wrap all ENTITIES into objects suitable for `ivy-read'."
  (mapcar #'ivy-mpdel--wrap entities))

(defun ivy-mpdel--unwrap (pair)
  "Return the entity wrapped within PAIR."
  (cdr pair))

(defun ivy-mpdel--apply-unwrapped (function)
  "Return a function applying FUNCTION after unwrapping its argument."
  (lambda (pair) (funcall function
                     (ivy-mpdel--unwrap pair))))

(defun ivy-mpdel--stored-playlist-add-entity (entity)
  "Select a stored playlist and add ENTITY to it."
  (libmpdel-list-stored-playlists
   (lambda (stored-playlists)
     (libmpdel-playlist-add
      entity
      (libmpdel-completing-read
       (format "Add %S to playlist: " (libmpdel-entity-name entity))
       stored-playlists)))))

(defun ivy-mpdel--playlist-add-entity (entity)
  "Add ENTITY to current playlist."
  (libmpdel-playlist-add entity (libmpdel-current-playlist)))


;;; Ivy interface

(defun ivy-mpdel-artists ()
  "Select music from a list of artists."
  (interactive)
  (libmpdel-list-artists
   (lambda (artists)
     (ivy-read "Artist: "
               (ivy-mpdel--wrap-all artists)
               :action (ivy-mpdel--apply-unwrapped #'ivy-mpdel-albums)
               :caller 'ivy-mpdel-artists))))


(defun ivy-mpdel-albums (artist)
  "Select music from a list of albums for ARTIST."
  (libmpdel-list
   artist
   (lambda (albums)
     (ivy-read "Album: "
               (ivy-mpdel--wrap-all albums)
               :action (ivy-mpdel--apply-unwrapped #'ivy-mpdel-album-songs)
               :caller 'ivy-mpdel-albums))))

(defun ivy-mpdel-album-songs (album)
  "Select a song from a list of songs of ALBUM."
  (libmpdel-list album #'ivy-mpdel-songs))

(defun ivy-mpdel-stored-playlists ()
  "Select music from a stored playlist or edit one."
  (interactive)
  (libmpdel-list-stored-playlists
   (lambda (stored-playlists)
     (ivy-read "Playlist: "
               (ivy-mpdel--wrap-all stored-playlists)
               :action (ivy-mpdel--apply-unwrapped #'ivy-mpdel-stored-playlist-songs)
               :caller 'ivy-mpdel-stored-playlists))))

(defun ivy-mpdel-stored-playlist-songs (stored-playlist)
  "Select a song from the list of songs of STORED-PLAYLIST."
  (libmpdel-list stored-playlist #'ivy-mpdel-songs))

(defun ivy-mpdel-songs (songs)
  "Select a song from a list of SONGS."
  (ivy-read "Song: "
            (ivy-mpdel--wrap-all songs)
            :action (ivy-mpdel--apply-unwrapped #'ivy-mpdel--playlist-add-entity)
            :caller 'ivy-mpdel-songs))
(mapc
 (lambda (ivy-caller)
   (ivy-add-actions
    ivy-caller
    `(("a" ,(ivy-mpdel--apply-unwrapped #'ivy-mpdel--playlist-add-entity) "Add to current playlist")
      ("r" ,(ivy-mpdel--apply-unwrapped #'libmpdel-playlist-replace) "Replace current playlist")
      ("P" ,(ivy-mpdel--apply-unwrapped #'ivy-mpdel--stored-playlist-add-entity) "Add to stored playlist"))))
 '(ivy-mpdel-artists ivy-mpdel-albums ivy-mpdel-stored-playlists ivy-mpdel-songs))

(ivy-add-actions
 'ivy-mpdel-songs
 `(("p" ,(ivy-mpdel--apply-unwrapped
          (lambda (song)
            (ivy-mpdel-albums (libmpdel-artist song)))) "See all albums of artist")))

(ivy-add-actions
 'ivy-mpdel-albums
 `(("p" (lambda (_) (ivy-mpdel-artists)) "See all artists")))

(provide 'ivy-mpdel)
;;; ivy-mpdel.el ends here
