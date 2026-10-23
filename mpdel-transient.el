;;; mpdel-transient.el --- MPDel command menus  -*- lexical-binding: t; -*-

;; Copyright © 2026  coopi <coldsideofyourpillow@disroot.org>

;; Author: coopi <coldsideofyourpillow@disroot.org>
;; Keywords: multimedia
;; URL: https://github.com/mpdel/mpdel
;; Package-Requires: ((emacs "28.1") (libmpdel "1.2.0") (navigel "0.7.0"))
;; Version: 2.1.1

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

;; Transient menus for MPDel commands.

;;; Code:

(require 'transient)
(require 'mpdel-browser)
(require 'mpdel-core)
(require 'mpdel-playlist)
(require 'mpdel-song)
(require 'mpdel-tablist)

;;; Buffer predicates

(defun mpdel-transient--tablist-p ()
  "Return non-nil in an MPDel list buffer."
  (derived-mode-p 'mpdel-tablist-mode))

(defun mpdel-transient--song-p ()
  "Return non-nil in an MPDel song buffer."
  (derived-mode-p 'mpdel-song-mode))

(defun mpdel-transient--playlist-p ()
  "Return non-nil in a playlist buffer."
  (derived-mode-p 'mpdel-playlist-mode))

(defun mpdel-transient--current-playlist-p ()
  "Return non-nil in the current playlist buffer."
  (derived-mode-p 'mpdel-playlist-current-playlist-mode))

(defun mpdel-transient--stored-playlist-p ()
  "Return non-nil in a stored playlist buffer."
  (derived-mode-p 'mpdel-playlist-stored-playlist-mode))

(defun mpdel-transient--non-playlist-tablist-p ()
  "Return non-nil in an MPDel list other than a playlist."
  (and (mpdel-transient--tablist-p)
       (not (mpdel-transient--playlist-p))))

(defun mpdel-transient--deletable-p ()
  "Return non-nil when items can be deleted in this buffer."
  (or (mpdel-transient--playlist-p)
      (and (derived-mode-p 'mpdel-browser-mode)
           (eq navigel-entity 'stored-playlists))))

(defun mpdel-transient--song-at-point-p ()
  "Return non-nil when a song is at point in an MPDel list."
  (and (mpdel-transient--tablist-p)
       (libmpdel-song-p (navigel-entity-at-point))))

;;; Command menus

(transient-define-prefix mpdel-transient-playback ()
  "Control playback and volume."
  [["Playback"
    ("SPC" "Play/pause" libmpdel-playback-play-pause :transient t)
    ("p" "Play" libmpdel-play :transient t)
    ("x" "Stop" libmpdel-stop :transient t)
    ("M-n" "Next song" libmpdel-playback-next :transient t)
    ("M-p" "Previous song" libmpdel-playback-previous :transient t)
    ("0" "Restart song" mpdel-core-restart-song :transient t)]
   ["Volume"
    ("+" "Volume up" mpdel-core-volume-increase :transient t)
    ("-" "Volume down" mpdel-core-volume-decrease :transient t)
    ("v" "Set volume" libmpdel-playback-set-volume)]]
  [["Random and repeat"
    ("r" "Random on" libmpdel-playback-set-random :transient t)
    ("R" "Random off" libmpdel-playback-unset-random :transient t)
    ("t" "Repeat on" libmpdel-playback-set-repeat :transient t)
    ("T" "Repeat off" libmpdel-playback-unset-repeat :transient t)]
   ["Single mode"
    ("s" "Repeat one song" libmpdel-playback-set-single-forever :transient t)
    ("S" "Single off" libmpdel-playback-set-single-never :transient t)
    ("o" "Single once" libmpdel-playback-set-single-once :transient t)]])

(transient-define-prefix mpdel-transient-seek ()
  "Seek within the current song."
  [["Backward"
    ("B" "Small step" mpdel-song-small-decrement :transient t)
    ("b" "Normal step" mpdel-song-normal-decrement :transient t)
    ("M-b" "Large step" mpdel-song-large-decrement :transient t)]
   ["Forward"
    ("F" "Small step" mpdel-song-small-increment :transient t)
    ("f" "Normal step" mpdel-song-normal-increment :transient t)
    ("M-f" "Large step" mpdel-song-large-increment :transient t)]]
  ["Position"
   ("0" "Restart song" mpdel-core-restart-song :transient t)
   ("s" "Seek to position" libmpdel-playback-seek)])

(transient-define-prefix mpdel-transient-library ()
  "Browse the music library and playlists."
  [["Library"
    (":" "Browser" mpdel-browser-open)
    ("N" "Artists" mpdel-core-open-artists)
    ("a" "Albums" mpdel-core-open-albums)
    ("g" "Genres" mpdel-core-open-genres)
    ("d" "Directories" mpdel-core-open-directories)]
   ["Playlists and song"
    ("l" "Current playlist" mpdel-playlist-open)
    ("L" "Stored playlists" mpdel-core-open-stored-playlists)
    ("o" "Open stored playlist" mpdel-playlist-open-stored-playlist)
    ("v" "Current song" mpdel-song-open)]])

(transient-define-prefix mpdel-transient-search ()
  "Search the MPD database."
  ["Search by"
   ("s" "Song title" mpdel-core-search-by-title)
   ("l" "Album" mpdel-core-search-by-album)
   ("r" "Artist" mpdel-core-search-by-artist)
   ("g" "Genre" mpdel-core-search-by-genre)
   ("f" "MPD filter expression" mpdel-core-search-by-filter)])

(transient-define-prefix mpdel-transient-playlist ()
  "Edit the current or a stored playlist."
  [["Selected items"
    :if mpdel-transient--tablist-p
    ("a" "Add to current playlist" mpdel-core-add-to-current-playlist)
    ("A" "Add to stored playlist" mpdel-core-add-to-stored-playlist)
    ("r" "Replace current playlist" mpdel-core-replace-current-playlist)
    ("R" "Replace stored playlist" mpdel-core-replace-stored-playlist)
    ("P" "Play song at point" mpdel-playlist-play
     :if mpdel-transient--current-playlist-p)
    ("P" "Play selection" mpdel-playlist-play
     :if mpdel-transient--stored-playlist-p)
    ("P" "Play selection" mpdel-core-insert-current-playlist
     :if mpdel-transient--non-playlist-tablist-p)]
   ["Current playlist"
    ("c" "Clear current playlist" mpdel-playlist-clear-current)
    ("s" "Save as stored playlist" mpdel-playlist-save
     :if mpdel-transient--current-playlist-p)
    ("<M-up>" "Move songs up" mpdel-playlist-move-up
     :if mpdel-transient--current-playlist-p)
    ("<M-down>" "Move songs down" mpdel-playlist-move-down
     :if mpdel-transient--current-playlist-p)]]
  ["Delete"
   :if mpdel-transient--deletable-p
   ("k" "Delete selected items" tablist-do-delete)])

(transient-define-prefix mpdel-transient-navigation ()
  "Navigate and mark items in an MPDel buffer."
  :refresh-suffixes t
  [["Navigate"
    :if mpdel-transient--tablist-p
    ("RET" "Open item" tablist-find-entry)
    ("^" "Open parent" navigel-open-parent)
    ("C-x C-j" "Dired on item" mpdel-core-dired)
    ("n" "Next line" next-line :transient t)
    ("p" "Previous line" previous-line :transient t)]
   ["Mark and refresh"
    :if mpdel-transient--tablist-p
    ("m" "Mark item" tablist-mark-forward :transient t)
    ("u" "Unmark item" tablist-unmark-forward :transient t)
    ("U" "Unmark all" tablist-unmark-all-marks :transient t)
    ("g" "Refresh list" revert-buffer :transient t)]]
  ["Song at point"
   :if mpdel-transient--song-at-point-p
   ("V" "Show song details" tablist-find-entry)])

(transient-define-prefix mpdel-transient-other ()
  "Select a server, update the database, or manage a song buffer."
  [["MPD server"
    ("C" "Switch profile" libmpdel-connect-profile)
    ("u" "Update database" libmpdel-database-update)]
   ["Song buffer"
    :if mpdel-transient--song-p
    ("g" "Refresh song" mpdel-song-refresh :transient t)
    ("p" "Play this song" mpdel-song-play)
    ("q" "Quit song window" mpdel-song-quit-window)]])

;;; Dispatch menu

;;;###autoload (autoload 'mpdel-transient "mpdel-transient" nil t)
(transient-define-prefix mpdel-transient ()
  "Show MPDel commands in a Transient menu."
  [["Playback and volume"
    ("SPC" "Play/pause" libmpdel-playback-play-pause :transient t)
    ("M-n" "Next song" libmpdel-playback-next :transient t)
    ("M-p" "Previous song" libmpdel-playback-previous :transient t)
    ("+" "Volume up" mpdel-core-volume-increase :transient t)
    ("-" "Volume down" mpdel-core-volume-decrease :transient t)]
   ["Seek"
    ("B" "Back small" mpdel-song-small-decrement :transient t)
    ("b" "Back normal" mpdel-song-normal-decrement :transient t)
    ("M-b" "Back large" mpdel-song-large-decrement :transient t)
    ("F" "Forward small" mpdel-song-small-increment :transient t)
    ("f" "Forward normal" mpdel-song-normal-increment :transient t)
    ("M-f" "Forward large" mpdel-song-large-increment :transient t)]]
  [["View"
    (":" "Browser" mpdel-browser-open)
    ("l" "Current playlist" mpdel-playlist-open)
    ("L" "Stored playlists" mpdel-core-open-stored-playlists)
    ("N" "Artists" mpdel-core-open-artists)
    ("v" "Current song" mpdel-song-open)]
   ["Selected items"
    :if mpdel-transient--tablist-p
    ("a" "Add to current" mpdel-core-add-to-current-playlist)
    ("A" "Add to stored" mpdel-core-add-to-stored-playlist)
    ("r" "Replace current" mpdel-core-replace-current-playlist)
    ("R" "Replace stored" mpdel-core-replace-stored-playlist)
    ("P" "Play song at point" mpdel-playlist-play
     :if mpdel-transient--current-playlist-p)
    ("P" "Play selection" mpdel-playlist-play
     :if mpdel-transient--stored-playlist-p)
    ("P" "Play selection" mpdel-core-insert-current-playlist
     :if mpdel-transient--non-playlist-tablist-p)]]
  [["Navigation"
    :if mpdel-transient--tablist-p
    ("RET" "Open item" tablist-find-entry)
    ("^" "Open parent" navigel-open-parent)
    ("C-x C-j" "Dired on item" mpdel-core-dired)
    ("n" "Next line" next-line :transient t)
    ("p" "Previous line" previous-line :transient t)]
   ["More commands"
    ("s" "Search" mpdel-transient-search)
    ("d" "Library" mpdel-transient-library)
    ("e" "Edit playlists" mpdel-transient-playlist)
    ("t" "Playback modes" mpdel-transient-playback)
    ("z" "Seek" mpdel-transient-seek)
    ("x" "Server and song" mpdel-transient-other)
    ("m" "Mark and navigate" mpdel-transient-navigation
     :if mpdel-transient--tablist-p)
    ("C" "Switch profile" libmpdel-connect-profile)]])

(provide 'mpdel-transient)
;;; mpdel-transient.el ends here
