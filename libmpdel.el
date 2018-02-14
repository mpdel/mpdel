;;; libmpdel.el --- Communication with an MPD server  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

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

;; libmpdel provides the core feature

;;; Code:
(require 'time-stamp)
(require 'tq)
(require 'cl-lib)


;;; Customization

(defgroup libmpdel nil
  "Communication with an MPD server."
  :group 'comm)

(defcustom libmpdel-hostname "localhost"
  "MPD server location to connect to.  Also see `libmpdel-port'."
  :type 'string)

(defcustom libmpdel-port 6600
  "MPD server port to connect to.  Also see `libmpdel-hostname'."
  :type 'string)

(defcustom libmpdel-music-directory "~/Music"
  "MPD `music_directory' variable's value.

This is used to map MPD's music files to the filesystem."
  :type 'directory)

(defcustom libmpdel-current-playlist-changed-hook nil
  "Functions to call when the current playlist is modified."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-stored-playlist-changed-hook nil
  "Functions to call when a stored playlist is modified."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-player-changed-hook nil
  "Functions to call when the player status changes.
This includes starting, stopping and seeking music."
  :type 'hook
  :group 'libmpdel)

(defcustom libmpdel-current-song-changed-hook nil
  "Functions to call when the current song changes.
See `libmpdel-current-song-id'."
  :type 'hook
  :group 'libmpdel)


;;; Global private variables

(defvar libmpdel--connection nil
  "Current connection to the MPD server.
The logs of this connection are accessible in the *mpd* buffer.")

(defconst libmpdel--response-regexp
  (rx line-start
      (or
       (and "OK" (? " MPD " (one-or-more not-newline)))
       (and "ACK ["
            (one-or-more (any digit)) "@" (one-or-more (any digit))
            "] " (one-or-more not-newline)))
      "\n")
  "Regexp matching the responses sent by the MPD server.")

(defvar libmpdel--msghandlers nil
  "Current commands sent to the server.
Each element in the list is of the form (COMMAND HANDLER BUFFER).

COMMAND is the query sent to the server.  Even though this
information is not necessary, it is useful to better understand
the log.

HANDLER is a function executed when the answers to COMMAND comes
back.  The function must accept one parameter (usually named
MESSAGE) that will contain the answer.

BUFFER is a buffer that was active when COMMAND was sent.  This
buffer is made active again while executing HANDLER.

An invariant of this MPD client is that there is always an IDLE
command sent to the server (and its corresponding handler in this
variable).  This means our client is always registered to
notifications in the server.  When we want to send a command to
the server (for example to change the current song), we always
have to (1) cancel the IDLE first (with a \"noidle\"
command), (2) send the command we want, and (3) send the IDLE
command again.  Cancelling the current \"idle\" command is done
in `mpdel-send-command'.  Sending \"idle\" again is done in the
handler for \"idle\" that will be triggered when the empty answer
for the cancelled \"idle\" arrives.

Because MPD answers in the order the commands are sent, we know
that the first handler is the one to execute when we receive a
message from the server.")


;;; Data structures

(cl-defstruct (libmpdel-artist
               (:constructor libmpdel--artist-create)
               (:conc-name libmpdel--artist-))
  (name nil :read-only t))

(cl-defstruct (libmpdel-album
               (:constructor libmpdel--album-create)
               (:conc-name libmpdel--album-))
  (name nil :read-only t)
  (artist nil :read-only t))

(cl-defstruct (libmpdel-song
               (:constructor libmpdel--song-create)
               (:conc-name libmpdel--song-))
  (name nil :read-only t)
  (track nil :read-only t)
  (file nil :read-only t)
  (album nil :read-only t)
  (id nil :read-only t)
  (pos nil :read-only t))

(cl-defstruct (libmpdel-stored-playlist
               (:constructor libmpdel--stored-playlist-create)
               (:conc-name libmpdel--stored-playlist-))
  (name nil :read-only t))

(cl-defstruct (libmpdel-current-playlist
               (:constructor libmpdel--current-playlist-create)
               (:conc-name libmpdel--current-playlist-)))

(defun libmpdel-current-playlist ()
  "Return the current playlist."
  (libmpdel--current-playlist-create))

(cl-defgeneric libmpdel-artist-name (object)
  "Return artist name of OBJECT.")

(cl-defmethod libmpdel-artist-name ((artist libmpdel-artist))
  (libmpdel--artist-name artist))

(cl-defmethod libmpdel-artist-name ((album libmpdel-album))
  (libmpdel-artist-name (libmpdel--album-artist album)))

(cl-defmethod libmpdel-artist-name ((song libmpdel-song))
  (libmpdel-artist-name (libmpdel--song-album song)))

(cl-defgeneric libmpdel-artist (object)
  "Return artist of OBJECT.")

(cl-defmethod libmpdel-artist ((artist libmpdel-artist))
  artist)

(cl-defmethod libmpdel-artist ((album libmpdel-album))
  (libmpdel--album-artist album))

(cl-defmethod libmpdel-artist ((song libmpdel-song))
  (libmpdel-artist (libmpdel--song-album song)))

(cl-defgeneric libmpdel-album-name (object)
  "Return album name of OBJECT.")

(cl-defmethod libmpdel-album-name ((album libmpdel-album))
  (libmpdel--album-name album))

(cl-defmethod libmpdel-album-name ((song libmpdel-song))
  (libmpdel-album-name (libmpdel--song-album song)))

(cl-defgeneric libmpdel-album (object)
  "Return album of OBJECT.")

(cl-defmethod libmpdel-album ((album libmpdel-album))
  album)

(cl-defmethod libmpdel-album ((song libmpdel-song))
  (libmpdel--song-album song))

(cl-defgeneric libmpdel-entity-name (object)
  "Return basename of OBJECT.")

(cl-defmethod libmpdel-entity-name ((artist libmpdel-artist))
  (libmpdel--artist-name artist))

(cl-defmethod libmpdel-entity-name ((album libmpdel-album))
  (libmpdel--album-name album))

(cl-defmethod libmpdel-entity-name ((song libmpdel-song))
  (libmpdel--song-name song))

(cl-defmethod libmpdel-entity-name ((stored-playlist libmpdel-stored-playlist))
  (libmpdel--stored-playlist-name stored-playlist))

(defun libmpdel-song-file (song)
  "Return the filename of SONG."
  (libmpdel--song-file song))

(defun libmpdel-song-track (song)
  "Return the track number of SONG within its album."
  (or (libmpdel--song-track song) ""))

(defun libmpdel-song-id (song)
  "Return SONG id within the current playlist, nil if none."
  (libmpdel--song-id song))

(defun libmpdel-song-position (song)
  "Return position of SONG in playlist, nil if not in playlist."
  (let ((pos (libmpdel--song-pos song)))
    (when (and (stringp pos) (not (string= pos "")))
      (string-to-number pos))))

(defun libmpdel--create-song-from-data (song-data)
  "Return a song from SONG-DATA, a server's response."
  (libmpdel--song-create
   :name (cdr (assq 'Title song-data))
   :track (cdr (assq 'Track song-data))
   :file (cdr (assq 'file song-data))
   :album (libmpdel--album-create
           :name (cdr (assq 'Album song-data))
           :artist (libmpdel--artist-create :name (cdr (assq 'Artist song-data))))
   :id (cdr (assq 'Id song-data))
   :pos (cdr (assq 'Pos song-data))))

(defun libmpdel--create-songs-from-data (data)
  "Return a list of songs from DATA, a server's response."
  (mapcar #'libmpdel--create-song-from-data (libmpdel-group-data data)))


;;; Helper functions

(defun libmpdel--process ()
  "Return the process communicating with the MPD server."
  (tq-process libmpdel--connection))

(defun libmpdel--process-buffer ()
  "Return the buffer associated with the connection process."
  (process-buffer (libmpdel--process)))

(defun libmpdel--connect ()
  "Create a new connection with the MPD server."
  ;; The *mpd* buffer will contain all the communication logs
  (when (libmpdel-connected-p)
    (user-error "A connection is already opened"))
  (with-current-buffer (get-buffer-create "*mpd*")
    (setq-local buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (setq libmpdel--connection (tq-create (open-network-stream
                                         "mpd" "*mpd*"
                                         libmpdel-hostname
                                         libmpdel-port
                                         :type 'plain)))
  (set-process-coding-system (libmpdel--process) 'utf-8-unix 'utf-8-unix)
  ;; Take care of the initial welcome message from server that we
  ;; don't ask for:
  (setq libmpdel--msghandlers '(("welcome" libmpdel--msghandler-ignore nil)))
  (tq-queue-add libmpdel--connection nil libmpdel--response-regexp nil #'libmpdel--message-filter)
  (libmpdel-refresh-status)
  ;; As an invariant of the MPD client, there is always an "idle"
  ;; command sent to the server. This acts like a registration to the
  ;; server's notifications. See `libmpdel--msghandlers' for more
  ;; information.
  (libmpdel--raw-send-command-with-handler "idle" #'libmpdel--msghandler-idle))

(defun libmpdel--raw-send-command (command)
  "Send COMMAND, a string, to the server and log that."
  (libmpdel--log command "->")
  (tq-enqueue
   libmpdel--connection
   (format "%s\n" command)
   libmpdel--response-regexp
   nil
   #'libmpdel--message-filter))

(defun libmpdel--raw-send-command-with-handler (command &optional handler)
  "Send COMMAND to MPD server and set HANDLER for the response.
If HANDLER is nil, response will be ignored.

If command is a string, send that.  Otherwise, it must be a list
that will be passed to `format' before being sent."
  (let ((command (if (listp command)
                     (apply #'format command)
                   command)))
    (setq libmpdel--msghandlers
          (append libmpdel--msghandlers
                  `((,command
                     ,(or handler #'libmpdel--msghandler-ignore)
                     ,(current-buffer)))))
    (libmpdel--raw-send-command command)))

(defun libmpdel--message-filter (_ message)
  "Take care of the MESSAGE sent by the server.

The first parameter is ignored.  MESSAGE contains a string
representing the answer from the server."
  ;; Because errors in handlers are not raised by Emacs, we log them.
  (condition-case-unless-debug error
      (progn
        ;; because answers arrive in the same order we sent the
        ;; commands, we are sure that the first handler is the one to
        ;; use.
        (cl-destructuring-bind (command handler buffer) (pop libmpdel--msghandlers)
          (libmpdel--log (format "\"%s\" (as answer to \"%s\")" message command)
                         "<-")
          ;; if answer is a ACK, then there was a problem. We log it as such.
          (if (string= (substring message 0 3) "ACK")
              (libmpdel--log "ACK message" "ko")
            (with-current-buffer (if (buffer-live-p buffer) buffer (current-buffer))
              (funcall handler (libmpdel--extract-data message))))))
    (error (libmpdel--log error "ko"))))

(defun libmpdel--log (string type-string)
  "Add STRING at end of *mpd* buffer.

TYPE-STRING is a two-letter string classifying the kind of
message to log."
  (with-current-buffer (libmpdel--process-buffer)
    (let ((inhibit-read-only t)
          (moving (= (point) (process-mark (libmpdel--process)))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark (libmpdel--process)))
        (insert "-------------------------\n")
        (insert (format "%s [%s] %s\n" type-string (time-stamp-string) string))
        (set-marker (process-mark (libmpdel--process)) (point)))
      (if moving (goto-char (process-mark (libmpdel--process)))))))

(defun libmpdel--msghandler-idle (data)
  "Handler for the response DATA to the \"idle\" command.
This handler is responsible for sending another \"idle\"
command."
  ;; Because "idle" only informs about what changed (e.g., "the
  ;; playback state changed") without telling the new state (e.g.,
  ;; "the player is now stopped"), we have to ask for the details:
  (when data
    (libmpdel-refresh-status))
  ;; Each time an "idle" is finished, we start a new one:
  (libmpdel--raw-send-command-with-handler "idle" #'libmpdel--msghandler-idle)
  (mapc (lambda (changed-subsystem)
          (cl-case (intern (cdr changed-subsystem))
            (playlist (run-hooks 'libmpdel-current-playlist-changed-hook))
            (stored_playlist (run-hooks 'libmpdel-stored-playlist-changed-hook))))
        data))

(defun libmpdel--msghandler-status (data)
  "Handler for the response DATA to the \"status\" command."
  (mapc (lambda (status-pair)
          (let ((status-key (car status-pair))
                (status-value (cdr status-pair)))
            (cl-case status-key
              (state (libmpdel--set-play-state status-value))
              (songid (libmpdel--set-current-song status-value))
              (playlistlength (libmpdel--set-playlist-length status-value))
              (volume (libmpdel--set-volume status-value)))))
        data))

(defun libmpdel--msghandler-ignore (_)
  "No handler was associated to last response."
  ;; nothing to do
  nil)

(defconst libmpdel--msgfield-regexp
  (rx line-start
      (group (+? (not (any ?:))))
      ": "
      (group (* not-newline))
      line-end)
  "Regexp matching a line consisting of a key and a value.")

(defun libmpdel--extract-data (message)
  "Return MESSAGE."
  (save-match-data
    (with-temp-buffer
      (insert message)
      (let ((end-of-message (point-at-bol))
            (data nil))
        (goto-char (point-min))
        (while (re-search-forward libmpdel--msgfield-regexp end-of-message t)
          (push (cons (intern (match-string 1)) (match-string 2)) data))
        (reverse data)))))

(defun libmpdel--string<-ignore-case (str1 str2)
  "Compare the contents of STR1 and STR2, ignoring case."
  (let ((comp (compare-strings str1 nil nil str2 nil nil t)))
    (or (eq comp t) (< comp 0))))

(defmacro libmpdel--define-state (name value-desc &rest set-body)
  "Generate code to set and get state for NAME.

Name is a symbol (e.g., `volume' or `play-state') naming the
state to generate code for.

VALUE-DESC is a string describing the kind of value accepted for
this state.

SET-BODY is a list of forms to put in the generated setter
function.  During executiong of SET-BODY, a variable NEW-VALUE is
bound containing the value to set."
  (declare (indent 1))
  `(progn
     (defvar ,(intern (format "libmpdel--%s" name)) nil
       ,(format "Current %s of MPD server.\n%s" name value-desc))

     (defun ,(intern (format "libmpdel--set-%s" name)) (new-value)
       ,(format "Save NEW-VALUE as current %s.\n%s" name value-desc)
       ,@set-body)

     (defun ,(intern (format "libmpdel-%s" name)) ()
       ,(format "Return current value of %s.\n%s" name value-desc)
       ,(intern (format "libmpdel--%s" name)))))

(libmpdel--define-state play-state
  "Value is `play', `pause' or `stop'."
  (let ((new-state (intern new-value))
        (old-state libmpdel--play-state))
    (unless (equal old-state new-state)
      (setq libmpdel--play-state new-state)
      (run-hooks 'libmpdel-player-changed-hook))))

(defun libmpdel-stopped-p ()
  "Return non-nil if player is stopped, nil otherwise."
  (eq 'stop (libmpdel-play-state)))

(libmpdel--define-state current-song
  "An object representing currently played song."
  (let ((old-song libmpdel--current-song))
    (when (or (not old-song) (not (equal new-value (libmpdel-song-id old-song))))
      (libmpdel-send-command
       "currentsong"
       (lambda (data)
         (setq libmpdel--current-song (libmpdel--create-song-from-data data))
         (run-hooks 'libmpdel-current-song-changed-hook))))))

(libmpdel--define-state playlist-length
  "Number of songs in current playlist."
  (setq libmpdel--playlist-length (string-to-number new-value)))

(libmpdel--define-state volume
  "Value is a string representing a number between 0 and 100."
  (setq libmpdel--volume new-value))

(defun libmpdel-time-to-string (time)
  "Return a string represeting TIME, a number in a string."
  (if (not time)
      "0"
    (let* ((time (string-to-number time))
           (seconds (mod time 60))
           (minutes (/ (- time seconds) 60)))
      (format "%02d:%02d" (truncate minutes) (truncate seconds)))))

(defun libmpdel-completing-read (prompt entities &optional transformer)
  "PROMPT user to select one entity among ENTITIES.

Transform each entity to a string with TRANSFORMER,
`libmpdel-entity-name' if nil."
  (let* ((transformer (or transformer #'libmpdel-entity-name))
         (map (make-hash-table :test 'equal :size (length entities)))
         (entity-strings (mapcar (lambda (entity) (funcall transformer entity)) entities)))
    (cl-mapcar (lambda (entity entity-string)
                 (puthash entity-string entity map))
               entities entity-strings)
    (let ((entity-string (completing-read prompt entity-strings nil t)))
      (gethash entity-string map))))


;;; Public functions

(defun libmpdel-connected-p ()
  "Return non-nil if there is a connection to MPD server."
  (and libmpdel--connection
       (process-live-p (libmpdel--process))))

(defun libmpdel-ensure-connection ()
  "Make sure there is an active connection to the MPD server."
  (unless (libmpdel-connected-p)
    (libmpdel--connect)))

(defun libmpdel-disconnect ()
  "Close connection to the MPD server."
  (when (not (libmpdel-connected-p))
    (user-error "There is no connection to MPD"))
  (tq-close libmpdel--connection)
  (setq libmpdel--connection nil))

(defun libmpdel-send-command (command &optional handler)
  "Send COMMAND to server and register HANDLER for the answer.
If HANDLER is nil, ignore response."
  (libmpdel-ensure-connection)
  ;; if current command is IDLE, we have to cancel it. See
  ;; `mpdel-msghandlers' for more information.
  (when (eql (elt (car (last libmpdel--msghandlers)) 1) #'libmpdel--msghandler-idle)
    (libmpdel--raw-send-command "noidle"))
  (libmpdel--raw-send-command-with-handler command handler))

(defun libmpdel-send-commands (commands)
  "Send several COMMANDS at once."
  (libmpdel-send-command
   (with-temp-buffer
     (insert "command_list_begin\n")
     (mapc (lambda (command) (insert command "\n")) commands)
     (insert "command_list_end")
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun libmpdel-entries (data key)
  "Collect DATA entries matching KEY."
  (mapcar #'cdr (cl-remove-if-not (apply-partially #'eq key) data :key #'car)))

(defun libmpdel-sorted-entries (data key)
  "Sort and collect DATA entries matching KEY."
  (sort (libmpdel-entries data key) #'libmpdel--string<-ignore-case))

(defun libmpdel-group-data (data)
  "Find repeating fields in DATA and group them."
  (when data
    (let ((first-key (caar data))
          result group)
      (mapc (lambda (key-value)
              (when (and
                     (eq (car key-value) first-key)
                     group)
                (push (reverse group) result)
                (setq group nil))
              (push key-value group))
            data)
      (push (reverse group) result)
      (reverse result))))

(cl-defgeneric libmpdel-dired (entity)
  "Open `dired' on ENTITY.")

(eval-when-compile
  (declare-function dired-jump "dired-x"))

(cl-defmethod libmpdel-dired ((song libmpdel-song))
  (require 'dired-x)
  (dired-jump t (expand-file-name (libmpdel-song-file song) libmpdel-music-directory)))


;;; Helper queries

(cl-defgeneric libmpdel-entity-to-criteria (object)
  "Return search criteria matching OBJECT.")

(cl-defmethod libmpdel-entity-to-criteria ((query string))
  query)

(cl-defmethod libmpdel-entity-to-criteria ((artist libmpdel-artist))
  (format "artist %S" (libmpdel-entity-name artist)))

(cl-defmethod libmpdel-entity-to-criteria ((album libmpdel-album))
  (format "%s album %S"
          (libmpdel-entity-to-criteria (libmpdel-artist album))
          (libmpdel-entity-name album)))

(cl-defmethod libmpdel-entity-to-criteria ((song libmpdel-song))
  (format "%s title %S"
          (libmpdel-entity-to-criteria (libmpdel-album song))
          (libmpdel-entity-name song)))

(cl-defgeneric libmpdel-list (object function)
  "Call FUNCTION with all entries matching OBJECT.")

(defun libmpdel-list-artists (function)
  "Call FUNCTION with all artists as parameters."
  (libmpdel-send-command
   "list artist"
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (artist-name) (libmpdel--artist-create :name artist-name))
               (libmpdel-sorted-entries data 'Artist))))))

(defun libmpdel-list-stored-playlists (function)
  "Call FUNCTION with all stored playlists as parameters."
  (libmpdel-send-command
   "listplaylists"
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (playlist-name) (libmpdel--stored-playlist-create :name playlist-name))
               (libmpdel-sorted-entries data 'playlist))))))

(cl-defmethod libmpdel-list ((artist libmpdel-artist) function)
  (libmpdel-send-command
   `("list album %s" ,(libmpdel-entity-to-criteria artist))
   (lambda (data)
     (funcall function
              (mapcar
               (lambda (album-name) (libmpdel--album-create :name album-name :artist artist))
               (libmpdel-sorted-entries data 'Album))))))

(cl-defmethod libmpdel-list ((album libmpdel-album) function)
  (libmpdel-send-command
   `("find %s" ,(libmpdel-entity-to-criteria album))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list ((stored-playlist libmpdel-stored-playlist) function)
  (libmpdel-send-command
   `("listplaylistinfo %S" ,(libmpdel-entity-name stored-playlist))
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))

(cl-defmethod libmpdel-list ((_ libmpdel-current-playlist) function)
  (libmpdel-send-command
   "playlistinfo"
   (lambda (data)
     (funcall function (libmpdel--create-songs-from-data data)))))


;;; Playlist queries

(cl-defgeneric libmpdel-playlist-add (entity playlist)
  "Add ENTITY to PLAYLIST.")

(cl-defmethod libmpdel-playlist-add (entity (_ libmpdel-current-playlist))
  (libmpdel-send-command `("findadd %s" ,(libmpdel-entity-to-criteria entity))))

(cl-defmethod libmpdel-playlist-add (entity (stored-playlist libmpdel-stored-playlist))
  (libmpdel-send-command
   `("searchaddpl %S %s"
     ,(libmpdel-entity-name stored-playlist)
     ,(libmpdel-entity-to-criteria entity))))

(cl-defmethod libmpdel-playlist-add ((stored-playlist libmpdel-stored-playlist) (_ libmpdel-current-playlist))
  (libmpdel-send-command `("load %S" ,(libmpdel-entity-name stored-playlist))))

(defun libmpdel-playlist-replace (entity)
  "Clear current playlist and add ENTITY to it."
  (libmpdel-playlist-clear)
  (libmpdel-playlist-add entity (libmpdel-current-playlist)))

(defun libmpdel-playlist-clear ()
  "Remove all songs from current playlist."
  (libmpdel-send-command "clear"))

(cl-defgeneric libmpdel-playlist-delete (songs playlist)
  "Remove SONGS from PLAYLIST.")

(cl-defmethod libmpdel-playlist-delete (songs (_ libmpdel-current-playlist))
  (libmpdel-send-commands
   (mapcar (lambda (song) (format "deleteid %s" (libmpdel-song-id song)))
           songs)))

(cl-defmethod libmpdel-playlist-delete (songs (stored-playlist libmpdel-stored-playlist))
  (libmpdel-list
   stored-playlist
   (lambda (all-playlist-songs)
     (let ((song-positions (cl-sort (mapcar (lambda (song)
                                              (cl-position song all-playlist-songs :test #'equal))
                                            songs)
                                    #'>)))
       (libmpdel-send-commands
        (mapcar
         (lambda (song-position)
           (format "playlistdelete %S %s"
                   (libmpdel-entity-name stored-playlist)
                   song-position))
         song-positions))))))

(defun libmpdel-playlist-move-up (songs)
  "Move up SONGS in current playlist."
  ;; We should move up from first in playlist to last
  (let* ((songs (cl-sort (cl-copy-seq songs) #'< :key #'libmpdel-song-position)))
    ;; Don't move up if first song is selected
    (unless (= (libmpdel-song-position (car songs)) 0)
      (libmpdel-send-commands
       (mapcar (lambda (song)
                 (format "moveid %s %s" (libmpdel-song-id song) (1- (libmpdel-song-position song))))
               songs)))))

(defun libmpdel-playlist-move-down (songs)
  "Move down SONGS in current playlist."
  ;; We should move down from last in playlist to first
  (let* ((songs (cl-sort (cl-copy-seq songs) #'> :key #'libmpdel-song-position)))
    ;; Don't move down if last song is selected
    (unless (= (libmpdel-song-position (car songs)) (1- libmpdel--playlist-length))
      (libmpdel-send-commands
       (mapcar (lambda (song)
                 (format "moveid %s %s" (libmpdel-song-id song) (1+ (libmpdel-song-position song))))
               songs)))))


;;; Playback queries

;;;###autoload
(defun libmpdel-playback-set-volume (volume)
  "Set volume to VOLUME."
  (interactive (list
                (read-string (format "Current volume is %s. New volume [0-100]: "
                                     (libmpdel-volume)))))
  (libmpdel-send-command `("setvol %s" ,volume)))

;;;###autoload
(defun libmpdel-playback-next ()
  "Play next song in the playlist."
  (interactive)
  (libmpdel-send-command "next"))

;;;###autoload
(defun libmpdel-playback-previous ()
  "Play previous song in the playlist."
  (interactive)
  (libmpdel-send-command "previous"))

;;;###autoload
(defun libmpdel-play ()
  "Start playing."
  (interactive)
  (libmpdel-send-command "play"))

;;;###autoload
(defun libmpdel-stop ()
  "Stop playing.  See also `libmpdel-playback-play-pause'."
  (interactive)
  (libmpdel-send-command "stop"))

(defun libmpdel-play-song (song)
  "Start playing SONG, a song of the current playlist."
  (libmpdel-send-command `("playid %s" ,(libmpdel-song-id song))))

;;;###autoload
(defun libmpdel-playback-play-pause ()
  "Toggle between play and pause.
See also `libmpdel-playback-stop'."
  (interactive)
  (libmpdel-send-command
   (cl-case libmpdel--play-state
     (play "pause 1")
     (pause "pause 0")
     (stop "play"))))

;;;###autoload
(defun libmpdel-playback-seek (time &optional handler)
  "Seeks to the position TIME within the current song.

TIME is a string indicating a number of seconds, fractions
allowed.  If prefixed by + or -, then the time is relative to
the current playing position.

If HANDLER is non-nil, execute it with no parameter when seek
succeeds."
  (interactive (list (read-string "New position (e.g., 67, -23, +12): ")
                     (lambda (_) (message "Seek done."))))
  (libmpdel-send-command `("seekcur %S" ,time) (lambda (_) (funcall handler))))


;;; Status queries

(defun libmpdel-refresh-status ()
  "Ask the server for its current status."
  (libmpdel-send-command "status" #'libmpdel--msghandler-status))


;;; Database queries

;;;###autoload
(defun libmpdel-database-update (&optional uri)
  "Update the music database for URI, everything if nil.
Updates the music database: find new files, remove deleted files,
update modified files.

URI is a particular directory or song/file to update.  If you do
not specify it, everything is updated."
  (interactive "i")
  (libmpdel-send-command
   (if uri `("update %S" ,uri) "update")))

(provide 'libmpdel)
;;; libmpdel.el ends here
