;;; libmpdee-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "libmpdee" "libmpdee.el" (0 0 0 0))
;;; Generated autoloads from libmpdee.el

(autoload 'mpd-set-connection-timeout "libmpdee" "\
Set the timeout of mpd connection object CONN to TIMEOUT.
See also `mpd-new-connection'.

\(fn CONN TIMEOUT)" t nil)

(autoload 'mpd-get-playlist "libmpdee" "\
Get all songs in the current playlist managed by the mpd server.
CONN and FOREACH are as in `mpd-get-songs'.

\(fn CONN &optional FOREACH)" t nil)

(autoload 'mpd-get-playlist-entry "libmpdee" "\
Get song data for entr(y/ies) ITEM in the current mpd playlist.
CONN, FOREACH and the return value are as in `mpd-get-songs'.
ITEM is the item position/id or a list of it. Note that ITEM as nil fetches
data for all entries in the current playlist rather than not doing anything.
Interpret ITEM as song id(s) iff USE-ID is non-nil.

\(fn CONN &optional ITEM FOREACH USE-ID)" t nil)

(autoload 'mpd-get-current-song "libmpdee" "\
Get song data for the current song in mpd.
CONN and FOREACH are as in `mpd-get-songs'. Return FOREACH if specified, and
the current song (see `mpd-get-songs' for how a song is represented) otherwise.

\(fn CONN &optional FOREACH)" t nil)

(autoload 'mpd-get-directory-songs "libmpdee" "\
Get all songs in a directory of the mpd database.
CONN, FOREACH and the return value are as in `mpd-get-songs'.
DIRECTORY is the relative directory path wrt the database root.
DIRECTORY could be a list as well, the action then corresponds to all songs
in all the directories. Note that the nil value for DIRECTORY corresponds
to the database toplevel rather than an empty list.

\(fn CONN &optional DIRECTORY FOREACH)" t nil)

(autoload 'mpd-get-directory-info "libmpdee" "\
Get directory info for DIRECTORY in the mpd database.
Use CONN as the mpd connection for the purpose and call function FOREACH,
if specified, with each information field. The arguments passed to FOREACH is a
song object, directory string or playlist string and one of the symbols 'file,
'playlist or 'directory describing the data sent. DIRECTORY could be a list as
well, the action then corresponds information for all the directories. Note that
a nil for DIRECTORY corresponds to the database toplevel rather than an empty
list. Return FOREACH, if non-nil; else a vector of three elements: a list of
songs in the directory (see `mpd-get-songs' for how a song is represented),
a list of playlists and a list of subdirectories.

\(fn CONN &optional DIRECTORY FOREACH)" t nil)

(autoload 'mpd-list-directory-recursive "libmpdee" "\
Get the file-directory hierarchy of a directory in the mpd database.
Use CONN for the connection and use function FOREACH to report each entry,
along with a non-nil second argument if the entry is a directory. DIRECTORY
could be a list as well, the action then corresponds to listing of all the
directories. Note that the nil value for DIRECTORY corresponds to the
database toplevel rather than an empty list.

\(fn CONN FOREACH &optional DIRECTORY)" t nil)

(autoload 'mpd-search "libmpdee" "\
Search for songs in the mpd database.
CONN, FOREACH and the return values are as in `mpd-get-songs'.
The valid values for BY are 'artist, 'album and 'title;
indicating the field to search for; and FOR is the search string.
If FOR is a non-empty list, search by BY for all FOR.

\(fn CONN BY FOR &optional FOREACH)" t nil)

(autoload 'mpd-get-artists "libmpdee" "\
Get the names of all artists whose songs are in the mpd database.
Use CONN for the connection, and call function FOREACH, if specified, with
each name. If FOREACH is a function, return FOREACH, else return a list of
artist names.

\(fn CONN &optional FOREACH)" t nil)

(autoload 'mpd-get-artist-albums "libmpdee" "\
Get all albums in the mpd database featuring artist(s) ARTIST.
Get all albums if ARTIST is not specified. If ARTIST is a list, find the albums
of all the artists in the list. Use CONN for the connection, and call function
FOREACH, if specified, with each name. If FOREACH is a function, return FOREACH,
else return the list of albums.

\(fn CONN &optional ARTIST FOREACH)" t nil)

(autoload 'mpd-get-handled-remote-url-prefixes "libmpdee" "\
Use CONN to query remote URL prefixes handled by the mpd server.
Call function FOREACH, if specified, with each prefix.
If FOREACH is a function, return FOREACH, else return the URL prefixes.

\(fn CONN &optional FOREACH)" t nil)

(autoload 'mpd-get-outputs "libmpdee" "\
Get output descriptions from the mpd server using connection CONN.
Call function FOREACH, if specified, for each output, with the output provided
as the argument. Return list of all outputs if FOREACH is not specified and
FOREACH otherwise. When a list is returned, each element of the list is a
property list, some known keys being `outputid' (integer), `outputname' (string)
and `outputenabled' (boolean).

\(fn CONN &optional FOREACH)" t nil)

(autoload 'mpd-enqueue "libmpdee" "\
Enqueue resource RES (or list of RES) to the mpd playlist.
Each of RES can be a file or a directory in the mpd database, or an URL.

\(fn CONN RES)" t nil)

(autoload 'mpd-delete "libmpdee" "\
Delete song at position/id POS from the mpd playlist.
Interpret POS as a list of song id's if USE-ID is non-nil. POS could be a list
to delete as well. If POS is a list and USE-ID is nil, sort it in descending
order and remove duplicates before proceeding, unless NEVER-SORT is non-nil.
Note that this is necessary for the correctness of the deletion, and NEVER-SORT
is only provided in case the arguments already satisfy the condition.

\(fn CONN POS &optional USE-ID NEVER-SORT)" t nil)

(autoload 'mpd-save-playlist "libmpdee" "\
Save current mpd playlist to FILE (or list of FILE).

\(fn CONN FILE)" t nil)

(autoload 'mpd-load-playlist "libmpdee" "\
Load playlist PLNAME (or list of PLNAME) to the mpd server.

\(fn CONN PLNAME)" t nil)

(autoload 'mpd-remove-playlist "libmpdee" "\
Remove playlist PLNAME from the mpd playlist directory.
PLNAME could as well be a list of playlist names.

\(fn CONN PLNAME)" t nil)

(autoload 'mpd-shuffle-playlist "libmpdee" "\
Shuffle current mpd playlist using connection CONN.

\(fn CONN)" t nil)

(autoload 'mpd-clear-playlist "libmpdee" "\
Clear current mpd playlist using connection CONN.

\(fn CONN)" t nil)

(autoload 'mpd-play "libmpdee" "\
Play song at position/id POS (default first) in the mpd playlist.
Interpret POS as a song id iff USE-ID is non-nil.

\(fn CONN &optional POS USE-ID)" t nil)

(autoload 'mpd-stop "libmpdee" "\
Stop playing the current mpd playlist.

\(fn CONN)" t nil)

(autoload 'mpd-pause "libmpdee" "\
Toggle the pause state of the current mpd playlist.
If prefix argument ARG is non-nil, pause iff ARG is positive
and resume playing otherwise.

\(fn CONN &optional ARG)" t nil)

(autoload 'mpd-next "libmpdee" "\
Play next song in the current mpd playlist.

\(fn CONN)" t nil)

(autoload 'mpd-prev "libmpdee" "\
Play previous song in the current mpd playlist.

\(fn CONN)" t nil)

(autoload 'mpd-move "libmpdee" "\
Move item from position/id FROM in the current mpd playlist to TO.
For lists of FROM and TO, do action in that order for each pair of items.
Interpret FROM as a list of song ids iff USE-ID is non-nil. Retain TO as a list
of positions irrespective of the value of USE-ID. If sending a list for FROM and
TO, note that every move changes the order of items in the playlist.

\(fn CONN FROM TO &optional USE-ID)" t nil)

(autoload 'mpd-swap "libmpdee" "\
Swap positions/ids FIRST and SECOND in the current mpd playlist.
For lists of FROM and TO, do action in that order for each pair of items.
Interpret FIRST and SECOND as song ids iff USE-ID is non-nil.
See also `mpd-move'.

\(fn CONN FIRST SECOND &optional USE-ID)" t nil)

(autoload 'mpd-seek "libmpdee" "\
Seek to song position/id SONG and time TIME in the mpd playlist.
Take TIME to be 0 by default. Interpret SONG as a song id
iff USE-ID is non-nil.

\(fn CONN SONG &optional TIME USE-ID)" t nil)

(autoload 'mpd-toggle-random "libmpdee" "\
Change random mode of mpd using connection CONN.
With ARG, set random on iff ARG is positive.

\(fn CONN &optional ARG)" t nil)

(autoload 'mpd-toggle-repeat "libmpdee" "\
Change repeat mode of mpd using connection CONN.
With ARG, set repeat on iff ARG is positive.

\(fn CONN &optional ARG)" t nil)

(autoload 'mpd-toggle-single "libmpdee" "\
Change single mode of mpd using CONN.
With ARG, set single on iff ARG is positive.

\(fn CONN &optional ARG)" t nil)

(autoload 'mpd-set-volume "libmpdee" "\
Set the volume for the mpd player to volume VOL.

\(fn CONN VOL)" t nil)

(autoload 'mpd-adjust-volume "libmpdee" "\
Adjust the volume for the mpd player by volume VOL.
If VOL is positive, increase the volume, and decrease otherwise.

\(fn CONN VOL)" t nil)

(autoload 'mpd-set-crossfade "libmpdee" "\
Set cross-fading time for the mpd player to TIME in seconds.
Turn off cross-fading if TIME is 0.

\(fn CONN TIME)" t nil)

(autoload 'mpd-set-password "libmpdee" "\
Set the password for access to the mpd server.
*WARNING* The password is sent to the server in plaintext. The processing done
by libmpdee to send the command for setting the password also has its data as
plaintext. When called interactively, offer to store and set the password on
reconnection. Note that this is not done when the call is not interactive.
Use hooked automatic mode (see `mpd-set-automatic-mode') to achieve the same.

\(fn CONN PASS)" t nil)

(autoload 'mpd-update "libmpdee" "\
Instruct the mpd server using CONN to update its database.
PATH is the path or a list of paths to be updated. Note that PATH as nil updates
the root directory rather than not updating at all. Ignore connection timeout,
if IGNORE-TIMEOUT is non-nil and the connection is not in command list mode.
Return update job id on success.

\(fn CONN &optional PATH IGNORE-TIMEOUT)" t nil)

(autoload 'mpd-output-enable "libmpdee" "\
Use connection CONN to enable mpd output ID.

\(fn CONN ID)" t nil)

(autoload 'mpd-output-disable "libmpdee" "\
Use connection CONN to disable mpd output ID.

\(fn CONN ID)" t nil)

(autoload 'mpd-libmpdee-submit-bug-report "libmpdee" "\
Interactively submit a bug report about `libmpdee'." t nil)

(register-definition-prefixes "libmpdee" '("libmpdee-version" "mpd-" "widget-mpd-format-handler" "with-mpd-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; libmpdee-autoloads.el ends here
