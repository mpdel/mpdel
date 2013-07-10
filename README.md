# A new MPD Client for Emacs

The idea behind this new client is that is it very easy to use and does not require the full EMMS framework.

When loaded, type ```M-x mpdel-navigator``` to show the navigator

## The Navigator

This view shows all the artists in your database. You can move between
artists with `<tab>`, `n` and `p`. You can browse a particular artist
with `RET`. This will give you a list of albums for this artist. Use
`RET` again to show a list of songs for this album. When viewing
albums or songs, press `u` to go one step up.

At any time, press `SPACE` to add the thing at point to the playlist
(that works for artists, albums and songs).

Press `o` (for `other`) to see your current playlist.

## The Current Playlist

This view shows what is currently being in the play queue.

In this view, press `C` to clear the playlist, `k` to delete the song at point from the playlist, and `RET` to play the song at point.

Press `o` (for `other`) to go back to the navigator.

## Contributions

All contributions are welcome. Ask me to get write access to the
repostitory or send me pull requests.

## Todo

- Write in-code documentation
- Implement play/pause
- Implement a search mechanism that goes beyons C-s
