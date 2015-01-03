[![TRAVIS-CI](https://travis-ci.org/boyw165/history.svg?branch=master)](https://travis-ci.org/boyw165/history)

history.el - History utility for source code navigation
=======================================================

This tool is similar to `pop-mark` or `pop-global-mark` but more powerful.
You can go through the whole history without losing them. More specific,
`pop-global-mark` will use the latest record but also discard it. But this
tool will preserve all the history. The tool will smartly ignored killed
buffers or invalid symbol string.

Basic Concept:
--------------
* Normal history database:

  (1) - (2) - (3) - (4) - `(5)`

* Goto previous Nth history:

  (1) - (2) - `(3)` - (4) - (5)

* Add a new history into the database:

  (1) - (2) - (3) - `(6)`
> history behind index will be discard, and new one will be appended to the
database

Usage:
------
`M-x history-mode` Add menu items and tool-bar items of history utility.

`(his-add-history)` Save current point and buffer as a history into the database.

`(his-add-history t)` Like above, but also save symbol string at point. When navigating to the history, the tool compare the matched string so that it make sure the history is VALID.

`M-x his-prev-history` Goto previous history.

`M-x his-next-history` Goto new history.

`M-x his-kill-histories` Discard whole history database.

Customization:
--------------
`his-history-max` The maximum length of the history database.

`his-ignore-buffer-names` A REGEXP list to ignore specific buffers.


