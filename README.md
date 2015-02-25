[![MELPA](http://melpa.org/packages/history-badge.svg)](http://melpa.org/#/history)
[![MELPA Stable](http://stable.melpa.org/packages/history-badge.svg)](http://stable.melpa.org/#/history)
[![TRAVIS-CI](https://travis-ci.org/boyw165/history.svg?branch=master)](https://travis-ci.org/boyw165/history)

history.el - History Utility For Code Navigation
================================================

This tool is similar to `pop-global-mark` but more powerful.
You can go through the whole history without losing them. Actually, `pop-global-mark` will use the latest record but also discard it. But this tool will preserve all the history and smartly ignore killed buffers or invalid symbol string.

You'll feel the power and convenience of using `history-add-history`, `history-prev-history` and `history-next-history` instead of built-in old way.

Demo
----
![history screenshot](demo/all.gif "history demo")

Basic Concept - Linked List Liked
---------------------------------
* Normal history database:

```
  (1) - (2) - (3) - (4) - (5)
                           ^ index
```

* Goto previous Nth history:

```
  (1) - (2) - (3) - (4) - (5)
               ^ index
```

* Add a new history into the database:

```
  (1) - (2) - (3) - (new)
                      ^ index
histories behind index will be discard,
and new one will be appended to the end.
```

Usage - First Step, Enable `history-mode`
-----------------------------------------

![history screenshot](demo/window-local-history.png "history menu")
![history screenshot](demo/toolbar.png "history tool-bar")

* **`(history-add-history)`** Save current point and buffer as a history into the database.

* **`(history-add-history t)`** Like above, but also save symbol string at point. When navigating to the history, the tool compare the matched string so that it makes sure the history is VALID.

* `M-x` **`history-prev-history`** Goto previous history.

* `M-x` **`history-next-history`** Goto new history.

* `M-x` **`history-kill-histories`** Discard whole history database.

* `M-x` **`history-goto-history`** Enter history preview mode. `ENTER` to confirm to use the selected history. `ESC` to cancel preview and restore everything.

![history screenshot](demo/history-goto-history.gif "goto history interactively")

Advanced Usage
--------------
* Configure `history-advised-before-functions` and `history-advised-after-functions` **to add history automatically for you.**

Example:

```lisp
;; Add history just before `find-tag' executed.
(add-to-list 'history-advised-before-functions 'find-tag-noselect t)

;; Add history just before `find-file' executed.
(add-to-list 'history-advised-before-functions 'find-file-noselect t)
```
> Advise primitive functions might cause infinite loop, strongly suggest you NOT to advise them.

Customization
-------------
* `history-history-max` The maximum length of the history database.

* `history-ignore-buffer-names` A REGEXP list to ignore specific buffers.

* `history-window-local-history` To toggle using **window-local** or **global** history. **Window-local history means every window has its independent history database.**

![history screenshot](demo/window-local-history.png "window-local history")

* `history-advised-before-functions` and `history-advised-after-functions`, a functions list to be **advised** to call `history-add-history`.

![history screenshot](demo/history-advice.png "history hooks")

Contribution
------------
Forks and pull requests are welcome!

