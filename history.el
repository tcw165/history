;;; history.el --- History utility for source code navigation
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 20141204.1100
;; URL: https://github.com/boyw165/history
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This tool is similar to `pop-global-mark' but more powerful. You can go
;; through the whole history without losing them. Actually, `pop-global-mark'
;; will use the latest record but also discard it. But this tool will preserve
;; all the history and smartly ignored killed buffers or invalid symbol string.
;;
;; You'll feel the power and convenience of using `history-add-history', 
;; `history-prev-history' and `history-next-history' instead of built-in old way.
;;
;; Basic Concept:
;; --------------
;; * Normal history database:
;;   (1) - (2) - (3) - (4) - (5)
;;                            ^ index
;; * Goto previous Nth history:
;;   (1) - (2) - (3) - (4) - (5)
;;                ^ index
;; * Add a new history into the database:
;;   (1) - (2) - (3) - (6)
;;                      ^ index, histories behind index will be discard, and new
;;                        one will be appended to the end.
;;
;; Usage:
;; ------
;; * M-x `history-mode'
;;   Add menu items and tool-bar items of history utility.
;; * (`history-add-history')
;;   Save current point and buffer as a history into the database.
;; * (`history-add-history' t)
;;   Like above, but also save symbol string at point. When navigating to the
;;   history, the tool compare the matched string so that it make sure the
;;   history is VALID.
;; * M-x `history-prev-history'
;;   Goto previous history.
;; * M-x `history-next-history'
;;   Goto new history.
;; * M-x `history-kill-histories'
;;   Discard whole history database.
;;
;; Customization:
;; --------------
;; * `history-history-max'
;;   The maximum length of the history database.
;; * `history-ignore-buffer-names'
;;   A REGEXP list to ignore specific buffers.
;;
;; TODO:
;; -----
;; n/a
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-12-28
;; * Support `history-ignore-buffer-names' to ignore some buffer with specific
;;   names.
;; * Enhance visualization of `history-show-history'.
;; * Add `history-mode'.
;;
;; 2014-06-01
;; * Initial release.
;;
;;; Code:

;; GNU Library.
(require 'thingatpt)
(require 'tool-bar)
(eval-when-compile (require 'cl))

(defgroup history nil
  "A lightweight history utility."
  :group 'convenience)

(defgroup history-face nil
  "Face of history."
  :group 'history)

(defface history-prompt
  '((t (:inherit 'minibuffer-prompt)))
  "Face of prompt when calling `history-goto-history'."
  :group 'history-face)

(defface history-current-history
  '((t (:foreground "black" :background "gold1" :widget 'bold)))
  "Face of symbol for current history when calling `history-goto-history'."
  :group 'history-face)

(defface history-other-history
  '((t (:foreground "dim gray" :background "#d1f5ea")))
  "Face of symbol for other history when calling `history-goto-history'."
  :group 'history-face)

(defcustom history-history-max 64
  "The maximum lenght of history."
  :type 'integer
  :group 'history)

(defcustom history-ignore-buffer-names '("\\*.*\\*")
  "Ths REGEXP list for matched ignore buffer names."
  :type '(repeat regexp)
  :group 'history)

(defvar history-histories nil
  "The history database. see `history-add-history' for details.")
(make-variable-frame-local 'history-histories)

(defvar history-index 0
  "The index of current history in the database.")
(make-variable-frame-local 'history-index)

(defun history-same-line? (pos1 pos2)
  (let ((line-pos1 (save-excursion
                     (goto-char pos1)
                     (beginning-of-line)
                     (point)))
        (line-pos2 (save-excursion
                     (goto-char pos2)
                     (beginning-of-line)
                     (point))))
    (= line-pos1 line-pos2)))

(defun history-add-history-internal (history)
  ;; Discard old histories.
  (and history-histories (> history-index 0)
       (let ((current (nthcdr history-index history-histories)))
         (setq history-histories (cdr current))))
  ;; Add new history.
  (push history history-histories)
  (setq history-index 0)
  ;; Keep total amount of history is less than `history-history-max'.
  (and (> (length history-histories) history-history-max)
       (setcdr (nthcdr (1- history-history-max) history-histories) nil)))

(defun history-remove-invalid-history ()
  "Go through the histories and check each buffer's validness."
  (dolist (history history-histories)
    (let* ((marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker))
           (symbol (plist-get history :symbol)))
      (if (buffer-live-p buffer)
          ;; If need to compare thing at point with history.
          (when symbol
            (with-current-buffer buffer
              (save-excursion
                (goto-char pos)
                (unless (equal symbol (thing-at-point 'symbol t))
                  ;; Remove it if thing at point doesn't match history.
                  (setq history-histories (delq history history-histories))))))
        ;; Remove it if its buffer was killed.
        (setq history-histories (delq history history-histories)))))
  ;; Update index if necessary.
  (when (and history-histories
             (>= history-index (length history-histories)))
    (setq history-index (1- (length history-histories)))))

(defun history-move-history (step)
  (setq history-index (+ history-index step))
  (cond
   ((>= history-index (length history-histories))
    (setq history-index (1- (length history-histories))))
   ((< history-index 0)
    (setq history-index 0))))

(defun history-use-current-history ()
  (when (> (length history-histories) 0)
    (let* ((history (nth history-index history-histories))
           (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; Switch to buffer.
      (switch-to-buffer buffer)
      ;; Update point.
      (goto-char pos))))

(defun history-undefined ()
  "Empty command for keymap binding."
  (interactive))

(defun history-preview-prev-history ()
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (setq history-index (1+ history-index))
    (and (>= history-index (length history-histories))
         (setq history-index (1- (length history-histories))))
    (insert (history-histories-string))
    (re-search-backward "\*")
    ;; Use history and re-select minibuffer.
    (history-use-current-history)
    (select-window (active-minibuffer-window))))

(defun history-preview-next-history ()
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (setq history-index (1- history-index))
    (and (< history-index 0)
         (setq history-index 0))
    (insert (history-histories-string))
    (re-search-backward "\*")
    ;; Use history and re-select minibuffer.
    (history-use-current-history)
    (select-window (active-minibuffer-window))))

(defun history-preview-goto-history ()
  (interactive)
  (when (minibufferp)
    (setq-default history-index history-index)
    (throw 'exit t)))

(defun history-histories-string ()
  "Histories list string."
  (let* ((total (length history-histories))
         (prompt (propertize (format "History %d/%d: "
                                     (- total history-index) total)
                             'face 'history-prompt))
         value)
    (loop for i from 0 below total do
          (setq value (concat value
                              (if (= i (- total 1 history-index))
                                  (propertize "*"
                                              'face 'history-current-history)
                                (propertize "."
                                            'face 'history-other-history)))))
    (concat prompt value)))

(defun history-menu-enable? ()
  (catch 'ignore
    (dolist (ignore history-ignore-buffer-names)
      (when (string-match ignore (buffer-name))
        (throw 'ignore nil)))
    (> (length history-histories) 0)))

(defun history-add-menu-items ()
  "Add menu and tool-bar buttons."
  ;; Menu items.
  (define-key-after global-map [menu-bar edit history-group]
    (cons "History" (make-sparse-keymap))
    'separator-search)
  (define-key-after global-map [menu-bar edit history-group set-history]
    '(menu-item "Add History" history-add-history))
  (define-key-after (default-value 'global-map) [menu-bar edit history-group previous-history]
    '(menu-item "Previous History" history-prev-history
		:enable (history-menu-enable?)))
  (define-key-after (default-value 'global-map) [menu-bar edit history-group next-history]
    '(menu-item "Next History" history-next-history
		:enable (history-menu-enable?)))
  (define-key-after global-map [menu-bar edit history-group show-history]
    '(menu-item "List History" history-show-history
		:help "List history in a buffer"))
  (define-key-after global-map [menu-bar edit history-group discard-history]
    '(menu-item "Kill All History" history-kill-histories
		:enable (history-menu-enable?)))
  ;; Tool-bar buttons.
  (when tool-bar-mode
    (define-key-after tool-bar-map [separator-history]
      '("--")
      'paste)
    (define-key-after tool-bar-map [add-history]
      '(menu-item "Add History" history-add-history
                  :image (find-image '((:type xpm :file "images/add-history.xpm"))))
      'separator-history)
    (define-key-after tool-bar-map [previous-history]
      '(menu-item "Previous History" history-prev-history
                  :image (find-image '((:type xpm :file "images/prev-history.xpm")))
                  :enable (history-menu-enable?))
      'add-history)
    (define-key-after tool-bar-map [next-history]
      '(menu-item "Next History" history-next-history
                  :image (find-image '((:type xpm :file "images/next-history.xpm")))
                  :enable (history-menu-enable?))
      'previous-history)))

(defun history-remove-menu-items ()
  "Remove menu and tool-bar buttons."
  ;; Menu items.
  (define-key global-map [menu-bar edit history-group] nil)
  ;; Tool-bar buttons.
  (when tool-bar-mode
    (define-key tool-bar-map [separator-history] nil)
    (define-key tool-bar-map [add-history] nil)
    (define-key tool-bar-map [previous-history] nil)
    (define-key tool-bar-map [next-history] nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun history-add-history (&optional is-thing?)
  "Add current position into the database, which is `global-mark-ring'. If 
IS-THING? is t, it will cache the symbol string at point (if any) and use it as 
a comparison in checking algorithm when navigating to it. If they are not matched, 
the history will be deleted immediately."
  (interactive)
  (catch 'ignore
    (dolist (ignore history-ignore-buffer-names)
      (when (string-match ignore (buffer-name))
        (throw 'ignore nil)))
    (let (history
          (thing (thing-at-point 'symbol t)))
      ;; Create history.
      (setq history (plist-put history :marker (copy-marker (point) t)))
      ;; Cache the symbol string if necessary.
      (and is-thing? thing
           (setq history (plist-put history :symbol thing)))
      ;; Add to databse.
      (history-add-history-internal history))))

;;;###autoload
(defun history-show-history ()
  "Show histories in a pretty way."
  (interactive)
  (history-remove-invalid-history)
  (message (history-histories-string)))

;;;###autoload
(defun history-goto-history ()
  (interactive)
  (when (> (length history-histories) 0)
    (minibuffer-with-setup-hook
      (lambda ()
        ;; Make index a buffer local variable so that user can return to
        ;; original status.
        (setq-local history-index history-index)
        ;; Change minibuffer's local map.
        (use-local-map (let ((map (make-sparse-keymap)))
                         (define-key map [remap self-insert-command] 'history-undefined)
                         (define-key map (kbd "<up>") 'history-undefined)
                         (define-key map (kbd "<down>") 'history-undefined)
                         (define-key map (kbd "<left>") 'history-preview-prev-history)
                         (define-key map (kbd "<right>") 'history-preview-next-history)
                         (define-key map (kbd "<escape>") 'exit-minibuffer)
                         (define-key map (kbd "<return>") 'history-preview-goto-history)
                         map)))
      (let* ((str (history-histories-string))
             (index (1+ (string-match "\*" str)))
             (buffer (current-buffer))
             (pos (point)))
        (if (catch 'exit
              (read-from-minibuffer "" (cons str index))
              nil)
            ;; Use history.
            (history-use-current-history)
          ;; Not to use history.
          (switch-to-buffer buffer)
          (goto-char pos))))))

;;;###autoload
(defun history-kill-histories ()
  "Discard all the histories."
  (interactive)
  (setq history-index 0
        history-histories nil))

;;;###autoload
(defun history-prev-history ()
  "Navigate to previous history."
  (interactive)
  (when history-histories
    (history-remove-invalid-history)
    (let* ((history (nth history-index history-histories))
	   (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (when (and (eq buffer (current-buffer))
                 (history-same-line? (point) pos))
        (history-move-history 1)))
    ;; Use history.
    (history-use-current-history))
  (history-show-history))

;;;###autoload
(defun history-next-history ()
  "Navigate to next history."
  (interactive)
  (when history-histories
    (history-remove-invalid-history)
    (let* ((history (nth history-index history-histories))
	   (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (when (and (eq buffer (current-buffer))
                 (history-same-line? (point) pos))
        (history-move-history -1)))
    ;; Use history.
    (history-use-current-history))
  (history-show-history))

;;;###autoload
(define-minor-mode history-mode
  "Add menus, toolbar buttons and more."
  :lighter " history"
  :global t
  (if history-mode
      (history-add-menu-items)
    (history-remove-menu-items)))

(provide 'history)
;;; history.el ends here
