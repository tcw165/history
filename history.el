;;; history.el --- History utility for source code navigation.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 20150103.1300
;; Package-Requires: ((emacs "24.3"))
;; Compatibility: GNU Emacs 24.3+
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
;; This tool is similar to `pop-mark' or `pop-global-mark' but more powerful.
;; You can go through the whole history without losing them. More specific,
;; `pop-global-mark' will use the latest record but also discard it. But this
;; tool will preserve all the history. The tool will smartly ignored killed
;; buffers or invalid symbol string.
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
;;                      ^ index
;; (history behind index will be discard, and new one will be appended to the
;;  database)
;;
;; Usage:
;; ------
;; * M-x history-mode
;;   Add menu items and tool-bar items of history utility.
;; * (his-add-history)
;;   Save current point and buffer as a history into the database.
;; * (his-add-history t)
;;   Like above, but also save symbol string at point. When navigating to the
;;   history, the tool compare the matched string so that it make sure the history
;;   is VALID.
;; * M-x his-prev-history
;;   Goto previous history.
;; * M-x his-next-history
;;   Goto new history.
;; * M-x his-kill-histories
;;   Discard whole history database.
;;
;; Customization:
;; --------------
;; * `his-history-max'
;;   The maximum length of the history database.
;; * `his-ignore-buffer-names'
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
;; * Support `his-ignore-buffer-names' to ignore some buffer with specific names.
;; * Enhance visualization of `his-show-history'.
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
  "A lightweight history utility.")

(defgroup history-face nil
  "Face of history."
  :group 'history)

(defface his-prompt
  '((t (:inherit 'minibuffer-prompt)))
  "Face of prompt when calling `his-goto-history'."
  :group 'history-face)

(defface his-current-history
  '((t (:foreground "black" :background "gold1" :widget 'bold)))
  "Face of symbol for current history when calling `his-goto-history'."
  :group 'history-face)

(defface his-other-history
  '((t (:foreground "dim gray" :background "#d1f5ea")))
  "Face of symbol for other history when calling `his-goto-history'."
  :group 'history-face)

(defcustom his-history-max 64
  "The maximum lenght of history."
  :type 'integer
  :group 'history)

(defcustom his-ignore-buffer-names '("\*.*\*")
  "Ths REGEXP list for matched ignore buffer names."
  :type '(repeat regexp)
  :group 'history)

(defvar his-histories nil
  "The history database. see `his-add-history' for details.")

(defvar his-index 0
  "The index of current history in the database.")

(defun his-same-line? (pos1 pos2)
  (let ((line-pos1 (save-excursion
                     (goto-char pos1)
                     (beginning-of-line)
                     (point)))
        (line-pos2 (save-excursion
                     (goto-char pos2)
                     (beginning-of-line)
                     (point))))
    (= line-pos1 line-pos2)))

(defun his-add-history-internal (history)
  ;; Discard old histories.
  (and his-histories (> his-index 0)
       (let ((current (nthcdr his-index his-histories)))
         (setq his-histories (cdr current))))
  ;; Add new history.
  (push history his-histories)
  (setq his-index 0)
  ;; Keep total amount of history is less than `his-history-max'.
  (and (> (length his-histories) his-history-max)
       (setcdr (nthcdr (1- his-history-max) his-histories) nil)))

(defun his-remove-invalid-history ()
  "Go through the histories and check each buffer's validness."
  (dolist (history his-histories)
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
                  (setq his-histories (delq history his-histories))))))
        ;; Remove it if its buffer was killed.
        (setq his-histories (delq history his-histories)))))
  ;; Update index if necessary.
  (when (and his-histories
             (>= his-index (length his-histories)))
    (setq his-index (1- (length his-histories)))))

(defun his-move-history (step)
  (setq his-index (+ his-index step))
  (cond
   ((>= his-index (length his-histories))
    (setq his-index (1- (length his-histories))))
   ((< his-index 0)
    (setq his-index 0))))

(defun his-use-current-history ()
  (when (> (length his-histories) 0)
    (let* ((history (nth his-index his-histories))
           (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; Switch to buffer.
      (switch-to-buffer buffer)
      ;; Update point.
      (goto-char pos))))

(defun his-undefined ()
  "Empty command for keymap binding."
  (interactive))

(defun his-preview-prev-history ()
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (setq his-index (1+ his-index))
    (and (>= his-index (length his-histories))
         (setq his-index (1- (length his-histories))))
    (insert (his-histories-string))
    (re-search-backward "\*")
    ;; Use history and re-select minibuffer.
    (his-use-current-history)
    (select-window (active-minibuffer-window))))

(defun his-preview-next-history ()
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (setq his-index (1- his-index))
    (and (< his-index 0)
         (setq his-index 0))
    (insert (his-histories-string))
    (re-search-backward "\*")
    ;; Use history and re-select minibuffer.
    (his-use-current-history)
    (select-window (active-minibuffer-window))))

(defun his-preview-goto-history ()
  (interactive)
  (when (minibufferp)
    (setq-default his-index his-index)
    (throw 'exit t)))

(defun his-histories-string ()
  "Histories list string."
  (let* ((total (length his-histories))
         (prompt (propertize (format "History %d/%d: "
                                     (- total his-index) total)
                             'face 'his-prompt))
         value)
    (loop for i from 0 below total do
          (setq value (concat value
                              (if (= i (- total 1 his-index))
                                  (propertize "*"
                                              'face 'his-current-history)
                                (propertize "."
                                            ' face 'his-other-history)))))
    (concat prompt value)))

(defun his-menu-enable? ()
  (catch 'ignore
    (dolist (ignore his-ignore-buffer-names)
      (when (string-match ignore (buffer-name))
        (throw 'ignore nil)))
    (> (length his-histories) 0)))

(defun his-add-menu-items ()
  "Add menu and tool-bar buttons."
  ;; Menu items.
  (define-key-after global-map [menu-bar edit history-group]
    (cons "History" (make-sparse-keymap))
    'separator-search)
  (define-key-after global-map [menu-bar edit history-group set-history]
    '(menu-item "Add History" his-add-history))
  (define-key-after (default-value 'global-map) [menu-bar edit history-group previous-history]
    '(menu-item "Previous History" his-prev-history
		:enable (his-menu-enable?)))
  (define-key-after (default-value 'global-map) [menu-bar edit history-group next-history]
    '(menu-item "Next History" his-next-history
		:enable (his-menu-enable?)))
  (define-key-after global-map [menu-bar edit history-group show-history]
    '(menu-item "List History" his-show-history
		:help "List history in a buffer"))
  (define-key-after global-map [menu-bar edit history-group discard-history]
    '(menu-item "Kill All History" his-kill-histories
		:enable (his-menu-enable?)))
  ;; Tool-bar buttons.
  (when tool-bar-mode
    (define-key-after tool-bar-map [separator-history]
      '("--")
      'paste)
    (define-key-after tool-bar-map [add-history]
      '(menu-item "Add History" his-add-history
                  :image (find-image '((:type xpm :file "images/add-history.xpm"))))
      'separator-history)
    (define-key-after tool-bar-map [previous-history]
      '(menu-item "Previous History" his-prev-history
                  :image (find-image '((:type xpm :file "images/prev-history.xpm")))
                  :enable (his-menu-enable?))
      'add-history)
    (define-key-after tool-bar-map [next-history]
      '(menu-item "Next History" his-next-history
                  :image (find-image '((:type xpm :file "images/next-history.xpm")))
                  :enable (his-menu-enable?))
      'previous-history)))

(defun remove-menu-items ()
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
(defun his-add-history (&optional is-thing?)
  "Add current position into the database, which is `global-mark-ring'. If 
IS-THING? is t, it will cache the symbol string at point (if any) and use it as 
a comparison in checking algorithm when navigating to it. If they are not matched, 
the history will be deleted immediately."
  (interactive)
  (catch 'ignore
    (dolist (ignore his-ignore-buffer-names)
      (when (string-match ignore (buffer-name))
        (throw 'ignore nil)))
    (let (history
          (thing (thing-at-point 'symbol t)))
      ;; Create history.
      (setq history (plist-put history :marker (copy-marker (point) t)))
      ;; Cache the symbol string if necessary.q
      (and is-thing? thing
           (setq history (plist-put history :symbol thing)))
      ;; Add to databse.
      (his-add-history-internal history))))

;;;###autoload
(defun his-show-history ()
  "Show histories in a pretty way."
  (interactive)
  (his-remove-invalid-history)
  (message (his-histories-string)))

;;;###autoload
(defun his-goto-history ()
  (interactive)
  (when (> (length his-histories) 0)
    (minibuffer-with-setup-hook
      (lambda ()
        ;; Make index a buffer local variable so that user can return to
        ;; original status.
        (setq-local his-index his-index)
        ;; Change minibuffer's local map.
        (use-local-map (let ((map (make-sparse-keymap)))
                         (define-key map [remap self-insert-command] 'his-undefined)
                         (define-key map (kbd "<up>") 'his-undefined)
                         (define-key map (kbd "<down>") 'his-undefined)
                         (define-key map (kbd "<left>") 'his-preview-prev-history)
                         (define-key map (kbd "<right>") 'his-preview-next-history)
                         (define-key map (kbd "<escape>") 'exit-minibuffer)
                         (define-key map (kbd "<return>") 'his-preview-goto-history)
                         map)))
      (let* ((str (his-histories-string))
             (index (1+ (string-match "\*" str)))
             (buffer (current-buffer))
             (pos (point)))
        (if (catch 'exit
              (read-from-minibuffer "" (cons str index))
              nil)
            ;; Use history.
            (his-use-current-history)
          ;; Not to use history.
          (switch-to-buffer buffer)
          (goto-char pos))))))

;;;###autoload
(defun his-kill-histories ()
  "Discard all the histories."
  (interactive)
  (setq his-index 0
        his-histories nil))

;;;###autoload
(defun his-prev-history ()
  "Navigate to previous history."
  (interactive)
  (when his-histories
    (his-remove-invalid-history)
    (let* ((history (nth his-index his-histories))
	   (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (when (and (eq buffer (current-buffer))
                 (his-same-line? (point) pos))
        (his-move-history 1)))
    ;; Use history.
    (his-use-current-history))
  (his-show-history))

;;;###autoload
(defun his-next-history ()
  "Navigate to next history."
  (interactive)
  (when his-histories
    (his-remove-invalid-history)
    (let* ((history (nth his-index his-histories))
	   (marker (plist-get history :marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (when (and (eq buffer (current-buffer))
                 (his-same-line? (point) pos))
        (his-move-history -1)))
    ;; Use history.
    (his-use-current-history))
  (his-show-history))

;;;###autoload
(define-minor-mode history-mode
  "Add menus, toolbar buttons and more."
  :lighter " history"
  :global t
  (if history-mode
      (his-add-menu-items)
    (remove-menu-items)))

(provide 'history)
;;; history.el ends here
