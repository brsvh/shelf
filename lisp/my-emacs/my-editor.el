;;; my-editor.el --- Editing enhancements of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((anzu "0.64") (consult "1.8") (editorconfig "0.11.0") (emacs "30.1") (form-feed "0.2.3") (mwim "0.4") (my-core "0.2.0") (rg "2.3.0") (yasnippet "0.14.1"))
;; URL: https://github.com/brsvh/shelf
;; Version: 0.2.0

;; This file is part of my-emacs.

;; my-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; my-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides enhanced settings for using Emacs as an editor,
;; with the core objectives being simplicity, speed, power, and
;; modernity.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'anzu)
  (require 'autorevert)
  (require 'consult)
  (require 'copyright)
  (require 'editorconfig)
  (require 'form-feed)
  (require 'mwim)
  (require 'my-prelude)
  (require 'recentf)
  (require 'rg)
  (require 'rg-ibuffer)
  (require 'saveplace)
  (require 'simple)
  (require 'subword)
  (require 'tabify)
  (require 'yasnippet))



(defun my/current-buffer-delete-trailing-whitespace (&rest _)
  "Delete trailing whitespaces in current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (delete-trailing-whitespace (point-min) (point-max)))
  (message "Trailing whitespaces in current buffer are deleted :)"))

(defun my/current-buffer-untabify (&rest _)
  "Convert tab to multiple spaces in current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (untabify (point-min) (point-max)))
  (message "Tabs in current buffer have convert to spaces :)"))

(defun my-editor-guess-file-mode ()
  "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened.  e.g. A
shebang line or file path may exist now."
  (when (eq major-mode 'fundamental-mode)
    (let ((buffer (or (buffer-base-buffer) (current-buffer))))
      (and (buffer-file-name buffer)
           (eq buffer (window-buffer (selected-window)))
           (set-auto-mode)
           (not (eq major-mode 'fundamental-mode))))))



;;;
;; Appearance:

(setup simple
  (:first-buffer
   ;; Display the current column number in Mode Line.
   column-number-mode
   ;; Display the current line number in Mode Line.
   line-number-mode))

(setup form-feed
  (:autoload form-feed-mode global-form-feed-mode)
  (:first-buffer
   ;; Display ^L glyphs as horizontal lines.
   global-form-feed-mode))



;;;
;; Bookmark:

(setup consult
  (:autoload consult-bookmark))

(setup bookmark
  (:with-map global-map
    (:keymap-set "<remap> <bookmark-bmenu-list>" #'consult-bookmark))
  (:when-loaded
    (:set
     bookmark-default-file (my-data-path "bookmarks.el"))))



;;;
;; Copyright:

(setup copyright
  ;; TODO should I enable automatic update of copyright when saving?
  (:set
   copyright-names-regexp
   (format "%s <%s>" user-full-name user-mail-address)))



;;;
;; Editing:

(setup editing
  (:with-map ctl-c-e-map
    (:keymap-set
     "SPC" #'my/current-buffer-delete-trailing-whitespace
     "TAB" #'my/current-buffer-untabify)))



;;;
;; Editorconfig:

(setup editorconfig
  (:first-file editorconfig-mode))



;;;
;; Files:

(setup autorevert
  (:first-file global-auto-revert-mode)
  (:when-loaded
    (:set auto-revert-verbose nil)))

(setup files
  (:with-hook after-save-hook
    (:hook #'my-editor-guess-file-mode))
  (:set
   ;; Move to trash when delete files.
   delete-by-moving-to-trash t

   ;; Enable auto-saving by default.
   auto-save-default t

   ;; Make auto-saving silent.
   auto-save-no-message t

   ;; Allow auto-saving whether deletions is big.
   auto-save-include-big-deletions t

   ;; Change the default location of auto-saved files.
   (prepend auto-save-file-name-transforms)
   (list ".*" (my-data-path* "auto-save/") t)

   ;; Change the default list file for store auto-saved files.
   auto-save-list-file-prefix
   (concat (my-data-path* "auto-save/" "lists/") "saves")

   ;; Make backups of file in version-control style.
   make-backup-files t
   version-control t

   ;; Create backup of file by copying way.
   backup-by-copying t

   ;; How many backups will be saved? Maybe 5 is enough for me.
   delete-old-versions t
   kept-old-versions 5
   kept-new-versions 5

   ;; Change the default location of backup files.
   (prepend backup-directory-alist)
   (cons "." (my-data-path* "backup/"))))



;;;
;; Goto:

(setup mwim
  (:with-map global-map
    (:keymap-set
     "<remap> <move-beginning-of-line>" #'mwim-beginning-of-code-or-line
     "<remap> <move-end-of-line>" #'mwim-end-of-code-or-line)))

(setup subword
  (:autoload global-subword-mode subword-mode)
  (:first-buffer global-subword-mode))



;;;
;; Place:

(setup saveplace
  (:first-file save-place-mode)
  (:when-loaded
    (:set
     ;; Change the storage location for persisting the editing place.
     save-place-file (my-state-path "place.el"))))



;;;
;; Rencent files:

(setup recentf
  (:first-file recentf-mode)
  (:advice-add
   ;; Silent load.
   recentf-load-list :around #'my-silent-message
   ;; Silent cleanup.
   recentf-cleanup :around #'my-silent-message)
  (:set
   ;; Change the storage location for persisting the recent files.
   recentf-save-file (my-state-path "recent.el"))
  (:keymap-set-into ctl-c-f-map "r" #'consult-recent-file))



;;;
;; Search:

(setup anzu
  (:first-buffer global-anzu-mode))

(setup rg-ibuffer
  (:autoload rg-list-searches))

(setup rg
  (:autoload
   rg
   rg-kill-saved-searches
   rg-literal
   rg-project
   rg-save-search
   rg-save-search-as-name
   rg-dwim)
  (:with-map ctl-c-s-map
    (:keymap-set
     "S" #'rg-save-search-as-name
     "d" #'rg-dwim
     "k" #'rg-kill-saved-searches
     "l" #'rg-list-searches
     "p" #'rg-project
     "s" #'rg-save-search
     "t" #'rg-literal
     "r" #'rg)))



;;;
;; Snippets:

(setup yasnippet
  (:when-loaded
    (:snoc yas-snippet-dirs (my-path my-etc-directory "snippets/"))
    (:advice-add yas-load-directory :around #'my-silent-message)))



;;;
;; Yank:

(setup consult
  (:with-map global-map
    (:keymap-set
     "<remap> <yank>" #'consult-yank-from-kill-ring
     "<remap> <yank-pop>" #'consult-yank-pop)))

(setup delsel
  ;; When typing with a selected region, replace it directly.
  (:first-buffer delete-selection-mode))

(setup simple
  (:set
   ;; Inhibit duplicated contents to clipboard.
   kill-do-not-save-duplicates t
   save-interprogram-paste-before-kill t))



(provide 'my-editor)
;;; my-editor.el ends here
