;;; my-project.el --- Project management of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((diff-hl "1.10.0") (emacs "29.1") (envrc "0.12") (git-cliff "0.8.0") (magit "4.1.0") (org-project-capture "3.1.1") (project "0.11.1") (tabspaces "1.5"))
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

;; This file dictates management of my projects, including how to
;; organize the files and content within the project, how to handle
;; version control.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'diff-hl)
  (require 'diff-hl-flydiff)
  (require 'diff-hl-margin)
  (require 'envrc)
  (require 'files)
  (require 'git-cliff)
  (require 'magit)
  (require 'org-project-capture)
  (require 'project)
  (require 'tab-bar)
  (require 'tabspaces)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'whitespace))



;;;
;; Core:

(setup project
  (:when-loaded
    (:set project-list-file (my-state-path "projects.el"))))

(setup tabspaces
  (:autoload
   tabspaces-clear-buffers
   tabspaces-close-workspace
   tabspaces-kill-buffers-close-workspace
   tabspaces-mode
   tabspaces-open-or-create-project-and-workspace
   tabspaces-remove-current-buffer
   tabspaces-remove-selected-buffer
   tabspaces-switch-buffer-and-tab
   tabspaces-switch-or-create-workspace
   tabspaces-switch-to-buffer)
  (:with-map ctl-c-p-tab-map
    (:keymap-set
     "C" #'tabspaces-clear-buffers
     "R" #'tabspaces-remove-selected-buffer
     "S" #'tabspaces-switch-buffer-and-tab
     "b" #'tabspaces-switch-or-create-workspace
     "d" #'tabspaces-close-workspace
     "k" #'tabspaces-kill-buffers-close-workspace
     "o" #'tabspaces-open-or-create-project-and-workspace
     "r" #'tabspaces-remove-current-buffer
     "s" #'tabspaces-switch-to-buffer))
  (:set
   ;; Share *scratch* and *Messages* buffers.
   tabspaces-include-buffers '("*scratch*" "*Messages*")

   ;; Inhibit create the project-todo.org
   tabspaces-initialize-project-with-todo nil

   ;; Inhibit bind default key-bindings of `tabspaces'.
   tabspaces-keymap-prefix nil

   ;; Let `tabspaces' save opened sessions.
   tabspaces-session t
   tabspaces-session-file (my-state-path* "tabspaces/sessons.eld")

   ;; But don't auto restore.
   tabspaces-session-auto-restore nil

   ;; I prefer to use `consult-buffer'.
   tabspaces-use-filtered-buffers-as-default nil)
  (:with-hook tab-bar-mode-hook
    (:hook #'tabspaces-mode)))



;;;
;; Capture:

(setup org-project-capture
  (:autoload
   org-project-capture-per-project
   org-project-capture-project-todo-completing-read)
  (:keymap-set-into
   ctl-c-p-map "c" #'org-project-capture-project-todo-completing-read)
  (:when-loaded
    (:set
     ;; Use `project' as default backend.
     org-project-capture-default-backend
     (make-instance 'org-project-capture-project-backend))
    ;; Capture tasks across individual projects.
    (org-project-capture-per-project)))



;;;
;; Environment:

(setup envrc
  (:autoload envrc-global-mode)
  (:first-ui envrc-global-mode)
  (:when-loaded
    (:with-map ctl-c-p-map
      (:keymap-set
       "e" envrc-command-map)))
  (:snoc popper-reference-buffers
         "\\*envrc\\*"))



;;;
;; VCS:

(setup vc-dir
  ;; Popup all `vc-dir-mode' buffers.
  (:snoc
   popper-reference-buffers
   'vc-dir-mode))

;;;
;; VCS w/ Git:

(setup vc-git
  ;; Popup all `vc-dir-git-mode' buffers.
  (:snoc popper-reference-buffers
         'vc-dir-git-mode))

(setup diff-hl-flydiff
  (:autoload diff-hl-flydiff-mode))

(setup diff-hl-margin
  (:autoload diff-hl-margin-mode))

(setup diff-hl
  (:autoload
   diff-hl-magit-pre-refresh
   diff-hl-magit-post-refresh
   diff-hl-mode)
  (:with-hook find-file-hook
    (:hook #'diff-hl-mode))
  (:with-hook diff-hl-mode-hook
    (:when-gui
     (:hook #'diff-hl-flydiff-mode))   ;; Highlight diff on-the-fly.
    (:when-tui
     (:hook #'diff-hl-margin-mode)))) ;; Highlight diff on the margin.

(setup magit
  (:set-default magit-define-global-key-bindings nil)
  (:with-map ctl-c-v-g-map
    (:keymap-set
     "d" #'magit-dispatch
     "s" #'magit-status))
  (:when-loaded
    (:with-hook magit-pre-refresh-hook
      (:hook #'diff-hl-magit-pre-refresh))
    (:with-hook magit-post-refresh-hook
      (:hook #'diff-hl-magit-post-refresh))))

(setup whitespace
  (:autoload whitespace-mode))

(setup magit-diff
  (:autoload magit-diff-mode)
  (:with-hook magit-diff-mode-hook
    (:hook
     ;; Show visual spaces.
     #'whitespace-mode)))

(setup magit-tag
  (:when-loaded (:also-load git-cliff)))



(provide 'my-project)
;;; my-project.el ends here
