;;; my-terminal.el --- Terminal emulator of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((eat "0.9.4") (emacs "30.1") (project "0.11.1") (with-editor "3.4.2"))
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

;; This file makes my `eshell' more powerful.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eat)
  (require 'em-alias)
  (require 'esh-mode)
  (require 'project))



;;;
;; General settings:

(setup with-editor
  (:autoload
   with-editor-async-shell-command
   with-editor-export-editor
   with-editor-shell-command))

(setup simple
  (:with-map global-map
    (:keymap-set
     "<remap> <async-shell-command>" with-editor-async-shell-command
     "<remap> <shell-command>" with-editor-shell-command)))



;;;
;; EAT:

(setup eat
  (:autoload eat-mode eat-project)
  (:snoc popper-reference-buffers 'eat-mode)
  (:with-map ctl-c-a-map
    (:keymap-set
     "s" eat))
  (:with-map project-prefix-map
    (:keymap-set
     "s" eat-project)))



;;;
;; EShell:

(setup eshell
  ;; Popup all `eshell-mode' buffers.
  (:snoc popper-reference-buffers 'eshell-mode)
  (:set
   eshell-aliases-file (my-path my-etc-directory "eshell/aliases")
   eshell-directory-name (my-state-path "eshell/"))
  (:with-mode eshell-mode
    (:hook with-editor-export-editor)))



(provide 'my-terminal)
;;; my-terminal.el ends here
