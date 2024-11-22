;;; my-writing.el --- Writing support of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (pdf-tools "1.1.0"))
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

;; This file has enhanced my general writing experience, and
;; individually enable support for different writing format.

;; Supported programming languages:
;;   - Markdown

;;; Code:

(require 'my-core)
(require 'my-writing-markdown)
(require 'my-writing-tex)
(require 'my-writing-typst)
(require 'my-writing-org)

(cl-eval-when (compile)
  (require 'elec-pair)
  (require 'hl-line)
  (require 'outline)
  (require 'pdf-loader)
  (require 'pdf-view)
  (require 'simple)
  (require 'text-mode))



;;;
;; Appearance:

(setup hl-line
  (:autoload hl-line-mode))

(setup outline
  (:with-hook outline-mode-hook
    (:hook
     hl-line-mode))) ;; Highlight current line.

(setup text-mode
  (:hook
   hl-line-mode)) ;; Highlight current line.

(setup window
  (:set
   (append display-buffer-alist)
   '( "\\*Outline\\*"
      (display-buffer-reuse-mode-window display-buffer-in-side-window)
      (side . right)
      (window-parameters . ( (mode-line-format . none)
                             (no-delete-other-window . t)
                             (no-other-window . t))))))



;;;
;; Editing:

(setup elec-pair
  (:autoload electric-pair-local-mode))

;; Automatically insert closing parentheses.

(setup outline
  (:with-hook outline-mode-hook
    (:hook electric-pair-local-mode)))

(setup text-mode
  (:hook electric-pair-local-mode))



;;;
;; Major modes:

(setup text-mode
  (:autoload text-mode)
  (:with-mode text-mode
    (:file-match
     "\\COPYING\\'"
     "\\COPYING\\.\\([a-zA-Z]+\\)"
     "\\LICENSE\\'"
     "\\LICENSE\\.\\([a-zA-Z]+\\)")))



;;;
;; PDF:

(setup pdf-loader
  (:autoload pdf-loader-install))

(setup pdf-view
  (:autoload pdf-view-mode)
  (:with-mode pdf-view-mode
    (:file-match "\\.pdf\\'"))
  ;; Ensure `pdf-tools'.
  (:advice-add pdf-view-mode :before pdf-loader-install))



;;;
;; Search:

(setup replace
  ;; Popup *Occur* buffers.
  (:snoc popper-reference-buffers
         "\\*Occur\\*"))



;;;
;; Wrap:

(setup simple
  (:autoload visual-line-mode))

;; Set soft line wrap by default.

(setup outline
  (:with-hook outline-mode-hook
    (:hook visual-line-mode)))

(setup text-mode
  (:hook visual-line-mode))



(provide 'my-writing)
;;; my-writing.el ends here
