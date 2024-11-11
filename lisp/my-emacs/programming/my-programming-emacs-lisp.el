;;; my-programming-emacs-lisp.el --- Porgramming with Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eldoc-box "1.12.1") (emacs "30.1") (flymake "1.3.7") (my-core "0.2.0") (parinfer-rust-mode "0.9.0"))
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

;; This file has enhanced my Emacs Lisp programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eldoc-box)
  (require 'parinfer-rust-mode))



;;;
;; Documentation:

(setup eldoc-box
  (:autoload eldoc-box-hover-at-point-mode eldoc-box-hover-mode))

(setup eldoc-box
  (:with-hook emacs-lisp-mode-hook
    (:hook #'eldoc-box-hover-at-point-mode)))



;;;
;; Diagnostics:

(setup flymake
  (:autoload flymake-mode)
  (:with-hook emacs-lisp-mode-hook
    (:hook #'flymake-mode)))



;;;
;; Expansion:

(setup elisp-mode
  (:snoc popper-reference-buffers
         "\\*Pp Macroexpand Output\\*")
  (:with-map emacs-lisp-mode-map
    (:keymap-set "C-c C-v" #'pp-macroexpand-last-sexp)))



;;;
;; Parens editing:

(setup parinfer-rust-mode
  (:with-hook emacs-lisp-mode-hook
    (:hook #'parinfer-rust-mode)))



(provide 'my-programming-emacs-lisp)
;;; my-programming-emacs-lisp.el ends here
