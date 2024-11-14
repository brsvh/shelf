;;; my-programming-rust.el --- Editing with Rust lang -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eglot "1.17") (emacs "30.1") (my-core "0.2.0"))
;; URL: https://github.com/brsvh/my-emacs
;; Version: 0.1.50

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

;; This file has enhanced my Rust operating and programming
;; experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eglot)
  (require 'rust-ts-mode))



;;;
;; Major modes:

(setup rust-ts-mode
  (:autoload rust-ts-mode)
  (:with-mode rust-ts-mode
    (:file-match
     "\\.rs\\'")))



;;;
;; LSP:

(setup eglot
  (:autoload eglot-ensure))

(setup rust-ts-mode
  (:with-mode rust-ts-mode
    (:hook eglot-ensure)))



(provide 'my-programming-rust)
;;; my-programming-rust.el ends here
