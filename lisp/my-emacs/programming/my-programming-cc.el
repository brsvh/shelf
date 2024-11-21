;;; my-programming-cc.el --- Editing with cc -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eglot "1.17") (emacs "30.1"))
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

;; This file has enhanced my cc operating and programming
;; experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'cc-mode)
  (require 'cmake-ts-mode)
  (require 'eglot))



;;;
;; Major modes:

(setup cmake-ts-mode
  (:autoload cmake-ts-mode)
  (:with-mode cmake-ts-mode
    (:file-match
     "CMakeLists.txt\\'"
     "\\.cmake\\'")))



;;;
;; LSP:

(setup eglot
  (:autoload eglot-ensure))

;; Activate `eglot' for all c/c++ major mode.
(setup cc-mode
  (:autoload
   c++-mode
   c-mode
   c-or-c++-mode)
  (:with-mode c-or-c++-mode
    (:hook eglot-ensure))
  (:with-mode c-mode
    (:hook eglot-ensure))
  (:with-mode c++-mode
    (:hook eglot-ensure)))



(provide 'my-programming-cc)
;;; my-programming-cc.el ends here
