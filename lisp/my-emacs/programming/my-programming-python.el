;;; my-programming-python.el --- Porgramming with Python -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eglot "1.17") (emacs "30.1") (my-core "0.2.0"))
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

;; This file has enhanced my Python programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eglot)
  (require 'python))



;;;
;; Major modes:

(setup python
  (:autoload python-ts-mode)
  (:with-mode python-ts-mode
    (:file-match
     "\\.py[iw]?\\'")))



;;;
;; LSP:

;; Activate `eglot' for all Python mode.
(setup python
  (:with-mode python-mode
    (:hook eglot-ensure))
  (:with-mode python-ts-mode
    (:hook eglot-ensure)))



(provide 'my-programming-python)
;;; my-programming-python.el ends here
