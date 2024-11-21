;;; my-programming-javascript.el --- Programming with JavasCript -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eglot "1.17") (emacs "30.1") (flymake "1.3.7") (flymake-eslint "1.7.0"))
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

;; This file has enhanced my JavaScript operating and programming
;; experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eglot)
  (require 'flymake)
  (require 'flymake-eslint)
  (require 'js))



;;;
;; Major modes:

(setup js
  (:autoload js-mode js-ts-mode)
  (:with-mode js-ts-mode
    (:file-match
     "\\.cjs\\'"
     "\\.js[mx]?\\'"
     "\\.mjs\\'")))



;;;
;; Diagnostics:

(setup flymake-eslint
  (:autoload flymake-eslint-enable))

(setup js
  (:with-mode js-ts-mode
    (:hook flymake-eslint-enable))
  (:with-mode js-mode
    (:hook flymake-eslint-enable)))



;;;
;; LSP:

(setup eglot
  (:autoload eglot-ensure))

;; Activate `eglot' for all JavaScript mode.
(setup js
  (:with-mode js-ts-mode
    (:hook eglot-ensure))
  (:with-mode js-mode
    (:hook eglot-ensure)))



(provide 'my-programming-javascript)
;;; my-programming-javascript.el ends here
