;;; my-programming-docker.el --- Editing with docker -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (git-modes "1.4.4") (my-core "0.2.0"))
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

;; This file has enhanced my docker operating and programming
;; experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'dockerfile-ts-mode)
  (require 'gitignore-mode))



;;;
;; Major modes:

(setup dockerfile-ts-mode
  (:with-mode dockerfile-ts-mode
    (:file-match
     "Dockerfile\\'"
     "Dockerfile\\.\\([a-zA-Z]+\\)")))

(setup gitignore-mode
  (:with-mode gitignore-mode
    (:file-match
     "\\.dockerignore\\'")))



(provide 'my-programming-docker)
;;; my-programming-docker.el ends here
