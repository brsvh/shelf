;;; my-browser.el --- Customize web browser for My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1"))
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

;; This file provides enhancements for `dired', making it a pleasure to
;; browse and manage files using Emacs.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'eww))



;;;
;; EWW:

(setup eww
  (:autoload eww)
  (:keymap-set-into ctl-c-a-map "b" eww)
  (:when-loaded
    (:set
     eww-bookmarks-directory (my-config-path* "eww/"))))



(provide 'my-browser)
;;; my-browser.el ends here
