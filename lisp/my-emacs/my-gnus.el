;;; my-gnus.el --- `gnus' enhancements of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/brsvh/my-emacs
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

;; This file provides support for sending and receiving my mails.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'gnus)
  (require 'gnus-start))



;;;
;; Core:

(setup gnus
  (:autoload gnus)
  (:keymap-set-into ctl-c-a-map "g" gnus))

(setup gnus-start
  (:when-loaded
    (:set
     gnus-default-directory (my-data-path* "gnus/")
     gnus-directory (my-data-path* "gnus/")
     gnus-dribble-directory (my-data-path "gnus/" "dribble/")
     gnus-home-directory (my-config-path* "gnus/")
     gnus-init-file (my-config-path* "gnus/" "init.el")
     gnus-startup-file (my-config-path* "gnus/" "newsrc"))))



;;;
;; News:

(setup gnus
  (:when-loaded
    (:set
     gnus-select-method '( nntp "news.gmane.io")
     (append gnus-secondary-select-methods) '( nntp "news.newsfan.net"))))



(provide 'my-gnus)
;;; my-gnus.el ends here
