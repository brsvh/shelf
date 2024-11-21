;;; my-erc.el --- Customize `erc' for My Emacs  -*- lexical-binding: t -*-

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
  (require 'erc)
  (require 'erc-services))



;;;
;; Appearance:

(setup erc
  (:when-loaded
    (:snoc erc-modules 'smiley)
    (:set-default
     erc--display-buffer-overriding-action
     '( display-buffer-same-window
        (inhibit-same-window . nil)))))

(setup erc-goodies
  (:when-loaded
    (:set erc-interpret-mirc-color t)))

(setup window
  (:set
   (append display-buffer-alist)
   '( (major-mode . erc-mode)
      (display-buffer-same-window))))



;;;
;; Core:

(setup erc-services
  (:autoload erc-services-mode)
  (:when-loaded
    (:set
     (append erc-nickserv-passwords)
     `( Libera.Chat (("brsvh" . ,(password-store-get "irc.libera.chat/brsvh"))))
     erc-prompt-for-nickserv-password nil)))

(setup erc
  (:autoload erc)
  (:keymap-set-into ctl-c-a-map "i" erc)
  (:when-loaded
    (:set
     erc-nick "brsvh"
     erc-server "irc.libera.chat"
     erc-user-full-name user-full-name)
    (:with-hook erc-before-connect
      (:hook
       (lambda (&rest _)
         (erc-update-modules))))))



;;;
;; Log:

(setup erc-log
  (:when-loaded
    (:set
     erc-log-channels-directory (my-data-path* "erc" "log/"))))

(setup erc
  (:when-loaded
    (:snoc erc-modules 'log)))



;;;
;; Services:

(setup erc
  (:when-loaded
    (:snoc erc-modules 'services)))



(provide 'my-erc)
;;; my-erc.el ends here
