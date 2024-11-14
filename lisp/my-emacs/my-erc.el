;;; my-erc.el --- Customize `erc' for My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (my-core "0.2.0"))
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
;; Core:

(setup erc-services
  (:autoload erc-services-mode)
  (:when-loaded
    (:set
     (append erc-nickserv-passwords)
     `( Libera.Chat (("brsvh" . ,(password-store-get "irc.libera.chat/brsvh"))))
     erc-prompt-for-nickserv-password nil)))

(setup erc
  (:when-loaded
    (:set
     erc-nick "brsvh"
     erc-server "irc.libera.chat"
     erc-user-full-name user-full-name)
    (:snoc erc-modules 'services 'smiley)
    (:with-hook erc-before-connect
      (:hook
       (lambda ()
         (erc-update-modules))))))



(provide 'my-erc)
;;; my-erc.el ends here
