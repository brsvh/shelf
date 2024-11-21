;;; my-mu4e.el --- `mu4e' enhancements of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((emacs "30.1") (mu4e "1.12.7"))
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

;; This file provides support for sending and receiving my mails.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'mu4e-alert)
  (require 'mu4e-marker-icons)
  (require 'mu4e-update)
  (require 'simple))



;;;
;; mu:

(setup mu4e-headers
  (:autoload mu4e~headers-flags-str)
  (:when-loaded
    (:set
     ;; Change default header date format, prefered to use locales' date
     ;; and time format.
     mu4e-headers-date-format "%x %X"

     ;; Show less fields.
     mu4e-headers-fields
     '(( :human-date . 25)
       ( :flags . 8)
       ( :from . 22)
       ( :subject)))))

(setup mu4e-marker-icons
  (:autoload mu4e-marker-icons-mode)
  (:after mu4e
    (mu4e-marker-icons-mode +1)))

(setup mu4e-thread
  (:when-loaded
    (:with-map mu4e-thread-mode-map
      (:keymap-unset "C-<tab>" t))))

(setup mu4e-update
  (:snoc
   popper-reference-buffers "\\*mu4e-update\\*")
  (:when-loaded
    (:set
     ;; Hide annoying "mu4e Retrieving mail..." msg in mini buffer:
     mu4e-hide-index-messages nil

     ;; Retrieving and indexing messages every 5 minutes.
     mu4e-update-interval 300)))

(setup simple
  (:when-loaded
    (:set
     mail-user-agent 'mu4e-user-agent)))

(setup window
  (:set
   (append display-buffer-alist)
   '( "\\*mu4e-main\\*"
      (display-buffer-same-window))))



;;;
;; Notification:

(setup mu4e-alert
  (:autoload mu4e-alert-enable-notifications)
  (:when-loaded
    (:when-os (linux)
      (:set mu4e-alert-set-default-style 'notifications)
      (mu4e-alert-enable-notifications))))

(setup mu4e
  (:when-loaded
    (:also-load mu4e-alert)))



(provide 'my-mu4e)
;;; my-mu4e.el ends here
