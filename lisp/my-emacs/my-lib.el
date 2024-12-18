;;; my-lib.el --- My Emacs Lisp extensions  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
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

;; These are extensions that provide additional features to support my
;; personal Emacs configuration.

;;; Code:

(require 'cl-lib)

(cl-eval-when (compile)
  (require 'server))

;;;###autoload
(defgroup my nil
  "Customize my Emacs Configuration."
  :prefix "my-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/brsvh/my-emacs"))



;;;
;; Common:

;;;###autoload
(defmacro my-os-is (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx', `windows',
`linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

;;;###autoload
(defmacro my-xdg-base-dir-for (concept)
  "Get the value of corresponds XDG Base Directory CONCEPT.
Allowable concepts (not quoted) are `cache', `config', `data' and
 `state'."
  (let* ((concepts '( (cache . ("XDG_CACHE_HOME" . "~/.cache/"))
                      (config . ("XDG_CONFIG_HOME" . "~/.config/"))
                      (data . ("XDG_DATA_HOME" . "~/.local/share/"))
                      (state . ("XDG_STATE_HOME" . "~/.local/state/")))))
    `(let ((default-cons (cdr (assoc ',concept ',concepts))))
       (expand-file-name (or (getenv (car default-cons))
                             (cdr default-cons))))))

;;;###autoload
(defun my-path (&rest segments)
  "Join SEGMENTS to a path."
  (let (file-name-handler-alist path)
    (setq path (expand-file-name (if (cdr segments)
                                     (apply #'file-name-concat segments)
                                   (car segments))))
    (if (file-name-absolute-p (car segments))
        path
      (file-relative-name path))))

;;;###autoload
(defun my-path= (a b)
  "Return t if A and B are references to same path."
  (equal
   (file-truename (directory-file-name (my-path a)))
   (file-truename (directory-file-name (my-path b)))))

;;;###autoload
(defun my-path? (a b)
  "Return t if B is under A."
  (let ((a (my-path a)) (b (my-path b)))
    (string-prefix-p a b)))

;;;###autoload
(defalias 'my-dir 'file-name-as-directory)

;;;###autoload
(defun my-dirname (path)
  "Return the parent directory to PATH."
  (let ((s  (file-name-directory (directory-file-name (my-path path)))))
    (unless (my-path= path s)
      (if (file-name-absolute-p path)
          (directory-file-name s)
        (file-relative-name s)))))

;;;###autoload
(defun my-path* (&rest segments)
  "Join SEGMENTS to a path, ensure it is exists."
  (let ((path (apply #'my-path segments)) dir)
    (if (and (my-os-is linux) (string-suffix-p "/" path))
        (setq dir path)
      (setq dir (my-dirname path)))
    (make-directory dir 'parents)
    path))

;;;###autoload
(defcustom my-cache-directory
  (if (not (my-os-is windows))
      (my-path (my-xdg-base-dir-for cache) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-config-directory
  (if (not (my-os-is windows))
      (my-path (my-xdg-base-dir-for config) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional config files are placed."
  :type 'directory
  :group 'my)

;;;###autoload
(defcustom my-data-directory
  (if (not (my-os-is windows))
      (my-path (my-xdg-base-dir-for data) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional non-volatile files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defcustom my-state-directory
  (if (not (my-os-is windows))
      (my-path (my-xdg-base-dir-for state) "emacs/")
    user-emacs-directory)
  "Directory beneath which additional state files are placed."
  :group 'my
  :type 'directory)

;;;###autoload
(defun my-cache-path (&rest segments)
  "Join SEGMENTS to `my-cache-directory'."
  (apply #'my-path (append (list my-cache-directory) segments)))

;;;###autoload
(defun my-cache-path* (&rest segments)
  "Join SEGMENTS to `my-cache-directory', ensure it is exists."
  (apply #'my-path* (append (list my-cache-directory) segments)))

;;;###autoload
(defun my-config-path (&rest segments)
  "Join SEGMENTS to `my-config-directory'."
  (apply #'my-path (append (list my-config-directory) segments)))

;;;###autoload
(defun my-config-path* (&rest segments)
  "Join SEGMENTS to `my-config-directory', ensure it is exists."
  (apply #'my-path* (append (list my-config-directory) segments)))

;;;###autoload
(defun my-data-path (&rest segments)
  "Join SEGMENTS to `my-data-directory'."
  (apply #'my-path (append (list my-data-directory) segments)))

;;;###autoload
(defun my-data-path* (&rest segments)
  "Join SEGMENTS to `my-data-directory', ensure it is exists."
  (apply #'my-path* (append (list my-data-directory) segments)))

;;;###autoload
(defun my-state-path (&rest segments)
  "Join SEGMENTS to `my-state-directory'."
  (apply #'my-path (append (list my-state-directory) segments)))

;;;###autoload
(defun my-state-path* (&rest segments)
  "Join SEGMENTS to `my-state-directory', ensure it is exists."
  (apply #'my-path* (append (list my-state-directory) segments)))

(defcustom my-authinfo-file (my-config-path "authinfo")
  "Location of my authinfo file."
  :group 'my
  :type 'file)

(defcustom my-authinfo-gpg-file (my-config-path "authinfo.gpg")
  "Location of my authinfo.gpg file."
  :group 'my
  :type 'file)

;;;###autoload
(defcustom my-custom-file
  (my-state-path "custom.el")
  "Location of my `custom-file'."
  :group 'my
  :type 'file)

;;;###autoload
(defcustom my-etc-directory (my-config-path "etc/")
  "Directory beneath which my configuration files are placed."
  :type 'directory
  :group 'my)

;;;###autoload
(defcustom my-native-lisp-directory (my-cache-path "eln-cache/")
  "Directory beneath which third-party Emacs Lisp files are placed."
  :type 'directory
  :group 'my)



;;;
;; Fonts:

;;;###autoload
(defgroup my-fonts nil
  "Fonts for my Emacs configuraiton."
  :group 'my)

;;;###autoload
(defcustom my-font-name "IBM Plex Mono"
  "Name of my default font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-monospace-font-name "IBM Plex Mono"
  "Name of my monospace font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-sans-serif-font-name "IBM Plex Sans"
  "Name of my sans-serif font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-font-size 14
  "Name of my default font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-latin-font-name "IBM Plex Mono"
  "Name of my latin font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-latin-monospace-font-name "IBM Plex Mono"
  "Name of my latin monospace font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-latin-sans-serif-font-name "IBM Plex Sans"
  "Name of my latin sans-serif font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-chinese-font-name "LXGW WenKai Mono"
  "Name of my chinese font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-chinese-monospace-font-name "LXGW WenKai Mono"
  "Name of my chinese monospace font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-chinese-sans-serif-font-name "IBM Plex Sans SC"
  "Name of my chinese sans-serif font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-symbol-font-name "Symbols Nerd Font Mono"
  "Name of my symbol font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-symbol-sans-serif-font-name "Symbols Nerd Font"
  "Name of my sans-serif font."
  :group 'my-fonts
  :type 'string)

;;;###autoload
(defcustom my-symbol-monospace-font-name "Symbols Nerd Font Mono"
  "Name of my symbol monospace font."
  :group 'my-fonts
  :type 'string)



;;;
;; Server:

;;;###autoload
(defun my/server-start ()
  "Allow this Emacs process to be a server for client processes."
  (interactive)
  (eval-and-compile
    (require 'server))
  (unless (server-running-p) (server-start)))



;;;
;; Themes:

;;;###autoload
(defcustom my-light-theme 'modus-operandi-tinted
  "Default light theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defcustom my-dark-theme 'modus-vivendi-tinted
  "Default dark theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defcustom my-theme my-light-theme
  "Default theme."
  :group 'my
  :type 'symbol)

;;;###autoload
(defun my-select-proper-theme (&rest _)
  "Set `my-theme' to proper variant base on window system type."
  (if (display-graphic-p)
      (setq my-theme my-light-theme)
    (setq my-theme my-dark-theme)))

;;;###autoload
(defun my-load-theme (theme)
  "Load THEME."
  (when (eq theme 'default)
    (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (if (custom-theme-p theme)
          (enable-theme theme)
        (load-theme theme :no-confirm)))))

;;;###autoload
(defun my-theme-setup (&rest _)
  "Setup `my-theme'."
  (my-select-proper-theme)
  (my-load-theme my-theme))

;;;###autoload
(defun my/toggle-theme ()
  "Toggle themes."
  (interactive)
  (cond ((member my-light-theme custom-enabled-themes)
         (my-load-theme my-dark-theme))
        ((member my-light-theme custom-enabled-themes)
         (my-load-theme my-dark-theme))
        (t (my-theme-setup))))



;;;
;; Trivial functions/commands:

;;;###autoload
(defun my-silent-message (fn &rest args)
  "Silencing load message, around FN with original ARGS."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply fn args)))



(provide 'my-lib)
;;; my-lib.el ends here
