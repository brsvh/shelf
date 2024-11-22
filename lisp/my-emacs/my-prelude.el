;;; my-prelude.el --- Prelude of My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((benchmark-init "1.2") (cl-lib "1.0") (gcmh "0.2.1") (emacs "30.1"))
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

;; The prelude of my Emacs configuration, setup matters of utmost
;; importance before initialization.

;; This file should be loaded at the `early-init-file' or the very
;; beginning of either the `user-init-file'.

;;; Code:

(require 'cl-lib)
(require 'my-lib)

(cl-eval-when (compile)
  (require 'benchmark-init)
  (require 'gcmh))

(defvar my-prelude-initial-file-name-handler-alist
  file-name-handler-alist
  "The initial value of `file-name-handler-alist'.")

(defvar my-prelude-frame-alist '( (height . 40)
                                  (horizontal-scroll-bars . nil)
                                  (internal-border-width . 6)
                                  (menu-bar-lines . nil)
                                  (right-divider-width . 6)
                                  (tool-bar-lines . nil)
                                  (vertical-scroll-bars . nil)
                                  (width . 120))
  "Frame parameters for setup frame creation.")

(defmacro my-prelude-autoload (func feature)
  "Autoload FUNC from FEATURE."
  `(unless (fboundp ',func)
     (autoload #',func ,(symbol-name feature) nil t)))

(defun my-prelude-reset-gc-threshold (&rest _)
  "Reset `gc-cons-threshold'."
  (set-default-toplevel-value
   'gc-cons-threshold
   (default-toplevel-value 'gc-cons-threshold)))

(defun my-prelude-reset-mode-line (&rest _)
  "Reset Mode Line."
  (set-default-toplevel-value
   'mode-line-format
   (default-toplevel-value 'mode-line-format)))

(defun my-prelude-restore-file-name-handler (&rest _)
  "Restore the appropriate value of `file-name-handler-alist'."
  (let ((prev my-prelude-initial-file-name-handler-alist)
        (current file-name-handler-alist))
    (set-default-toplevel-value
     'file-name-handler-alist
     (delete-dups (append current prev)))))



;;;
;; Runtime:

(my-prelude-autoload startup-redirect-eln-cache startup)

;; Set a customized path for user Emacs Lisp Native Compilation file
;; storage.
(startup-redirect-eln-cache my-native-lisp-directory)

;; Inhibit save customizations to `user-init-file`.
(setq custom-file my-custom-file)

;; Redirect the location of directory containing the user’s Emacs Lisp
;; packages.
(setq package-user-dir (my-data-path (format "elpa/%s/"
                                             emacs-version))
      package-quickstart-file (my-path package-user-dir
                                       "autoloads.el"))

;; Reset frame layout.
(setq default-frame-alist
      (delete-dups (append default-frame-alist
                           my-prelude-frame-alist)))



;;;
;; Startup optimizations:

(progn                                          ;; Garbage collections
  (my-prelude-autoload gcmh-mode gcmh)

  ;; Activate intelligent garbage collections.
  (add-hook 'after-init-hook #'gcmh-mode 100)

  ;; Restore the garbage collection threshold to its default value.
  (advice-add 'gcmh-mode :before #'my-prelude-reset-gc-threshold))

(unless (daemonp)                                 ;; File name handler
  ;; Keep the initial file name handler.
  (put 'file-name-handler-alist
       'initial-value
       my-prelude-initial-file-name-handler-alist)

  ;; Reduce the number of suffixes supported by file name handler
  ;; during the startup to save overhead.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (eval-when-compile
         (locate-file-internal "calc-loaddefs.el" load-path))
       nil
     (list (rassq 'jka-compr-handler
                  my-prelude-initial-file-name-handler-alist))))

  ;; Restore the appropriate value of `file-name-handler-alist`.
  (add-hook 'after-init-hook
            #'my-prelude-restore-file-name-handler
            100))

(unless noninteractive                                   ;; Appearance
  ;; Avoid window resizing caused by font changes at the startup.
  (setq frame-inhibit-implied-resize t)

  ;; Keep the initial Mode Line.
  (put 'mode-line-format
       'initial-value
       (default-toplevel-value 'mode-line-format))

  ;; Avoid Mode Line the rendering during the configuration evaluation
  ;; process.
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf (setq mode-line-format nil))))



;;;
;; Benchmark:

(progn
  (my-prelude-autoload benchmark-init/activate benchmark-init)
  (my-prelude-autoload benchmark-init/deactivate benchmark-init)

  ;; Activate `benchmark-init' as early as possible to capture loading
  ;; information during the startup process.
  (benchmark-init/activate)

  ;; Deactivate `benchmark-init' at the very end of `after-init-hook'.
  (add-hook 'after-init-hook #'benchmark-init/deactivate 100))



(provide 'my-prelude)
;;; my-prelude.el ends here
