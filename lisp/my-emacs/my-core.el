;;; my-core.el --- Heart of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((dash "2.19.1") (emacs "30.1") (my-lib "0.1.0") (on "0.1.0") (orderless "1.2") (setup "1.4.0") (svg-lib "0.3") (transient "0.7.5"))
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

;; The heart of my Emacs configuraiton, setup the prerequisites for
;; start the play and arrange the various instruments.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'my-lib)
(require 'on)
(require 'orderless)
(require 'setup)

(cl-eval-when (compile)
  (require 'package)
  (require 'project)
  (require 'server))

;; `popper-reference-buffers' is a variable defined in `popper', but it
;; is evaluated upon starting `popper-mode', modifying its value after
;; `popper' is loaded will be ineffective.  As my settings for this
;; variable are distributed across various files, I define it once here
;; to prevent Emacs complain.  '("\\*Messages\\*$") is its original
;; default value.
(defvar popper-reference-buffers '("\\*Messages\\*$")
  "List of buffers to treat as popups.")



;;;
;; `setup` keywords:

(eval-and-compile

  (setup-define :advice-add
    (lambda (symbol where function)
      `(advice-add ',symbol ,where ,function))
    :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
    :after-loaded t
    :debug '(sexp sexp function-form)
    :ensure '(nil nil func)
    :repeatable t)

  (setup-define :advice-remove
    (lambda (symbol function)
      `(advice-remove ',symbol ,function))
    :documentation "REmove a piece of advice on a function.
See `advice-remove' for more details."
    :after-loaded t
    :debug '(sexp function-form)
    :ensure '(nil func)
    :repeatable t)

  (setup-define :after
    (lambda (feature &rest body)
      `(with-eval-after-load ',feature ,@body))
    :documentation "Eval BODY after FEATURE."
    :indent 1)

  (setup-define :autoload
    (lambda (func)
      (let ((fn (if (memq (car-safe func) '(quote function))
                    (cadr func)
                  func)))
        `(unless (fboundp (quote ,fn))
           (autoload (function ,fn)
             ,(symbol-name (setup-get 'feature))
             nil
             t))))
    :documentation "Autoload FUNC if not already bound."
    :repeatable t
    :signature '(FUNC ...))

  (setup-define :define-advice
    (lambda (symbol args &rest body)
      `(define-advice ,symbol ,args ,@body))
    :documentation "Add a piece of advice on a function.
See `define-advice' for more details."
    :debug '(sexp sexp form)
    :after-loaded t
    :indent 1)

  (setup-define :eval-when
    (lambda (timing &rest body)
      `(cl-eval-when ,timing ,@body))
    :documentation "Control when BODY is evaluated by TIMING.
Usage see `cl-eval-when'."
    :debug '(sexp body)
    :indent 1)

  (setup-define :face
    (lambda (face spec)
      `(custom-set-faces (quote (,face ,spec))))
    :documentation "Customize FACE to SPEC."
    :signature '(face spec ...)
    :debug '(setup)
    :repeatable t
    :after-loaded t)

  (setup-define :first-buffer
    (lambda (function)
      `(add-hook 'on-first-buffer-hook ,function))
    :documentation "Add FUNCTION to `on-first-buffer-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-file
    (lambda (function)
      `(add-hook 'on-first-file-hook ,function))
    :documentation "Add FUNCTION to `on-first-file-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-input
    (lambda (function)
      `(add-hook 'on-first-input-hook ,function))
    :documentation "Add FUNCTION to `on-first-input-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :first-ui
    (lambda (function)
      `(add-hook 'on-init-ui-hook ,function))
    :documentation "Add FUNCTION to `on-init-ui-hook'."
    :ensure '(func)
    :repeatable t)

  (setup-define :init
    (lambda (&rest body)
      (macroexp-progn body))
    :documentation "BODY to run before NAME has been loaded."
    :debug '(form)
    :after-loaded nil
    :indent 1)

  (setup-define :keymap-set
    (lambda (key definition)
      `(keymap-set ,(setup-get 'map) ,key ,definition))
    :documentation "Set KEY to DEFINITION.
See `keymap-set'."
    :debug '(key sexp)
    :ensure '(nil func)
    :repeatable t)

  (setup-define :keymap-set-into
    (lambda (feature-or-map &rest body)
      (if (string-match-p "-map\\'" (symbol-name feature-or-map))
          (progn
            `(:with-map ,feature-or-map (:keymap-set ,@body)))
        `(:with-feature ,feature-or-map (:keymap-set ,@body))))
    :documentation "Set keys to definition into FEATURE-OR-MAP."
    :debug '(sexp &rest key sexp)
    :ensure '(nil func))

  (setup-define :keymap-unset
    (lambda (key remove)
      `(keymap-set ,(setup-get 'map) ,key ,remove))
    :documentation "Unset or REMOVE definition of KEY.
See `keymap-unset'."
    :debug '(key boolean)
    :repeatable t)

  (setup-define :keymap-unset-into
    (lambda (feature-or-map &rest body)
      (if (string-match-p "-map\\'" (symbol-name feature-or-map))
          (progn
            `(:with-map ,feature-or-map (:keymap-unset ,@body)))
        `(:with-feature ,feature-or-map (:keymap-unset ,@body))))
    :documentation "Set keys to definition into FEATURE-OR-MAP."
    :debug '(sexp &rest key boolean))

  (setup-define :quit
    #'setup-quit
    :documentation "Unconditionally abort the evaluation.")

  (setup-define :require-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES.
See https://www.emacswiki.org/emacs/SetupEl#h5o-10."
    :indent 1)

  (setup-define :set
    (setup-make-setter
     (lambda (symbol)
       `(funcall #'symbol-value ',symbol))
     (lambda (symbol value)
       `(funcall #'set ',symbol ,value)))
    :documentation "Set SYMBOL's value to VALUE.
These forms are supported:

(append VAR)   Assuming VAR designates a list, add VAL as its last
               element, unless it is already member of the list.

(prepend VAR)  Assuming VAR designates a list, add VAL to the beginning,
               unless it is already member of the list.

(remove VAR)   Assuming VAR designates a list, remove all instances of
               VAL.

(append* VAR)  Assuming VAR designates a list, add each element of VAL
               to the end of VAR, keeping their order, unless it is
               already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element of VAL
               to the start of VAR, keeping their order, unless it is
               already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all instances of
               each element of VAL."
    :debug '(sexp form)
    :repeatable t)

  (setup-define :set-default
    (setup-make-setter
     (lambda (symbol)
       `(funcall #'symbol-value ',symbol))
     (lambda (symbol value)
       `(funcall #'set-default ',symbol ,value)))
    :documentation "Set SYMBOL's default value to VALUE.
These forms are supported:

(append VAR)   Assuming VAR designates a list, add VAL as its last
               element, unless it is already member of the list.

(prepend VAR)  Assuming VAR designates a list, add VAL to the beginning,
               unless it is already member of the list.

(remove VAR)   Assuming VAR designates a list, remove all instances of
               VAL.

(append* VAR)  Assuming VAR designates a list, add each element of VAL
               to the end of VAR, keeping their order, unless it is
               already a member of the list.

(prepend* VAR) Assuming VAR designates a list, add each element of VAL
               to the start of VAR, keeping their order, unless it is
               already a member of the list.

(remove* VAR)  Assuming VAR designates a list, remove all instances of
               each element of VAL."
    :debug '(sexp form)
    :repeatable t)

  (setup-define :snoc
    (lambda (symbol elem &rest elements)
      `(:set ,symbol (funcall #'-snoc ,symbol ,elem ,@elements)))
    :documentation "Append ELEM and ELEMENTS to the end of SYMBOL."
    :debug '(sexp sexp form))

  (setup-define :snoc-local
    (lambda (symbol elem &rest elements)
      `(add-hook ',(setup-get 'hook)
                 (lambda ()
                   (setq-local ,symbol (funcall #'-snoc
                                                ,symbol
                                                ,elem
                                                ,@elements)))))
    :documentation "Append ELEM and ELEMENTS to the end of SYMBOL."
    :debug '(sexp sexp form))

  (setup-define :when-gui
    (lambda (&rest body)
      `(when (display-graphic-p)
         ,@body))
    :documentation "Evaluate BODY when `display-graphic-p' is non-nil."
    :debug '(form))

  (setup-define :when-os
    (lambda (system &rest body)
      (let ((conditions (mapcar
                         (lambda (sys)
                           `(my-os-is ,sys))
                         system)))
        `(when (or ,@conditions)
           ,@body)))
    :documentation "Evaluate BODY when current OS is memeber of SYSTEM."
    :debug '(sexp form)
    :indent 1)

  (setup-define :when-tui
    (lambda (&rest body)
      `(unless (display-graphic-p)
         ,@body))
    :documentation "Evaluate BODY when `display-graphic-p' is nil."
    :debug '(form)))



;;;
;; Essentials:

(setup package
  (:when-loaded
    (:snoc
     package-archives
     ;; Add GNU Devel ELPA.
     '("gnu-devel" . "https://elpa.gnu.org/devel/")
     ;; Add NonGNU Devel ELPA.
     '("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/"))))

(setup server
  (:with-hook after-init-hook
    (:hook my/server-start))
  (:when-loaded
    (:set
     server-auth-dir (my-state-path "server/"))))



;;;
;; Keymaps:

(defvar ctl-c-map (make-keymap)
  "Default keymap use to bind my commands.")

(defvar ctl-c-4-map (make-keymap)
  "Default keymap use to bind my window operating commands.")

(defvar ctl-c-5-map (make-keymap)
  "Default keymap use to bind my frame operating commands.")

(defvar ctl-c-a-map (make-keymap)
  "Default keymap use to bind my action commands.")

(defvar ctl-c-e-map (make-keymap)
  "Default keymap use to bind my editing commands.")

(defvar ctl-c-f-map (make-keymap)
  "Default keymap use to bind my files operating commands.")

(defvar ctl-c-p-map project-prefix-map
  "Default keymap use to bind my project operating commands.")

(defvar ctl-c-p-tab-map (make-keymap)
  "Default keymap use to bind my project Tab operating commands.")

(defvar ctl-c-s-map (make-keymap)
  "Default keymap use to bind my search commands.")

(defvar ctl-c-v-map (make-keymap)
  "Default keymap use to bind my version controling commands.")

(defvar ctl-c-v-g-map (make-keymap)
  "Default keymap for to bind my version controling (Git) commands.")

(defvar ctl-c-w-map (make-keymap)
  "Default keymap use to bind my workspace commands.")

(defvar ctl-c-home-map (make-keymap)
  "Default keymap use to bind my Emacs operating commands.")

(setup my-maps
  (keymap-set global-map  "C-c"    ctl-c-map)
  (keymap-set ctl-c-map   "4"      ctl-c-4-map)
  (keymap-set ctl-c-map   "5"      ctl-c-5-map)
  (keymap-set ctl-c-map   "a"      ctl-c-a-map)
  (keymap-set ctl-c-map   "e"      ctl-c-e-map)
  (keymap-set ctl-c-map   "f"      ctl-c-f-map)
  (keymap-set ctl-c-map   "p"      ctl-c-p-map)
  (keymap-set ctl-c-map   "s"      ctl-c-s-map)
  (keymap-set ctl-c-map   "v"      ctl-c-v-map)
  (keymap-set ctl-c-map   "w"      ctl-c-w-map)
  (keymap-set ctl-c-map   "<home>" ctl-c-home-map)
  (keymap-set ctl-c-p-map "TAB"  ctl-c-p-tab-map)
  (keymap-set ctl-c-v-map "g"    ctl-c-v-g-map))



;;;
;; Built-in features:

(setup files
  (:keymap-set-into ctl-c-home-map "r" restart-emacs))



;;;
;; Third-Party features:

(setup svg-lib
  (:set-default
   svg-lib-icons-dir (my-data-path "svg-icons/")))

(setup transient
  (:set-default
   transient-history-file (my-state-path "transient/history.el")
   transient-levels-file (my-state-path "transient/levels.el")
   transient-values-file (my-state-path "transient/values.el")))

(provide 'my-core)
;;; my-core.el ends here
