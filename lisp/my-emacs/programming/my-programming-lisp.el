;;; my-programming-lisp.el --- Porgramming with Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (parinfer-rust-mode "0.9.0") (slime "2.31"))
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

;; This file has enhanced my Lisp-family programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'inf-lisp)
  (require 'lisp-mode)
  (require 'parinfer-rust-mode)
  (require 'slime)
  (require 'slime-repl))



;;;
;; Core:

(setup slime
  (:autoload slime slime-setup)
  (:when-loaded
    (:set
     ;; Auto connect slime.
     slime-auto-start 'always

     ;; Contrib features will be loaded.
     slime-contribs '( slime-asdf
                       slime-company
                       slime-fancy
                       slime-mrepl
                       slime-quicklisp
                       slime-references))))

(setup lisp-mode
  (:when-loaded
    (when (executable-find "sbcl")
      (:set inferior-lisp-program "sbcl"))
    (:with-hook lisp-mode-hook
      (:hook
       (lambda nil
         (unless (memq 'slime-lisp-mode-hook lisp-mode-hook)
           (:also-load slime-autoloads slime)
           (slime-setup)))))))



;;;
;; Documentation:

(setup slime
  (:autoload slime-documentation)
  (:snoc popper-reference-buffers
         "\\*slime-description\\*")
  (:when-loaded
    (:with-map slime-doc-map
      (:keymap-set
       "C-h" slime-documentation))))



;;;
;; Parens editing:

(setup parinfer-rust-mode
  (:autoload parinfer-rust-mode))

(setup lisp-mode
  (:hook parinfer-rust-mode))



;;; REPL:

(setup slime-repl
  (:snoc popper-reference-buffers
         'slime-repl-mode)
  (:when-loaded
    (:set
     slime-repl-history-file (my-state-path* "slime/" "history.eld"))))



(provide 'my-programming-lisp)
;;; my-programming-lisp.el ends here
