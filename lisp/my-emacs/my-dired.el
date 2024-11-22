;;; my-dired.el --- Customize `dired' for My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((dired-git-info "0.3.1") (dired-open "0.0.1") (diredfl "0.5") (emacs "30.1") (nerd-icons-dired "0.0.1"))
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
  (require 'dired)
  (require 'dired-aux)
  (require 'dired-git-info)
  (require 'dired-open)
  (require 'dired-x)
  (require 'diredfl)
  (require 'nerd-icons-dired))



;;;
;; Appearance:

(setup diredfl
  (:autoload diredfl-mode))

(setup nerd-icons-dired
  (:autoload nerd-icons-dired-mode))

(setup dired
  (:when-gui
   (:with-hook dired-mode-hook
     (:hook
      diredfl-mode          ;; Colorful `dired'.
      nerd-icons-dired-mode ;; `dired' with Nerd icons.
      (lambda ()            ;; Inhibit fill column indicator.
        (when (bound-and-true-p display-fill-column-indicator-mode)
          (display-fill-column-indicator-mode -1)))))))



;;;
;; Core:

(setup dired-open
  (:autoload dired-open-xdg))

(setup dired
  (:when-loaded
    (:set
     ;; Open directory what I mean.
     dired-dwim-target t

     ;; Copy and delete files recursively.
     dired-recursive-copies 'always
     dired-recursive-deletes 'always

     ;; Use my arguments of ls.
     dired-listing-switches (concat "-l "
                                    "--almost-all "
                                    "--human-readable "
                                    "--group-directories-first "
                                    "--no-group"))
    (:also-load dired-aux ;; Other part of `dired'.
                dired-x)  ;; Desktop support.
    (:after dired-x
      (:when-os (linux)
        (:set
         ;; Open files with some specified extensions by external
         ;; programs.
         dired-guess-shell-alist-user
         '( ("\\.\\(?:djvu\\|eps\\)\\'" "xdg-open")
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "xdg-open")
            ("\\.\\(?:mp3\\|flac\\)\\'" "xdg-open")
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "xdg-open")
            ("\\.\\(?:xcf\\)\\'" "xdg-open")
            ("\\.csv\\'" "xdg-open")
            ("\\.docx\\'" "xdg-open")
            ("\\.html?\\'" "xdg-open")
            ("\\.md\\'" "xdg-open")
            ("\\.tex\\'" "xdg-open")
            ("\\.pdf\\'" "xdg-open")))))
    (:with-map dired-mode-map
      (:keymap-set
       "M-<return>" dired-open-xdg))))



;;;
;; Extensions:

(setup dired-git-info
  (:autoload dired-git-info-mode))

(setup dired
  (:when-loaded
    (:with-map dired-mode-map
      (:keymap-set
       ")" dired-git-info-mode))))



(provide 'my-dired)
;;; my-dired.el ends here
