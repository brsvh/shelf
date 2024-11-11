;;; my-mule.el --- Multilingual environment of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (my-core "0.2.0") (pangu-spacing "0.4"))
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

;; Enable my Emacs to support multiple language environments, currently
;; supporting:
;;  - English
;;  - Chinese

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'rx)
  (require 'pangu-spacing))



(defun my/toggle-real-enclose-chinese-with-spaces (&rest _)
  "Make `pangu-spacing-mode' real insert spaces."
  (interactive)
  (setq-local pangu-spacing-real-insert-separtor
              (not pangu-spacing-real-insert-separtor)))



;;;
;; Common settings:

(setup mule-cmds
  (set-default-coding-systems 'utf-8)
  (set-language-environment "utf-8")
  (prefer-coding-system 'utf-8))

(setup emacs
  (:when-gui
   (:set my-font-name "IBM Plex Mono"
         my-font-size 13)
   (set-face-attribute 'default
                       nil
                       :font (font-spec :family my-font-name))
   (:set-default word-wrap-by-category t)))



;;; Chinese:

(setup emacs
  (:when-gui
   (:set my-chinese-font-name "LXGW WenKai Mono")
   (set-fontset-font t
                     'cjk-misc
                     (font-spec :family my-chinese-font-name))
   (set-fontset-font t
                     'han
                     (font-spec :family my-chinese-font-name))))

;; Automatically add spaces at the junctions when Chinese characters are
;; mixed with other languages.
(setup pangu-spacing
  (:autoload pangu-spacing-mode)
  (:with-map ctl-c-e-map
    (:keymap-set
     "C-SPC" #'pangu-spacing-mode))
  (:when-loaded
    (:set pangu-spacing-real-insert-separtor t)))



;;;
;; Latin:

(setup emacs
  (:when-gui
   (:set my-latin-font-name "IBM Plex Mono")
   (set-fontset-font t
                     'latin
                     (font-spec :family my-latin-font-name))))



;;;
;; Symbol:

(setup emacs
  (:when-gui
   (:set my-symbol-font-name "Symbols Nerd Font Mono")
   (set-fontset-font t
                     'symbol
                     (font-spec :family my-symbol-font-name))))



(provide 'my-mule)
;;; my-mule.el ends here
