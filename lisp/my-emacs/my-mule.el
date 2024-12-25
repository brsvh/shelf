;;; my-mule.el --- Multilingual environment of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (pangu-spacing "0.4") (rime "1.0.5"))
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
(require 'rime)

(cl-eval-when (compile)
  (require 'rx)
  (require 'pangu-spacing))



(defun my/toggle-real-enclose-chinese-with-spaces (&rest _)
  "Make `pangu-spacing-mode' real insert spaces."
  (interactive)
  (setq-local pangu-spacing-real-insert-separtor
              (not pangu-spacing-real-insert-separtor)))

(defvar my-geometric-shape-ranges
  '( (#x25A0 . #x25FF))
  "Unicode code point ranges of geometric shapes.")

(defvar my-symbol-code-point-ranges
  '( (#x23fb . #x23fe)
     (#x2b58 . #x2b58)
     (#x2665 . #x2665)
     (#x26a1 . #x26a1)
     (#xe000 . #xe00a)
     (#xe0a0 . #xe0a2)
     (#xe0a3 . #xe0a3)
     (#xe0b0 . #xe0b3)
     (#xe0b4 . #xe0c8)
     (#xe0ca . #xe0ca)
     (#xe0cc . #xe0d7)
     (#xe000 . #xe0a9)
     (#xf000 . #xf0eb)
     (#xe5fa . #xe6b7)
     (#xe700 . #xe8ef)
     (#xea60 . #xec1e)
     (#xed00 . #xefce)
     (#xf000 . #xf2ff)
     (#xf300 . #xf381)
     (#xf000 . #xf305)
     (#xf001 . #xf847)
     (#xf0001 . #xf1af0))
  "Unicode code point ranges of symbols.")

(defun my/geometric-shape-font-setup (&rest _)
  "Setup my geometric shapes font."
  (interactive)
  (dolist (range my-geometric-shape-ranges)
    (set-fontset-font t range (font-spec :family my-font-name))))

(defun my/symbol-font-setup (&rest _)
  "Setup my symbols font."
  (interactive)
  (dolist (range my-symbol-code-point-ranges)
    (set-fontset-font t range (font-spec :family my-symbol-font-name))))



;;;
;; Common settings:

(setup mule-cmds
  (set-default-coding-systems 'utf-8)
  (set-language-environment "utf-8")
  (prefer-coding-system 'utf-8)
  (:set-default default-input-method "rime")
  ;; Prevent the system-wide input method.
  (:when-pgtk
   (:set pgtk-use-im-context-on-new-connection nil)
   ;; Ensure GTK IM Module is disabled in current session.
   (pgtk-use-im-context nil)))

(setup emacs
  (:when-gui
   (set-face-attribute 'default
                       nil
                       :font (font-spec :family my-font-name
                                        :size my-font-size))
   (my/geometric-shape-font-setup))
  (:set-default word-wrap-by-category t))

(setup face-remap
  (:autoload variable-pitch-mode)
  (:when-loaded
    (:face
     fixed-pitch nil :font (font-spec :family my-monospace-font-name)
     variable-pitch nil :font (font-spec :family my-sans-serif-font-name))))



;;; Chinese:

(setup emacs
  (:when-gui
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
     "C-SPC" pangu-spacing-mode))
  (:when-loaded
    (:set pangu-spacing-real-insert-separtor t)))

(setup rime
  (:when-loaded
    (:when-gui
     (:face
      rime-candidate-num-face ((t ( :inherit font-lock-keyword-face  :bold nil)))
      rime-comment-face (( t ( :inherit font-lock-comment-face)))
      rime-default-face (( t ( :inherit default))))
     (:set
      rime-posframe-properties '( :internal-border-width 2)
      rime-show-candidate 'posframe))))



;;;
;; Latin:

(setup emacs
  (:when-gui
   (set-fontset-font t
                     'latin
                     (font-spec :family my-latin-font-name))))



;;;
;; Symbol:

(setup emacs
  (:when-gui
   (my/symbol-font-setup)))



(provide 'my-mule)
;;; my-mule.el ends here
