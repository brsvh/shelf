;;; my-writing-markdown.el --- Writing with `markdown-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (grip-mode "2.3.3") (markdown-mode "2.6") (my-core "0.2.0") (valign "3.1.1"))
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

;; This file provides support for writing in Markdown, including editing
;; and previewing.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'grip-mode)
  (require 'markdown-mode)
  (require 'valign))



;;;
;; Major modes:

(setup markdown-mode
  (:autoload gfm-mode markdown-mode)
  (:with-mode markdown-mode
    (:file-match
     "\\.markdown\\'"
     "\\.md\\'"
     "\\.mdown\\'"
     "\\.mkd\\'"
     "\\.mkdn\\'"))
  (:with-mode gfm-mode
    (:file-match
     "README\\.md\\'"
     ;; Match README_en.md, README-zh.md, etc.
     "README[_-]\\([a-zA-Z]+\\)\\.md"
     ;; Match README_zh-Hans.md, README-zh_CN.md, etc.
     "README[_-]\\([a-zA-Z]+[_-][a-zA-Z]+\\)\\.md"))
  (:when-loaded
    (:set
     markdown-command "multimarkdown")))



;;;
;; Editing:

(setup markdown-mode
  (:with-hook (markdown-mode-hook gfm-mode-hook)
    (:hook
     ;; Auto insert paried '*', '_', and '`'.
     #'(lambda ()
         (electric-pair-local-mode +1)
         (:snoc-local electric-pair-pairs
                      (cons ?* ?*)
                      (cons ?_ ?_)
                      (cons ?` ?`))))))



;;;
;; Live preview:

(setup grip-mode
  (:autoload grip-mode)
  (:after markdown-mode
    (:keymap-set-into markdown-mode-command-map "g" #'grip-mode)
    (let ((credential (auth-source-user-and-password "api.github.com")))
      (:set
       grip-github-user (car credential)
       grip-github-password (cadr credential))))
  (:when-loaded
    (:set grip-preview-use-webkit nil)))



;;;
;; Table:

;; Align variable-pitch font, CJK characters and images in tables.
(setup valign
  (:autoload valign-mode)
  (:with-hook gfm-mode-hook
    (:hook #'valign-mode))
  (:with-hook markdown-mode-hook
    (:hook #'valign-mode)))



(provide 'my-writing-markdown)
;;; my-writing-markdown.el ends here
