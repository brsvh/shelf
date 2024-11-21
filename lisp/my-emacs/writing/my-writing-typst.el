;;; my-writing-typst.el --- Writing with Typst -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1") (typst-ts-mode "0.10.0"))
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

;; This file provides support for writing in Typst.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'typst-ts-mode))

(defun my-writing-typst-find-grammar (&rest _)
  "Find 'libtree-sitter-typst.so'."
  (cl-some
   (lambda (dir)
     (let ((files (directory-files dir t)))
       (cl-find-if (lambda (file)
                     (string-equal (file-name-nondirectory file)
                                   "libtree-sitter-typst.so"))
                   files)))
   treesit-extra-load-path))



;;;
;; Major modes:

(setup typst-ts-mode
  (:autoload typst-ts-mode)
  (:with-mode typst-ts-mode
    (:file-match
     "\\.typ\\'"))
  (:when-loaded
    (:set
     typst-ts-mode-grammar-location (my-writing-typst-find-grammar))))



(provide 'my-writing-typst)
;;; my-writing-typst.el ends here
