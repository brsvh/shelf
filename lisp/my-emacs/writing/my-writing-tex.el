;;; my-writing-tex.el --- Writing with TeX -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((auctex "14.0.6") (citar "1.4.0") (emacs "30.1"))
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

;; This file provides support for writing in TeX.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'citar)
  (require 'citar-capf)
  (require 'latex)
  (require 'reftex)
  (require 'tex)
  (require 'tex-site))



;;;
;; Common:

(setup reftex
  (:autoload turn-on-reftex))

(setup tex-site
  (:set TeX-auto-global (my-cache-path "auctex/")))

(setup tex
  (:autoload
   TeX-mode
   TeX-error-overview-mode
   TeX-output-mode)
  ;; Popup error, help, and output buffer.
  (:snoc popper-reference-buffers
         'TeX-error-overview-mode
         "\\*TeX Help\\*"
         'TeX-output-mode)
  (:set
   ;; Automatically generated AUCTeX style files.
   TeX-auto-local ".auto"
   TeX-auto-save t
   TeX-parse-self t)
  (:set-default
   TeX-master nil)
  (:when-loaded
    (:option
     ;; Automatically generated AUCTeX style files.
     (append TeX-auto-private) (my-data-path "auctex/auto/")
     ;; Manually generated AUCTeX style files
     (append TeX-style-private) (my-data-path "auctex/manual/"))
    (:snoc TeX-command-list
           '( "XeLaTeX" "xelatex -interaction=nonstopmode %s"
              TeX-run-command t t
              :help "Run XeLaTeX"))))



;;;
;; Citation:

(setup citar-capf
  (:autoload citar-capf-setup))

(setup citar
  (:when-loaded
    (:set citar-at-point-function 'embark-act)))

(setup tex
  (:with-hook TeX-mode
    (:hook citar-capf-setup)))



;;;
;; LaTeX:

(setup latex
  (:autoload LaTeX-mode)
  (:with-hook LaTeX-mode-hook
    (:hook
     ;; Support labels, references, citations, and index.
     turn-on-reftex)))



(provide 'my-writing-tex)
;;; my-writing-tex.el ends here
