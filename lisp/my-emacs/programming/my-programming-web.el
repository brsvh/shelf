;;; my-programming-web.el --- Web(site) Development -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((eglot "1.17") (emacs "30.1") (rainbow-mode "1.0.6") (web-mode "17.3.20"))
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

;; This file has enhanced my web programming experience.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'css-mode)
  (require 'eglot)
  (require 'html-ts-mode)
  (require 'mhtml-mode)
  (require 'rainbow-mode)
  (require 'sgml-mode)
  (require 'web-mode))

(define-derived-mode astro-mode web-mode "Astro"
  "Major mode to edit Astro.js file."
  (setq-local web-mode-enable-front-matter-block t
              web-mode-engine "astro"))

(define-derived-mode vue-mode web-mode "Vue"
  "Major mode to edit Vue.js file."
  (setq-local web-mode-engine "vue"))

(defvar my-html-language-server-options '( "vscode-html-language-server"
                                           "--stdio")
  "Options of html-language-server program.")

(defvar my-vue-language-server-options '( "vue-language-server"
                                          "--stdio")
  "Options of vue-language-server program.")



;;;
;; Major modes:

(setup astro-mode
  (:with-mode astro-mode
    (:file-match
     "\\.astro\\'")))

(setup css-mode
  (:autoload css-ts-mode css-mode)
  (:with-mode css-ts-mode
    (:file-match
     "\\.css\\'")))

(setup html-ts-mode
  (:autoload html-ts-mode)
  (:with-mode html-ts-mode
    (:file-match
     "\\.html\\'")))

(setup mhtml-mode
  (:autoload mhtml-mode))

(setup sgml-mode
  (:autoload html-mode))

(setup vue-mode
  (:with-mode vue-mode
    (:file-match
     "\\.vue\\'")))

(setup web-mode
  (:autoload web-mode))



;;;
;; Appearance:

(setup rainbow-mode
  (:autoload rainbow-mode)
  (:with-mode css-ts-mode
    (:hook rainbow-mode))
  (:with-mode css-mode
    (:hook rainbow-mode))
  (:with-mode mhtml-mode
    (:hook rainbow-mode))
  (:with-mode html-mode
    (:hook rainbow-mode))
  (:with-mode vue-mode
    (:hook vue-mode))
  (:with-mode web-mode
    (:hook rainbow-mode)))



;;;
;; Language Server:

(setup eglot
  (:autoload eglot-ensure)
  (:when-loaded
    (:snoc
     eglot-server-programs
     (cons 'html-ts-mode my-html-language-server-options)
     (cons 'vue-mode my-vue-language-server-options))))

(setup css-mode
  (:with-mode css-ts-mode
    (:hook eglot-ensure))
  (:with-mode css-mode
    (:hook eglot-ensure)))

(setup vue-mode
  (:with-mode vue-mode
    (:hook eglot-ensure)))



(provide 'my-programming-web)
;;; my-programming-web.el ends here
