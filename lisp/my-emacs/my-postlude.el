;;; my-postlude.el --- Postlude of My Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((emacs "30.1"))
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

;; The postlude of my Emacs configuration, ensure something that need to
;; be set at the very end of initialization.

;;; Code:

(require 'my-core)

(defun my-postlude--load-custom-file (&rest _)
  "Load `custom-file'."
  (when (file-exists-p custom-file)
    (load custom-file nil 'nomessage)))



(progn
  ;; Load customizations.
  (my-postlude--load-custom-file))

(provide 'my-postlude)
;;; my-postlude.el ends here
