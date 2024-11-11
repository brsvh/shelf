;;; my-workspace.el --- Workspace support for My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((activities "0.7.1")(emacs "30.1"))
;; URL: https://github.com/brsvh/shelf
;; Version: 0.1.50

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

;; This file provides support for workspace management, though not true
;; workspaces, only offering limited isolation.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'activities)
  (require 'activities-list)
  (require 'activities-tabs))



;;;
;; Activities:

(setup activities-list
  (:autoload activities-list))

(setup activites-tabs
  (:autoload activities-tabs-mode))

(setup activities
  (:autoload
   activities-discard
   activities-discard
   activities-kill
   activities-list
   activities-mode
   activities-new
   activities-resume
   activities-revert
   activities-suspend
   activities-switch)
  (:snoc popper-reference-buffers
         "\\*Activities\\*")
  (:with-map ctl-c-w-map
    (:keymap-set
     "b"   #'activities-switch-buffer
     "g"   #'activities-revert
     "RET" #'activities-switch
     "C-d" #'activities-discard
     "C-k" #'activities-kill
     "C-l" #'activities-list
     "C-n" #'activities-new
     "C-r" #'activities-resume
     "C-s" #'activities-suspend)))



(provide 'my-workspace)
;;; my-workspace.el ends here
