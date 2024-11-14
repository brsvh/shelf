;;; my-workflow.el --- My workflow in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((emacs "30.1") (org "9.7.11") (org-modern "1.5"))
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

;; This file configure my workflows in Emacs, currently focusing solely
;; on Getting Things Done (GTD).

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-modern)
  (require 'window))



;;;
;; Common settings:

(setup org
  (:when-loaded
    (:set
     ;; `org-directory' also set in `my-writing-org', but this file is
     ;; load first, ensure it has correct value anyway.
     org-directory (my-path "~/org")

     ;; Use timestamps to record the duration of a task.
     org-log-done 'time

     ;; Collect everything.
     (append org-agenda-files) (my-path org-directory "inbox.org")

     ;; Collect Events and Meetings.
     (append org-agenda-files) (my-path org-directory "agenda.org")

     ;; Collect tasks of my project.
     (append org-agenda-files) (my-path org-directory "project.org")

     ;; My states of task:
     ;; - TODO, tasks planned for today.
     ;; - NEXT, upcoming tasks for the next few days.
     ;; - WAIT, tasks awaiting updates from others.
     ;; - DONE, tasks already completed.
     ;; - CANC, tasks cancelled.
     ;; - FAIL, tasks abandoned.
     org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|"
                                   "DONE(d)" "CANC(c)" "FAIL(f)")))))



;;;
;; Agenda:

(setup org-agenda
  (:autoload org-agenda)
  (:snoc popper-reference-buffers
         "\\*Org Agenda\\*$")
  (:with-map ctl-c-a-map
    (:keymap-set
     ;; Show agenda.
     "a" org-agenda))
  (:when-loaded
    (:snoc
     org-agenda-custom-commands
     '("c" "Agenda for today"
       ((agenda ""
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-deadline-warning-days 0)))
        (todo "NEXT"
              ((org-agenda-skip-function
                '(org-agenda-skip-entry-if 'deadline))
               (org-agenda-prefix-format "  %i %-12:c [%e] ")
               (org-agenda-overriding-header "\nTasks\n")))
        (agenda nil
                ((org-agenda-entry-types '(:deadline))
                 (org-agenda-format-date "")
                 (org-deadline-warning-days 7)
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                 (org-agenda-overriding-header "\nDeadlines")))
        (tags-todo "inbox"
                   ((org-agenda-prefix-format "  %?-12t% s")
                    (org-agenda-overriding-header "\nInbox\n")))
        (tags "CLOSED>=\"<today>\""
              ((org-agenda-overriding-header
                "\nCompleted today\n"))))))))



;;;
;; Capture:

(setup org-capture
  (:autoload org-capture)
  (:with-map ctl-c-a-map
    (:keymap-set
     ;; Capture everything.
     "c" org-capture))
  (:when-loaded
    (:snoc
     org-capture-templates
     `("e" "Event" entry (file+headline "agenda.org" "Event")
       ,(concat
         "* NEXT %?\n"
         "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n"))
     `("i" "Inbox" entry (file+headline "inbox.org" "Inbox")
       ,(concat
         "* TODO %?\n"))
     `("m" "Meeting" entry (file+headline "agenda.org" "Meeting")
       ,(concat
         "* NEXT %?\n"
         "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n"))
     `("n" "Note" entry (file+headline "inbox.org" "Note")
       ,(concat
         "* TODO %?\n"))
     `("r" "Read" entry (file+headline "inbox.org" "Reading list")
       ,(concat
         "* %?\n")))))

(setup window
  (:set
   (append display-buffer-alist)
   '( "\\*Agenda Commands\\*"
      ;; Prefer to show the select window under the current window.
      (display-buffer-reuse-window display-buffer-below-selected)
      ;; Hide Mode Line.
      (window-parameters (mode-line-format . none))
      ;; Set height to 2/5 of current frame.
      (window-height 0.4))))



;;;
;; UI:

(setup org-modern
  (:autoload org-modern-agenda))

(setup org-agenda
  (:with-hook org-agenda-finalize-hook
    (:hook
     org-modern-agenda))
  (:when-loaded
    (:set
     org-agenda-tags-column 0
     org-agenda-block-separator ?─
     org-agenda-time-grid '((daily today require-timed)
                            (800 1000 1200 1400 1600 1800 2000)
                            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     org-agenda-current-time-string
     "◀── now ─────────────────────────────────────────────────")))



(provide 'my-workflow)
;;; my-workflow.el ends here
