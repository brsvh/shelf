;;; my-writing-org.el --- Org support of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: extensions
;; Package-Requires: ((citar "14.0.6") (citar-embark "1.0") (citar-org-roam "0.5.1") (consult-org-roam "0.1") (emacs "30.1") (embark "1.1") (ob-mermaid "0") (org "9.7.11") (org-modern "1.5") (org-roam "2.2.2") (org-roam-bibtex "0.6.1") (org-side-tree "0.5") (ox-tufte "4.2.0") (valign "3.1.1"))
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

;; This file configures how I use `org-mode'.

;;; Code:

(require 'my-core)

(cl-eval-when (compile)
  (require 'citar)
  (require 'citar-capf)
  (require 'citar-embark)
  (require 'citar-org-roam)
  (require 'consult-org-roam)
  (require 'elec-pair)
  (require 'embark)
  (require 'font-lock)
  (require 'ob-mermaid)
  (require 'oc)
  (require 'oc-basic)
  (require 'oc-bibtex)
  (require 'ol)
  (require 'org)
  (require 'org-capture)
  (require 'org-clock)
  (require 'org-element)
  (require 'org-id)
  (require 'org-indent)
  (require 'org-macs)
  (require 'org-modern)
  (require 'org-persist)
  (require 'org-registry)
  (require 'org-roam)
  (require 'org-roam-bibtex)
  (require 'org-roam-capture)
  (require 'org-roam-db)
  (require 'org-roam-mode)
  (require 'org-roam-node)
  (require 'org-side-tree)
  (require 'ox)
  (require 'ox-publish)
  (require 'ox-tufte)
  (require 'valign)
  (require 'window))

(defun my-org-top-level-min ()
  "Return the minimum point value of top-level text."
  (point-min))

(defun my-org-top-level-max ()
  "Return the maximum point value of top-level text."
  (save-excursion
    (goto-char (my-org-top-level-min))
    (if (re-search-forward org-outline-regexp-bol nil t)
        (pos-bol)
      (point-max))))

(defun my-org-epom-in-top-level-p (epom)
  "Return t if EPOM in top-level region, otherwise nil."
  (org-with-point-at epom
    (let* ((point (point)))
      (and (>= point (my-org-top-level-min))
           (<= point (my-org-top-level-max))))))

(defun my-org-top-level-keyword-name-begin (keyword)
  "Return the position of the beginning of KEYWORD name."
  (save-excursion
    (let ((regexp (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                          (regexp-quote keyword))))
      (goto-char (my-org-top-level-min))
      (when (re-search-forward regexp (my-org-top-level-max) t)
        (match-beginning 1)))))

(defun my-org-top-level-keyword-name-end (keyword)
  "Return the position of the end of KEYWORD name."
  (save-excursion
    (let ((regexp (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                          (regexp-quote keyword))))
      (goto-char (my-org-top-level-min))
      (when (re-search-forward regexp (my-org-top-level-max) t)
        (match-end 1)))))

(defun my-org-top-level-keyword-value-begin (keyword)
  "Return the position of the beginning of KEYWORD value."
  (save-excursion
    (let ((regexp (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                          (regexp-quote keyword))))
      (goto-char (my-org-top-level-min))
      (when (re-search-forward regexp (my-org-top-level-max) t)
        (match-beginning 2)))))

(defun my-org-top-level-keyword-value-end (keyword)
  "Return the position of the end of KEYWORD value."
  (save-excursion
    (let ((regexp (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                          (regexp-quote keyword))))
      (goto-char (my-org-top-level-min))
      (when (re-search-forward regexp (my-org-top-level-max) t)
        (match-end 2)))))

(defun my-org-top-level-last-keyword-end ()
  "Return the position of the end of last keyword."
  (save-excursion
    (let ((regexp "^#\\+\\([^:]+\\):\\s-*\\(.*\\)$"))
      (goto-char (my-org-top-level-max))
      (if (re-search-backward regexp (my-org-top-level-min) t)
          (match-end 2)
        (my-org-top-level-min)))))

(defun my-org-top-level-keyword-p (keyword)
  "Return true if the KEYWORD exists."
  (save-excursion
    (goto-char (my-org-top-level-min))
    (when (re-search-forward (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                                     (regexp-quote keyword))
                             (my-org-top-level-max)
                             t)
      (match-string-no-properties 1))))

(defun my-org-get-top-level-keyword-value (keyword)
  "Return the value of top-level KEYWORD."
  (save-excursion
    (goto-char (my-org-top-level-min))
    (when (re-search-forward (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                                     (regexp-quote keyword))
                             (my-org-top-level-max)
                             t)
      (match-string-no-properties 2))))

(defun my-org-set-top-level-keyword-value (keyword value)
  "Set the value of  top-level KEYWORD to VALUE."
  (save-excursion
    (let* ((regexp (format "^#\\+\\(%s\\):\\s-*\\(.*\\)$"
                           (regexp-quote keyword)))
           (found nil))
      (goto-char (my-org-top-level-min))
      (if (re-search-forward regexp (my-org-top-level-max) t)
          (replace-match (format "#+%s: %s" keyword value) t t)
        (goto-char (my-org-top-level-last-keyword-end))
        (if (= (point) (point-min))
            (insert (format "#+%s: %s\n" keyword value))
          (insert (format "\n#+%s: %s" keyword value)))))))

(defun my/org-put-created-at-at-point (&optional pom)
  "Insert CREATED_AT property for the entry at POM.

POM is an marker, or buffer position."
  (interactive)
  (or pom (setq pom (point)))
  (org-with-point-at pom
    (let ((at (org-entry-get pom "CREATED_AT"))
          (ts (format-time-string "<%Y-%m-%d %a %H:%M>")))
      (unless at
        (org-entry-put nil "CREATED_AT" ts))
      nil)))

(defun my/org-add-top-level-created-at ()
  "Insert CREATED_AT keyword at top-level."
  (interactive)
  (let ((ts (format-time-string "<%Y-%m-%d %a %H:%M>")))
    (unless (my-org-top-level-keyword-p "created_at")
      (my-org-set-top-level-keyword-value "created_at" ts))))

(defun my/org-add-created-at ()
  "Insert CREATED_AT property for all entires."
  (interactive)
  (my/org-add-top-level-created-at)
  (org-map-entries #'my/org-put-created-at-at-point))

(defun my-org-get-id (&optional epom create prefix inherit)
  "Get the ID of the entry at EPOM.

If EPOM is between the beginning and end of top-level, instead of
getting ID keyword.

EPOM is an element, marker, or buffer position.  If EPOM is nil,
refer to the entry at point.

If INHERIT is non-nil, ID properties inherited from parent
entries are considered.  Otherwise, only ID properties on the
entry itself are considered.

When CREATE is nil, return the ID of the entry if found,
otherwise nil.  When CREATE is non-nil, create an ID if none has
been found, and return the new ID.  PREFIX will be passed through
to `org-id-new'."
  (or epom (setq epom (point)))
  (org-with-point-at epom
    (let ((file (or org-id-overriding-file-name
                    (buffer-file-name (buffer-base-buffer))))
          (id (if (my-org-epom-in-top-level-p epom)
                  (my-org-get-top-level-keyword-value "id")
                (org-entry-get epom "ID" (and inherit t)))))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new prefix))
        (if (my-org-epom-in-top-level-p epom)
            (my-org-set-top-level-keyword-value "id" id)
          (org-entry-put epom "ID" id))

        (org-id-add-location id file)
        id)))))

(defun my-org-set-id (value &optional epom)
  "Set the value of ID of the entry at EPOM."
  (or epom (setq epom (point)))
  (org-with-point-at epom
    (let ((file (or org-id-overriding-file-name
                    (buffer-file-name (buffer-base-buffer)))))
      (if (my-org-epom-in-top-level-p epom)
          (my-org-set-top-level-keyword-value "id" value)
        (org-entry-put epom "ID" value))
      (org-id-add-location value file)
      value)))

(defun my/org-create-id (&optional force)
  "Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID."
  (interactive)
  (if force
      (let ((file (or org-id-overriding-file-name
                      (buffer-file-name (buffer-base-buffer))))
            (id (org-id-new)))
        (setq id (org-id-new))
        (my-org-set-id id))
    (my-org-get-id (point) 'create)))

(defun my/org-put-id-at-point (&optional pom)
  "Insert ID property for the entry at POM.

POM is an marker, or buffer position."
  (interactive)
  (or pom (setq pom (point)))
  (org-with-point-at pom
    (let ((file (or org-id-overriding-file-name
                    (buffer-file-name (buffer-base-buffer))))
          (id (org-entry-get nil "ID")))
      (unless (and id (org-uuidgen-p id))
        (setq id (org-id-new))
        (org-entry-put nil "ID" id)
        (org-id-add-location id file))
      id)))

(defun my/org-add-top-level-id ()
  "Insert CREATED_AT keyword at top-level."
  (interactive)
  (let ((file (or org-id-overriding-file-name
                  (buffer-file-name (buffer-base-buffer))))
        (id (my-org-get-top-level-keyword-value "id")))
    (unless (and id (org-uuidgen-p id))
      (setq id (org-id-new))
      (my-org-set-top-level-keyword-value "id" id)
      (org-id-add-location id file))
    id))

(defun my/org-add-id ()
  "Insert ID property for top-level and all entires."
  (interactive)
  (my/org-add-top-level-id)
  (org-map-entries #'my/org-put-id-at-point))

(defun my/org-add-top-level-last-modified ()
  "Insert LAST_MODIFIED keyword at top-level."
  (interactive)
  (let ((ts (format-time-string "<%Y-%m-%d %a %H:%M>")))
    (my-org-set-top-level-keyword-value "last_modified" ts)))

(defun my/org-downcase-keywords ()
  "Ensure use lowercase keywords in a `org-mode' buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (rexp "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)"))
      (while (re-search-forward rexp nil 'noerror)
        (replace-match (downcase (match-string-no-properties 1))
                       'fixedcase nil nil 1)))))

(defun my-org-roam-capture--setup-target-location ()
  "Initialize the buffer, and goto the location of the new capture.
Return the ID of the location."
  (let (p new-file-p)
    (pcase (org-roam-capture--get-target)
      (`(file ,path)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+olp ,path ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m))
       (widen))
      (`(file+head ,path ,head)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+head+olp ,path ,head ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m)))
      (`(file+datetree ,path ,tree-type)
       (setq path (org-roam-capture--target-truepath path))
       (require 'org-datetree)
       (widen)
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (funcall
        (pcase tree-type
          (`week #'org-datetree-find-iso-week-create)
          (`month #'org-datetree-find-month-create)
          (_ #'org-datetree-find-date-create))
        (calendar-gregorian-from-absolute
         (cond
          (org-overriding-default-time
           ;; Use the overriding default time.
           (time-to-days org-overriding-default-time))
          ((org-capture-get :default-time)
           (time-to-days (org-capture-get :default-time)))
          ((org-capture-get :time-prompt)
           ;; Prompt for date.  Bind `org-end-time-was-given' so
           ;; that `org-read-date-analyze' handles the time range
           ;; case and returns `prompt-time' with the start value.
           (let* ((org-time-was-given nil)
                  (org-end-time-was-given nil)
                  (prompt-time (org-read-date
                                nil t nil "Date for tree entry:")))
             (org-capture-put
              :default-time
              (if (or org-time-was-given
                      (= (time-to-days prompt-time) (org-today)))
                  prompt-time
                ;; Use 00:00 when no time is given for another
                ;; date than today?
                (apply #'encode-time 0 0
                       org-extend-today-until
                       (cl-cdddr (decode-time prompt-time)))))
             (time-to-days prompt-time)))
          (t
           ;; Current date, possibly corrected for late night
           ;; workers.
           (org-today)))))
       (setq p (point)))
      (`(node ,title-or-id)
       ;; first try to get ID, then try to get title/alias
       (let ((node (or (org-roam-node-from-id title-or-id)
                       (org-roam-node-from-title-or-alias title-or-id)
                       (user-error "No node with title or id \"%s\"" title-or-id))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node)))))
    ;; Setup `org-id' for the current capture target and return it back to the
    ;; caller.
    (save-excursion
      (goto-char p)
      (if-let ((id (my-org-get-id p)))
          (setf (org-roam-node-id org-roam-capture--node) id)
        (my-org-set-id (org-roam-node-id org-roam-capture--node)))
      (prog1
          (org-id-get)
        (run-hooks 'org-roam-capture-new-node-hook)))))

(defun my-org-setup-save-functions (&rest _)
  "Run `my-org-save-functions'."
  (add-hook 'before-save-hook #'my/org-add-created-at nil t)
  (add-hook 'before-save-hook #'my/org-add-id nil t)
  (add-hook 'before-save-hook #'my/org-add-top-level-last-modified nil t)
  (add-hook 'before-save-hook #'my/org-downcase-keywords nil t))

(defun my-writing-org-roam-db-fake-sync (fn &rest args)
  "Fake db sync around FN with ARGS."
  (cl-letf (((symbol-function #'org-roam-db-sync) #'ignore))
    (apply fn args)))

(defun my-writing-org-roam-db-sync-once (&rest _)
  "Sync `org-roam-db-location' once."
  (advice-remove 'org-roam-db-query #'my-writing-org-roam-db-sync-once)
  (org-roam-db-sync))



;;;
;; Core:

(setup org
  (:autoload org-mode)
  (:set org-directory (my-path "~/org"))
  (:when-loaded
    (:with-hook org-mode-hook
      (:hook
       my-org-setup-save-functions))))

(setup ol
  (:autoload org-store-link)
  (:keymap-set-into ctl-c-a-map "l" org-store-link))



;;;
;; Appearance:

(setup simple
  (:autoload visual-line-mode))

(setup org-side-tree
  (:autoload org-side-tree)
  ;; Show a indented side tree.
  (:with-hook org-side-tree-mode-hook
    (:hook
     org-indent-mode
     visual-line-mode))
  (:when-loaded
    (:set
     ;; FIXME temporarily set it to nil to fix timer errors, but why?
     ;; > Error "Invalid search bound (wrong side of point)"
     org-side-tree-add-overlays nil

     ;; Display trees in right side.
     org-side-tree-display-side 'right

     ;; Always enable folding in Org-Side-Tree buffers.
     org-side-tree-enable-folding t)))

(setup valign
  (:autoload valign-mode))

(setup org
  (:when-loaded
    (:set
     ;; Inhibit alignment of tags.
     org-auto-align-tags nil

     ;; Hide markers.
     org-hide-emphasis-markers t

     ;; Insert new headings after the current subtree.
     org-insert-heading-respect-content t

     ;; Show pretty entities.
     org-pretty-entities t

     ;; Move What I Mean.
     org-special-ctrl-a/e t

     ;; Enable `org-indent-mode' at startup.
     org-startup-indented t

     ;; Insert tags after heading.
     org-tags-column 0)
    (:with-map org-mode-map
      (:keymap-set
       ;; Show a side tree as outline.
       "C-c m t" org-side-tree))))

(setup org-modern
  (:autoload org-modern-mode)
  (:with-hook org-mode-hook
    (:hook org-modern-mode))
  (:when-loaded
    (:set
     org-modern-star '( "●" "◉" "◎" "○")
     org-modern-replace-stars '( "●" "◉" "◎" "○")
     ;; Prefer to use `valign'.
     org-modern-table nil))
  ;; Align variable-pitch font, CJK characters and images in tables.
  (:with-hook org-modern-mode-hook
    (:hook valign-mode)))

(setup window
  (:set
   (append display-buffer-alist)
   '( (major-mode . org-side-tree-mode)
      (display-buffer-reuse-mode-window display-buffer-in-side-window)
      (side . right)
      (window-parameters . ( (mode-line-format . none)
                             (no-delete-other-window . t)
                             (no-other-window . t))))
   (append display-buffer-alist)
   '( "\\*Org todo\\*"
      ;; Prefer to show the select window under the current window.
      (display-buffer-reuse-window display-buffer-below-selected)
      ;; Hide Mode Line.
      (window-parameters . ( (mode-line-format . none)))
      ;; Set height to 1/3 of current frame.
      (window-height . 0.33))))

(setup font-lock
  (:when-loaded
    (font-lock-add-keywords
     'org-mode
     '( ("[[:multibyte:]]\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?[[:multibyte:]]?"
         (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
        ("[[:multibyte:]]?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)[[:multibyte:]]"
         (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
     'append)))



;;;
;; Babel:

(setup org
  (:when-loaded
    (org-babel-do-load-languages 'org-babel-load-languages
                                 (list (cons 'mermaid t)))))



;;;
;; Capture:

(setup window
  (:set
   (append display-buffer-alist)
   '( "\\*Org Select\\*"
      ;; Prefer to show the select window under the current window.
      (display-buffer-reuse-window display-buffer-below-selected)
      ;; Hide Mode Line.
      (window-parameters . ( (mode-line-format . none)))
      ;; Set height to 2/5 of current frame.
      (window-height . 0.4))))



;;;
;; Citation:

(setup citar-capf
  (:autoload citar-capf-setup))

(setup citar-embark
  (:autoload citar-embark-mode))

(setup citar-org-roam
  (:autoload citar-org-roam-mode))

(setup embark
  (:autoload embark-act))

(setup citar
  (:when-loaded
    (:set
     citar-at-point-function 'embark-act)))

(setup org
  (:also-load oc oc-basic oc-bibtex)
  (:with-hook org-mode-hook
    (:hook citar-capf-setup))
  (:when-loaded
    (:after embark
      (citar-embark-mode +1))))

(setup oc
  (:autoload org-cite-insert)
  (:when-loaded
    (:set
     ;; Use `citar' as default processor
     org-cite-activate-processor 'citar
     org-cite-follow-processor 'citar
     org-cite-insert-processor 'citar))
  (:after org
    (:with-map org-mode-map
      (:keymap-set
       "C-c c i" org-cite-insert))))



;;;
;; Clock:

(setup org-clock
  (:autoload org-clock-kill-emacs-query)
  (:when-loaded
    (:set
     org-clock-persist-file (my-data-path "org/presist" "clock.el"))))



;;;
;; Editing:

(setup org
  (:with-hook org-mode-hook
    (:hook
     ;; Ensure `tab-width' is 8.
     (lambda nil
       (setq-local tab-width 8))
     ;; Auto insert paried '*', '/', '=', and '~'.
     (lambda ()
       (electric-pair-local-mode +1)
       (:snoc-local electric-pair-pairs
                    (cons ?* ?*)
                    (cons ?/ ?/)
                    (cons ?= ?=)
                    (cons ?~ ?~))))))



;;;
;; Export:

(setup ox
  (:also-load ox-tufte)
  (:when-loaded
    (:set
     (append org-export-filter-paragraph-functions)
     (lambda (text _backend _info)
       (let* ((text (replace-regexp-in-string
                     "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)"
                     "\\1\\2"
                     text))
              (text (replace-regexp-in-string
                     "\\([[:multibyte:]]\\) \\(.*?\\) \\([[:multibyte:]]\\)"
                     "\\1\\2\\3"
                     text))
              (text (replace-regexp-in-string
                     "\\([[:multibyte:]]\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\([[:multibyte:]]\\)"
                     "\\1 \\2 \\3"
                     text)))
         text)))))



;;;
;; id:

(setup org-id
  (:when-loaded
    (:advice-add org-id-get :override my-org-get-id)
    (:advice-add org-id-get-create :override my/org-create-id)
    (:set
     org-id-locations-file (my-data-path "org/" "id-locations.el")
     ;; Link to entry with ID.
     org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))



;;;
;; Persist:

(setup org-persist
  (:when-loaded
    (:set
     org-persist-directory (my-data-path "org/persist/"))))



;;;
;; Publish:

(setup ox-publish
  (:when-loaded
    (:set
     org-publish-timestamp-directory (my-data-path "org/timestamps/"))))



;;;
;; Registry:

(setup org-registry
  (:when-loaded
    (:set
     org-registry-file (my-data-path "org/"  "registry.el"))))



;; Roam:

(setup consult-org-roam
  (:autoload consult-org-roam-mode))

(setup org-roam-bibtex
  (:autoload org-roam-bibtex-mode))

(setup org-roam-capture
  (:autoload org-roam-capture))

(setup org-roam-db
  (:autoload
   org-roam-db-autosync-enable
   org-roam-db-query))

(setup org-roam-mode
  (:autoload
   org-roam-mode
   org-roam-buffer-toggle))

(setup org-roam-node
  (:autoload
   org-roam-node-find
   org-roam-node-insert))

(setup org-roam
  (:set
   org-roam-directory (my-path* org-directory "roam/"))
  (:with-map ctl-c-a-map
    (:keymap-set
     ;; Capture a note.
     "n" org-roam-capture))
  (:after org
    (:with-map org-mode-map
      (:keymap-set
       "C-c r f" org-roam-node-find
       "C-c r i" org-roam-node-insert
       "C-c r b" org-roam-buffer-toggle))
    (citar-org-roam-mode +1))
  ;; Separate store Org Roam citations.
  (:after oc
    (:set
     (append org-cite-global-bibliography)
     (my-path org-roam-directory "citations.bib")))
  (:after citar
    (:set
     (append citar-bibliography)
     (my-path org-roam-directory "citations.bib")))
  (:with-mode org-roam-mode
    (:hook consult-org-roam-mode)))

(setup org-roam-capture
  (:when-loaded
    (:advice-add
     org-roam-capture--setup-target-location
     :override
     my-org-roam-capture--setup-target-location)))

(setup org-roam-db
  (:set org-roam-db-location (my-path org-roam-directory "roam.db"))
  (:advice-add
   ;; Don't immediately initialize Org Roam database when enable
   ;; `org-roam-db-autosync-mode'.
   org-roam-db-autosync-enable :around my-writing-org-roam-db-fake-sync
   ;; Sync `org-roam' db when first query.
   org-roam-db-query :before my-writing-org-roam-db-sync-once)
  (:after org
    (org-roam-db-autosync-enable)))

(setup org-roam-mode
  (:set
   ;; Show back links, reference links, unreference links in Org Roam
   ;; buffer.
   org-roam-mode-sections
   '( (org-roam-backlinks-section :unique t)
      org-roam-reflinks-section
      org-roam-unlinked-references-section)

   ;; Display the Org Roam buffer in the right window.
   (append display-buffer-alist)
   '( "\\*org-roam\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . 1)
      (window-width . 0.33)
      (window-parameters . ( (no-other-window . t)
                             (no-delete-other-windows . t)))))
  (:with-hook org-roam-mode-hook
    ;; Show citations.
    (:hook org-roam-bibtex-mode)))


(provide 'my-writing-org)
;;; my-writing-org.el ends here
