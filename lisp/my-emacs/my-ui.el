;;; my-ui.el --- UI enhancements of My Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Burgess Chang

;; Author: Burgess Chang <bsc@brsvh.org>
;; Keywords: local
;; Package-Requires: ((consult "1.8") (doom-modeline "4.2.0") (emacs "30.1") (embark "1.1") (embark-consult "1.1") (ibuffer-project "2.2") (imenu-list "0.9") (marginalia "1.7") (modus-themes "4.5.0") (nerd-icons "0.1.0") (nerd-icons-ibuffer "1.0.0") (orderless "1.2") (popper "0.4.6") (sideline "0.2.0") (spacious-padding "0.5.0") (svg-tag-mode "0.3.2") (switch-window "1.6.1") (treemacs "3.1") (treemacs-nerd-icons "0.0.1") (vertico "1.9"))
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

;; This file provides enhancements to the user interface for my-emacs,
;; including improvements to the appearance, interaction, and default
;; behaviors of Emacs.

;;; Code:

(require 'consult)
(require 'my-core)
(require 'orderless)

(cl-eval-when (compile)
  (require 'consult)
  (require 'consult-info)
  (require 'doom-modeline)
  (require 'embark)
  (require 'embark-consult)
  (require 'help-fns)
  (require 'ibuffer)
  (require 'ibuffer-project)
  (require 'imenu-list)
  (require 'marginalia)
  (require 'modus-themes)
  (require 'nerd-icons)
  (require 'nerd-icons-ibuffer)
  (require 'popper)
  (require 'popper-echo)
  (require 'savehist)
  (require 'scroll-bar)
  (require 'sideline)
  (require 'spacious-padding)
  (require 'svg-tag-mode)
  (require 'switch-window)
  (require 'tab-bar)
  (require 'tab-line)
  (require 'treemacs)
  (require 'treemacs-customization)
  (require 'treemacs-nerd-icons)
  (require 'vertico)
  (require 'vertico-directory)
  (require 'window)
  (require 'winner))

(defun my-check-graphic-by-window-system (func &optional display)
  "Check `initial-window-system' when no DISPLAY bt wrap the FUNC."
  (if display
      (funcall func display)
    initial-window-system))

(defun my-imenu-list-display-buffer (buffer alist)
  "Display the `imenu-list' buffer at the side.
This function should be used with `display-buffer-alist'.
See `display-buffer-alist' for a description of BUFFER and ALIST."
  (or (get-buffer-window buffer)
      (let ((window (ignore-errors
                      (split-window (frame-root-window)
                                    (imenu-list-split-size)
                                    imenu-list-position))))
        (when window
          (window--display-buffer buffer window 'window alist)
          (set-window-dedicated-p window t)
          (set-window-parameter window 'no-other-window t)
          (set-window-parameter window 'no-delete-other-window t)
          window))))

(defun my-modus-themes-enable-p (&rest _)
  "Return non-nil if current theme is belong to Modus Themes, else nil."
  (cl-some (lambda (theme)
             (member theme '(modus-operandi
                             modus-operandi-tinted
                             modus-vivendi
                             modus-vivendi-tinted)))
           custom-enabled-themes))

(defun my-orderless-consult-suffix ()
  "Regexp which matches the end of string with Consult tofu support."
  (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
      (format "[%c-%c]*$"
              consult--tofu-char
              (+ consult--tofu-char consult--tofu-range -1))
    "$"))

;; Recognizes the following patterns:
;;
;; * .ext (file extension)
;; * regexp$ (regexp matching at end)
;;
;; See:
;;
;;  https://github.com/minad/consult/wiki#minads-orderless-configuration
(defun my-orderless-file-extension-dispatch (word _index _total)
  "WORD must be a string, _INDEX and the _TOTAL number of components."
  (cond
   ;; Ensure that $ works with Consult commands, which add
   ;; disambiguation suffixes.
   ((string-suffix-p "$" word)
    `(orderless-regexp
      .
      ,(concat (substring word 0 -1) (my-orderless-consult-suffix))))
   ;; File extensions
   ((and (or minibuffer-completing-file-name
             (derived-mode-p 'eshell-mode))
         (string-match-p "\\`\\.." word)
         `(orderless-regexp
           .
           ,(concat "\\."
                    (substring word 1)
                    (my-orderless-consult-suffix)))))))

(defun my-popper-fit-window-height (win)
  "Determine the height of WIN by fitting it to the buffer's content."
  (fit-window-to-buffer
   win
   (floor (frame-height) 2.5)
   (floor (frame-height) 2.5)))

(defun my-popper-tab-line--format (tab tabs)
  "Format of TAB & TABS in popper tab-line."
  (let ((name (tab-line-tab-name-format-default tab tabs))
        (idx (cl-position tab tabs)))
    (propertize
     (format " %s %s" (char-to-string (+ idx #x0031)) name)
     'face (if (eq tab (current-buffer))
               (if (mode-line-window-selected-p)
                   'tab-line-tab-current
                 'tab-line-tab-inactive)
             'tab-line-tab-inactive))))

;; TODO support to match buffer name with a regular expression pattern.
(defvar my-switch-window-ignore-rules nil
  "A list of rules to ignore windows during selection.
Each rule can be:
- A string: Match window's buffer name exactly.
- A symbol: Match buffer's `major-mode`.")

(defun my-switch-window--ignore-p (window)
  "Return t if WINDOW matches any rule in `my-switch-window-ignore-rules'."
  (let* ((rules my-switch-window-ignore-rules)
         (buffer (window-buffer window))
         (name (buffer-name buffer))
         (mode (buffer-local-value 'major-mode buffer)))
    (or
     (memq mode rules)
     (member name rules))))

(defun my-switch-window--list-filter (windows)
  "Filter out all WINDOWS that should be ignored."
  (cl-remove-if 'my-switch-window--ignore-p
                windows))

(defun my-tab-bar-local-buffer-p (buffer)
  "Return whether BUFFER is in the buffres of current tab."
  (memq buffer (frame-parameter nil 'buffer-list)))

(defun my-theme-is-modus (&rest _)
  "Return non-nil if current theme is belong to Modus Themes, else nil."
  (member my-theme '(modus-operandi
                     modus-operandi-tinted
                     modus-vivendi
                     modus-vivendi-tinted)))

(defvar consult--source-tab-buffer
  `( :name     "Tab buffer"
     :narrow   ?b
     :history  buffer-name-history
     :category buffer
     :face     consult-buffer
     :state    consult--buffer-state
     :default  t
     :enabled  ,(lambda () tab-bar-mode)
     :items    ,(lambda ()
                  (consult--buffer-query
                   :predicate (lambda (buffer)
                                (memq buffer
                                      (frame-parameter nil 'buffer-list)))
                   :sort 'visibility
                   :as #'consult--buffer-pair))))

(orderless-define-completion-style my-orderless-with-initialism
  (orderless-matching-styles '(orderless-initialism
                               orderless-literal
                               orderless-regexp)))

(defvar ctl-c-b-t-map (make-keymap)
  "Default keymap use to bind my treemacs commands.")

(setup my-maps
  (keymap-set ctl-c-b-map "t" ctl-c-b-t-map))



;;;
;; Action:

(setup embark
  (:snoc popper-reference-buffers
         "\\`\\*Embark Collect \\(Live\\|Completions\\)\\*")
  (:with-map global-map
    (:keymap-set
     "C-." embark-act
     "C-;" embark-dwim
     "C-h B" embark-bindings))
  (:when-loaded
    (:set
     ;; Prefer to pop up Embark buffer below the current window.
     embark-verbose-indicator-display-action
     '(display-buffer-reuse-window display-buffer-below-selected)))
  (:after winner
    ;; Ignore Embark window when redo or undo.
    (:snoc winner-boring-buffers
           "*Embark Collect Live*"
           "*Embark Collect Compiletions*")))

(setup embark-consult
  (:after embark
    (:with-hook embark-collect-mode-hook
      (:hook consult-preview-at-point-mode))))



;;;
;; Buffer:

(setup ibuffer-project
  (:autoload
   ibuffer-project-generate-filter-groups
   ibuffer-do-sort-by-project-file-relative))

(setup nerd-icons-ibuffer
  (:autoload nerd-icons-ibuffer-mode)
  (:when-gui
   (:when-loaded
     (:set
      nerd-icons-ibuffer-icon t        ;; Enable icons.
      nerd-icons-ibuffer-color-icon t  ;; Colorful icons.
      nerd-icons-ibuffer-icon-size 1.0 ;; Set icon size.
      nerd-icons-ibuffer-human-readable-size t))))

(setup ibuffer
  (:autoload ibuffer)
  (:after winner
    (:snoc winner-boring-buffers
           "*Ibuffer*"))
  (:with-hook ibuffer-mode-hook
    (:hook
     nerd-icons-ibuffer-mode
     (lambda ()
       (:set ibuffer-filter-groups
             (ibuffer-project-generate-filter-groups))
       (unless (eq ibuffer-sorting-mode
                   'project-file-relative)
         (ibuffer-do-sort-by-project-file-relative))))))

(setup buff-menu
  (:keymap-set-into global-map "<remap> <list-buffers>" ibuffer))

(setup buffer
  (:with-map global-map
    (:keymap-set
     ;; Select a buffer open in current window.
     "<remap> <switch-to-buffer>" consult-buffer)))

(setup sideline
  (:autoload global-sideline-mode sideline-mode)
  (:first-buffer global-sideline-mode))

(setup uniquify
  (:when-loaded
    (:set
     ;; Use `forward' style.
     ;;
     ;; the files ‘/foo/bar/mumble/name’ and ‘/baz/quux/mumble/name’
     ;; will have follow name.
     ;;
     ;;  bar/mumble/name quux/mumble/name
     uniquify-buffer-name-style 'forward)))



;;;
;; Core:

(setup emacs
  (:set
   ;; Inhibit using dialog boxes.
   use-dialog-box nil))



;;;
;; File system:

(setup treemacs-filewatch-mode
  (:autoload treemacs-filewatch-mode))

(setup treemacs-follow-mode
  (:autoload treemacs-follow-mode))

(setup treemacs-customization
  (:when-loaded
    (:set
     ;; Display a more compact indentation.
     treemacs-indentation 1

     ;; Inhibit select to treemacs buffer.
     treemacs-is-never-other-window t

     ;; Show hidden files (.*).
     treemacs-show-hidden-files t

     ;; Redirect persist files.
     treemacs-persist-file (my-data-path "treemacs/state")
     treemacs-last-error-persist-file (my-data-path "treemacs/error")

     ;; Show at left.
     treemacs-position 'left

     ;; Keep treemacs silent.
     treemacs-silent-refresh t
     treemacs-silent-filewatch t)))

(setup treemacs
  (:autoload
   treemacs
   treemacs-bookmark
   treemacs-find-file
   treemacs-select-directory
   treemacs-select-window)
  (:set (append my-switch-window-ignore-rules) 'treemacs-mode)
  (:with-map ctl-c-b-t-map
    (:keymap-set
     "b" treemacs-bookmark
     "d" treemacs-select-directory
     "f" treemacs-find-file
     "o" treemacs
     "s" treemacs-select-window))
  (:when-loaded
    (:also-load treemacs-nerd-icons)
    ;; Prefer to use nerd-icons theme.
    (treemacs-load-theme "nerd-icons")

    ;; Watch the filesystem changes.
    (treemacs-filewatch-mode +1)

    ;; Follow the current file.
    (treemacs-follow-mode +1)))

(setup window
  (:set
   (append display-buffer-alist)
   '( "Treemacs"
      (display-buffer-reuse-mode-window display-buffer-in-side-window)
      (side . left)
      (slot . -1)
      (window-parameters . ( (mode-line-format . none)
                             (no-delete-other-window . t)
                             (no-other-window . t))))))



;;;
;; Frame:

(setup menu-bar
  ;; Ensure Menu Bar is disabled.
  (menu-bar-mode -1))

(setup tool-bar
  ;; Ensure Tool Bar is disabled.
  (tool-bar-mode -1))

(setup frame
  (:advice-add
   display-graphic-p :around my-check-graphic-by-window-system)
  (:set
   ;; Resize frame pixel by pixel.
   frame-resize-pixelwise t)
  (:with-map global-map
    (:keymap-set
     ;; Select a buffer open in a new frame.
     "<remap> <switch-to-buffer-other-frame>" consult-buffer-other-frame)))

;; Set more padding in the Frame.
(setup spacious-padding
  (:autoload spacious-padding-mode)
  (:first-ui spacious-padding-mode)
  (:when-loaded
    (:set
     spacious-padding-subtle-mode-line t)))



;;;
;; Help:

(setup help-fns
  (:autoload describe-keymap))

(setup help
  (:set
   ;; Better which-key.
   prefix-help-command #'embark-prefix-help-command)
  (:with-map help-map
    (:keymap-set
     "C-k" describe-keymap)))

(setup info
  (:with-map global-map
    (:keymap-set
     "<remap> <info>" consult-info)))



;;;
;; History:

(setup savehist
  ;; Save our history.
  (:first-ui savehist-mode)
  (:set
   ;; Drop duplicated history.
   history-delete-duplicates t)
  (:when-loaded
    (:set
     savehist-file (my-state-path "history.el"))))



;;;
;; Icons:

(setup nerd-icons
  (:when-loaded
    (:snoc
     nerd-icons-mode-icon-alist
     '( eat-mode nerd-icons-devicon "nf-dev-terminal"))))



;;;
;; Index:

(setup imenu-list
  (:autoload imenu-list-smart-toggle)
  (:set
   (append my-switch-window-ignore-rules)
   'imenu-list-major-mode)
  (:when-loaded
    (:advice-add
     imenu-list-display-buffer :override my-imenu-list-display-buffer)
    (:set
     imenu-list-mode-line-format nil
     imenu-list-position 'right
     imenu-list-size 30))
  (:keymap-set-into ctl-c-b-map "l" imenu-list-smart-toggle))



;;;
;; Minibuffer:

(setup marginalia
  (:first-ui marginalia-mode)
  (:when-loaded
    ;; Show marginalia at right.
    (:set marginalia-align 'right)))

(setup minibuffer
  (:set
   ;; Allow nested minibuffer.
   enable-recursive-minibuffers t

   ;; Use compiletion UI when complete in Minibuffer.
   completion-in-region-function #'consult-completion-in-region

   ;; Preferred completion styles:
   ;;
   ;; * substring:          bar    -> foo-bar-baz
   ;; * orderless:          f b b  -> foo-bar-baz
   ;; * basic:              fo     -> foo-bar-baz
   ;; * partial-completion: -bar-b -> foo-bar-baz
   completion-styles
   '(substring orderless basic partial-completion)

   ;; Provide abbreviation completion when intending to complete text,
   ;; commands, variables, and symbols.
   completion-category-overrides
   '( (file (styles basic partial-completion))
      (command (styles my-orderless-with-initialism))
      (variable (styles my-orderless-with-initialism)
                (symbol (styles my-orderless-with-initialism))))))

(setup orderless
  (:when-loaded
    (:set
     ;; Allow escape with blackslash.
     orderless-component-separator #'orderless-escapable-split-on-space

     ;; Support extensions dispatcher.
     orderless-style-dispatchers '(my-orderless-file-extension-dispatch
                                   orderless-affix-dispatch))))

(setup vertico
  (:first-ui vertico-mode)
  (:when-loaded
    (:set
     ;; Resize the Vertico Buffer size when the number of candidates
     ;; changes.
     vertico-resize t

     ;; Return to the top when reaching the bottom of the candidates.
     vertico-cycle t)))

(setup vertico-directory
  (:after vertico
    (:with-map vertico-map
      (:keymap-set
       "<return>" vertico-directory-enter

       ;; Eliminating multiple characters in the path simultaneously by
       ;; word.
       "<backspace>" vertico-directory-delete-char
       "M-<backspace>" vertico-directory-delete-word))))



;;;
;; Mouse:

;; TODO add GUI and TUI mouse customization.



;;;
;; Mode Line:

(setup doom-modeline
  (:first-ui doom-modeline-mode)
  (:when-loaded
    (:set
     ;; Disable Bar.
     doom-modeline-bar-width 0

     ;; Let width of segments always less than 80.
     doom-modeline-window-width-limit 80

     ;; Activate icon according whether Emacs session is GUI.
     doom-modeline-icon (display-graphic-p)

     ;; Modified icon is ugly :(
     doom-modeline-buffer-state-icon nil

     ;; Enable word count.
     doom-modeline-enable-word-count t)))



;;;
;; Popup:

(setup popper-echo
  (:autoload popper-echo-mode popper-tab-line-mode)
  (:when-loaded
    (:set
     popper-echo-dispatch-keys
     '( "M-1" "M-2" "M-3" "M-4" "M-5"
        "M-6" "M-7" "M-8" "M-9" "M-0"))
    (:advice-add
     popper-tab-line--format :override my-popper-tab-line--format)))

(setup popper
  (:first-ui popper-mode popper-tab-line-mode)
  (:snoc popper-reference-buffers
         "\\*Backtrace\\*"
         "\\*Compile-Log\\*"
         "\\*Error\\*"
         "\\*Help\\*"
         "\\*Warnings\\*")
  (:with-map global-map
    (:keymap-set
     "C-`" popper-toggle
     "C-~" popper-cycle
     "M-p" popper-toggle-type))
  (:when-loaded
    (:set
     popper-group-function #'popper-group-by-directory
     popper-window-height #'my-popper-fit-window-height)
    (:after doom-modeline
      (:advice-add
       popper--modified-mode-line
       :override
       (lambda nil
         (when popper-mode-line
           (let ((mode-line-format
                  '( "%e" ( :eval (doom-modeline-format--minimal)))))
             (if (member popper-mode-line mode-line-format)
                 mode-line-format
               (append (cl-subseq mode-line-format
                                  0
                                  popper-mode-line-position)
                       (list
                        popper-mode-line
                        (nthcdr popper-mode-line-position
                                mode-line-format))))))))

      ;; Replace " POP " in `doom-modeline' with .
      (:set
       popper-mode-line
       '(:eval (let* ((face (if (doom-modeline--active)
                                'doom-modeline-emphasis
                              'mode-line-inactive)))
                 (if (and (bound-and-true-p doom-modeline-icon)
                          (bound-and-true-p doom-modeline-mode))
                     (format " %s "
                             (nerd-icons-octicon "nf-oct-pin" :face face))
                   (propertize " POP " 'face face))))))))



;;;
;; Scrolling:

(setup scroll-bar
  (:when-gui
   (scroll-bar-mode -1)))

(setup scroll
  (:set
   ;; Let Emacs prioritizes speed over precise scrolling.
   fast-but-imprecise-scrolling t

   ;; Inhibit automatically adjust the window's vertical position to
   ;; keep point centered vertically.
   auto-window-vscroll nil

   ;; Keep the cursor at the same screen position when scrolling.
   scroll-preserve-screen-position t

   ;; Let the cursor can move directly to the top or bottom edge of the
   ;; window.
   scroll-margin 0

   ;; Smoother and less disruptive scrolling, nerver recenters the point
   ;; when it moves off-screen.
   scroll-conservatively 101

   ;; Let the cursor can move directly to the left or right edge of the
   ;; window.
   hscroll-margin 2

   ;; Smoother and less disruptive horizontal scrolling.
   hscroll-step 1))



;;;
;; Startup:

(setup startup
  (:set
   ;; Inhibit *GNU Emacs* buffer.
   inhibit-startup-screen t

   ;; Inhibit startup message in echo area (minibuffer):
   ;;  For information about GNU Emacs and the GNU system, type C-h C-a.
   inhibit-startup-echo-area-message t

   ;; Inhibit content in *scratch* buffer.
   initial-scratch-message nil

   ;; Open *scratch* buffer with `fundamental-mode'.
   initial-major-mode 'fundamental-mode)
  (:advice-add
   display-startup-echo-area-message :override ignore
   display-startup-screen            :override ignore))



;;;
;; Tab:

(setup tab-bar
  (:first-ui tab-bar-mode)
  (:with-map global-map
    (:keymap-set
     "<remap> <switch-to-buffer-other-tab>" consult-buffer-other-tab))
  (:with-hook tab-bar-mode-hook
    (:hook
     (lambda ()
       (if (bound-and-true-p tab-bar-mode)
           ;; Prefer to use `consult--source-tab-buffer' when
           ;; `tab-bar-mode' is enabled.
           (progn
             ;; Hide default source `consult--source-buffer'.
             (consult-customize consult--source-buffer
                                :hidden t
                                :default nil)
             (:set
              ;; Use buffer of current Tab as default source.
              (prepend consult-buffer-sources)
              'consult--source-tab-buffer))
         ;; Unset `consult--source-tab-buffer' when `tab-bar-mode' is
         ;; disabled.
         (progn
           (consult-customize consult--source-buffer
                              :hidden nil
                              :default t)
           (:set (remove consult-buffer-sources)
                 'consult--source-tab-buffer))))))
  (:when-loaded
    (:set
     ;; Only show Tab Bar when have one more Tabs.
     tab-bar-show 1

     ;; Switch to *scratch* buffer by default.
     tab-bar-new-tab-choice "*scratch*")))



;;;
;; Theme:

(setup my-themes
  (:set
   my-theme 'modus-operandi
   my-light-theme 'modus-operandi
   my-dark-theme 'modus-vivendi)
  (:first-ui my-theme-setup))

(setup modus-themes
  (:only-if (my-theme-is-modus))
  (:set
   ;; Reload modus themes when its option's values is changed.
   modus-themes-custom-auto-reload t)

  (:when-loaded
    (:set
     ;; Use bold and italic for code syntax highlighting.
     modus-themes-bold-constructs t
     modus-themes-italic-constructs t

     ;; Use `fixed-pitch' face for Org tables and code blocks.
     modus-themes-mixed-fonts t

     ;; Use bold prompts.
     modus-themes-prompts '(bold)

     ;; Change boldness of completion faces.
     modus-themes-completions '( (matches . (extrabold))
                                 (selection . (semibold
                                               fitalic
                                               text-also)))

     ;; Set different font sizes for headings of various levels.
     modus-themes-headings '( (0 . (1.40 ultrabold))
                              (1 . (1.30 extrabold))
                              (2 . (1.20 heavy))
                              (3 . (1.10 bold))
                              (t . (1.05 semibold))))
    (:face internal-border ((t (:inherit fringe))))
    ;; Override the default faces of modus themes.
    (:snoc modus-themes-common-palette-overrides
           ;; Make `tab-bar' more subtle.
           '(bg-tab-bar bg-active)
           '(bg-tab-current bg-main)
           '(bg-tab-other bg-inactive)
           ;; Make Mode Line borderless.
           '(border-mode-line-active unspecified)
           '(border-mode-line-inactive unspecified))))



;;;
;; Window:

(setup switch-window
  (:when-loaded
    (:advice-add
     switch-window--list
     :filter-return
     my-switch-window--list-filter)))

(setup window
  (:set
   ;; Resize window pixel by pixel.
   window-resize-pixelwise t)
  (:with-map global-map
    (:keymap-set
     ;; When there are more than two windows, select the window to
     ;; switch to by number.
     "<remap> <other-window>" switch-window

     ;; When there are re than two windows, select the window to
     ;; maximize to by number.
     "<remap> <delete-other-windows>" switch-window-then-maximize

     ;; When there are more than two windows, split the window below by
     ;; selecting its number.
     "<remap> <split-window-below>" switch-window-then-split-below

     ;; When there are more than two windows, split the window right by
     ;; selecting its number.
     "<remap> <split-window-right>" switch-window-then-split-right

     ;; Select a buffer open in other window.
     "<remap> <switch-to-buffer-other-window>" consult-buffer-other-window)))

(setup winner
  (:first-ui winner-mode)
  (:with-map ctl-c-map
    (:keymap-set
     "<left>" winner-undo
     "<right>" winner-redo))
  (:when-loaded
    (:snoc winner-boring-buffers
           "*Backtrace*"
           "*Compile-Log*"
           "*Error*"
           "*Help*"
           "*Warnings*")))



(provide 'my-ui)
;;; my-ui.el ends here
