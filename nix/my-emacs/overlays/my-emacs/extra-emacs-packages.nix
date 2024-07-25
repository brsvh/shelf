# NOTE: All packages related to magit should use the versions available
#       in MELPA.
epkgs:
with epkgs;
(with elpaPackages; [
  activities
  auctex
  cl-lib
  compat
  consult
  dash
  diff-hl
  dired-git-info
  eglot
  eldoc
  embark
  embark-consult
  external-completion
  flymake
  gcmh
  jsonrpc
  let-alist
  lv
  marginalia
  orderless
  org
  org-modern
  persist
  popper
  project
  queue
  rainbow-mode
  seq
  setup
  svg-lib
  transient
  vertico
  xref
  yasnippet
])
++ (with manualPackages.my; [
  eglot-booster
  form-feed
  on
  sideline-eldoc
])
++ (with melpaPackages; [
  aio
  apheleia
  benchmark-init
  biblio
  biblio-core
  bibtex-completion
  citar
  citar-embark
  citar-org-roam
  citeproc
  company
  consult-org-roam
  diredfl
  docker
  doom-modeline
  eldoc-box
  emacsql
  embark-org-roam
  envrc
  f
  flymake-eslint
  git-cliff
  git-commit
  git-modes
  grip-mode
  hl-todo
  ht
  ibuffer-project
  inheritenv
  lsp-mode
  magit
  magit-section
  mermaid-mode
  modus-themes
  mwim
  nerd-icons
  nerd-icons-dired
  nerd-icons-ibuffer
  nix-ts-mode
  ob-mermaid
  org-category-capture
  org-project-capture
  org-roam
  org-roam-bibtex
  org-side-tree
  ox-reveal
  ox-tufte
  pangu-spacing
  parsebib
  pdf-tools
  rainbow-delimiters
  rg
  s
  shrink-path
  sideline
  sideline-flymake
  sideline-lsp
  simple-httpd
  smartparens
  string-inflection
  svg-tag-mode
  switch-window
  tabspaces
  valign
])
++ (with nongnuPackages; [
  anzu
  eat
  edit-indirect
  editorconfig
  geiser
  geiser-guile
  geiser-mit
  geiser-racket
  haskell-mode
  hl-block-mode
  htmlize
  markdown-mode
  org-contrib
  tablist
  web-mode
  wgrep
  with-editor
  yasnippet-snippets
])
