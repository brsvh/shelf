{
  emacs30-nox,
  emacsPackagesFor,
  runtimeShell,
  writeScriptBin,
  ...
}:
let
  emacs = (emacsPackagesFor emacs30-nox).withPackages (epkgs: [
    epkgs.my-emacs
  ]);
in
writeScriptBin "my-emacs-formatter" ''
  #!${runtimeShell}
  ":"; exec ${emacs}/bin/emacs --no-init-file --script "$0" -- "$@"

  (require 'my-core)
  (require 'my-editor)
  (require 'my-programming)
  (require 'my-programming-emacs-lisp)

  (defun emacs-lisp-format-file (file)
    "Format the Emacs Lisp FILE."
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (let ((buffer (current-buffer))
              (inhibit-message t)
              (message-log-max nil))
          (cl-labels ((lget (var)
                        (buffer-local-value var buffer)))
            (setq-local indent-line-function (lget 'indent-line-function)
                        indent-tabs-mode (lget 'indent-tabs-mode)
                        lisp-indent-function (lget 'lisp-indent-function))
            (goto-char (point-min))
            (indent-region (point-min) (point-max))
            (write-file file))))))

  (mapcar #'emacs-lisp-format-file (cdr command-line-args-left))
''
