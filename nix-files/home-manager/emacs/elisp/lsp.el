(defun sstoltze/lsp-file-relative-name (line-number)
  "Return the current buffer file name, relative to the project root.
    If LINE-NUMBER is given, append the line at point to the file name."
  (if (fboundp 'lsp-workspace-root)
      (format "%s%s"
              (file-relative-name (buffer-file-name) (lsp-workspace-root))
              line-number)))

(defun sstoltze/lsp-yank-relative-name (line-number)
  "Yank the current buffer file name, relative to the project root.
    If prefix argument LINE-NUMBER is given, append the line at point to
    the file name."
  (interactive (list (if (consp current-prefix-arg)
                         (format ":%d" (line-number-at-pos nil t))
                       "")))
  (kill-new (sstoltze/lsp-file-relative-name line-number)))

(defun sstoltze/lsp-run-mix-test (test)
  "Run mix test in the project.

Prefix argument TEST specifies which test to run.
No prefix runs test at point, single prefix runs current file,
double prefix runs all tests."
  (interactive (list (cond ((and (consp current-prefix-arg)
                                 (>= (car current-prefix-arg) 16))
                            "")
                           ((consp current-prefix-arg)
                            (sstoltze/lsp-file-relative-name ""))
                           (t
                            (sstoltze/lsp-file-relative-name (format ":%d" (line-number-at-pos nil t)))))))
  (let ((test-command (format "mix test %s" test)))
    (async-shell-command (format "cd '%s' && %s" (lsp-workspace-root) test-command) "*Mix test*")))

(use-package lsp-mode
  :ensure t
  :hook ((elixir-ts-mode  . lsp-deferred)
         (gleam-ts-mode   . lsp-deferred)
         (nix-mode        . lsp-deferred)
         (lsp-mode        . projectile-mode))
  :bind ((:map lsp-mode-map
               ("M-+"     . lsp-find-references)
               ("M-."     . lsp-find-definition)
               ("C-c l s" . lsp)))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.6)
  ;; Show error diagnostics in the modeline
  (lsp-modeline-diagnostics-enable t)
  (lsp-log-max 10000)
  ;; Recommended for lsp as the replies can get rather large and slow things down - 1 mb
  (read-process-output-max (* 1024 1024))
  (lsp-file-watch-threshold 2000))

;; Trying some things out to speed up LSP/emacs
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\deps\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\assets/vendor\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\priv/static\\'"))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)
