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
