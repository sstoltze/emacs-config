(use-package projectile
  :ensure t
  :defer t
  :bind-keymap (("C-c p" . projectile-command-map))
  :bind ((:map projectile-command-map
               ("y"       . sstoltze/lsp-yank-relative-name)))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-use-git-grep      t)
  :config
  (when (eq system-type 'windows-nt)
    ;; Seems to be necessary for windows
    (setq projectile-git-submodule-command nil
          projectile-indexing-method       'alien)))

(use-package counsel-projectile
  :ensure t
  :after (:all counsel projectile)
  :custom
  (counsel-projectile-sort-files   t)
  (counsel-projectile-sort-buffers t)
  :init
  (counsel-projectile-mode 1))
