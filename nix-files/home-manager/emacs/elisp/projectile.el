(use-package projectile
  :ensure t
  :defer t
  :bind-keymap (("C-c p" . projectile-command-map))
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
  :init
  (counsel-projectile-mode 1))
