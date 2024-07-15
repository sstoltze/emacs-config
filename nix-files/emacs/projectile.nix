{ ... }:
{
  programs.emacs.extraConfig = ''
    (defun sstoltze/projectile-file-relative-name (line-number)
      "Return the current buffer file name, relative to the project root.
    If LINE-NUMBER is given, append the line at point to the file name."
      (if (fboundp 'projectile-project-root)
          (format "%s%s"
                  (file-relative-name (buffer-file-name) (projectile-project-root))
                  line-number)))

    (defun sstoltze/projectile-yank-relative-name (line-number)
      "Yank the current buffer file name, relative to the project root.
    If prefix argument LINE-NUMBER is given, append the line at point to
    the file name."
      (interactive (list (if (consp current-prefix-arg)
                             (format ":%d" (line-number-at-pos nil t))
                           "")))
      (kill-new (sstoltze/projectile-file-relative-name line-number)))

    (use-package projectile
      :ensure t
      :defer t
      :bind-keymap (("C-c p" . projectile-command-map))
      :bind ((:map projectile-command-map
                   ("y"   . #'sstoltze/projectile-yank-relative-name)))
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
  '';
}
