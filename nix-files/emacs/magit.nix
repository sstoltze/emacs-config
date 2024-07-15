{ ... }:

{
  programs.emacs.extraConfig = ''
    (use-package sqlite3
      :ensure t
      :defer t)

    (use-package magit
      :ensure t
      :defer t
      :bind (("C-x g" . magit-status)       ; Display the main magit popup
             ("C-c g" . magit-file-dispatch)) ; Run blame, etc. on a file
      :hook ((magit-mode-hook . hl-line-mode))
      :custom
      (magit-completing-read-function 'ivy-completing-read)
      ;; Remove the startup message about turning on auto-revert
      (magit-no-message (list "Turning on magit-auto-revert-mode..."))
      ;; Command prefix for merge conflicts. Alternatively use 'e' for ediff
      (smerge-command-prefix "\C-cv")
      :config
      (with-eval-after-load 'hl-line-mode
        (set-face-background 'hl-line
                             ;; Magit background color
                             (face-background 'magit-section-highlight))))

    (use-package git-timemachine
      :ensure t
      :defer t)

    (use-package git-link
      :ensure t
      :defer t)
  '';

}
