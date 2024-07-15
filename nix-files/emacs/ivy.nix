{ ... }:
{
  programs.emacs.extraConfig = ''
    ;;;; --- Counsel / Swiper / Ivy ---
    ;;;;; Counsel pulls in ivy and swiper
    ;;;;; Doing C-x C-f, C-M-j will create currently entered text as file-name
    (use-package counsel
      :ensure t
      ;; Defer to save time when just opening a file
      :defer t
      :hook ((prog-mode . ivy-mode))
      ;; Always enabled, do not show in mode-line
      :diminish counsel-mode
      :diminish ivy-mode
      ;; Load counsel when we need it
      :bind (("M-x"     . counsel-M-x)
             ("C-x b"   . ivy-switch-buffer)
             ("C-x C-f" . counsel-find-file)
             ("C-s"     . swiper-isearch)
             ("C-M-s"   . swiper-isearch-other-window)
             ;; counsel-grep-or-swiper should be faster on large buffers
             ("C-r"     . counsel-grep-or-swiper)
             ;; Find recent files
             ("C-x C-r" . counsel-recentf)
             ;; Resume last ivy completion - rarely used
             ;; ("C-c C-r" . ivy-resume)
             ;; Help commands
             ("C-h a"   . counsel-apropos)
             ("C-h b"   . counsel-descbinds)
             ("C-h f"   . counsel-describe-function)
             ("C-h v"   . counsel-describe-variable)
             ;; Store a view for the current session
             ("C-c v"   . ivy-push-view)
             ;; Remove a stored view
             ("C-c V"   . ivy-pop-view)
             (:map swiper-map
                   ("C-c s" . swiper-avy)))
      :custom
      ;; Allows selecting the prompt with C-p (same as C-M-j)
      (ivy-use-selectable-prompt    t)
      ;; Use ivy while in minibuffer to e.g. insert variable names
      ;; when doing counsel-set-variable
      (enable-recursive-minibuffers t)
      ;; Recentfs, views and bookmarks in ivy-switch-buffer
      (ivy-use-virtual-buffers      t)
      :config
      (ivy-mode 1)
      (counsel-mode 1)
      ;; With fuzzy matching, we do not need the initial ^ in the prompts
      (setq ivy-initial-inputs-alist '())
      ;; Show how deep the minibuffer goes
      (minibuffer-depth-indicate-mode 1)
      ;; Sort recentf by timestamp
      (add-to-list 'ivy-sort-functions-alist
                   '(counsel-recentf . file-newer-than-file-p))
      ;; Allow "M-x lis-pac" to match "M-x list-packages"
      (setq ivy-re-builders-alist '((swiper                . ivy--regex-plus)
                                    (swiper-isearch        . ivy--regex-plus)
                                    (counsel-rg            . ivy--regex-plus)
                                    (counsel-projectile-rg . ivy--regex-plus)
                                    (counsel-git-grep      . ivy--regex-plus)
                                    (t                     . ivy--regex-fuzzy))
            ivy-flx-limit         5000
            ;; Special views in ivy-switch-buffer
            ;; Use {} to easily find views in C-x b
            ivy-views             (append `(("init.el {}"
                                             (file "~/.emacs.d/init.el"))
                                            ("gtd {}"
                                             (horz
                                              (file "~/.emacs.d/org-files/gtd/unsorted.org")
                                              (vert (file "~/.emacs.d/org-files/gtd/projects.org")
                                                    (file "~/.emacs.d/org-files/gtd/archive.org"))))))))

    ;; Better fuzzy-matching
    (use-package flx
      :ensure t
      :after ivy)

    ;; Add info to ivy-buffers like 'M-x' or 'C-x b'
    (use-package ivy-rich
      :ensure t
      :after ivy
      :custom
      (ivy-rich-path-style 'abbrev)
      :config
      (ivy-rich-mode 1))

    (use-package ivy-posframe
      :ensure t
      :after ivy
      :diminish ivy-posframe-mode
      :custom
      (ivy-posframe-border-width 1)
      (swiper-action-recenter t)
      (ivy-posframe-display-functions-alist '((swiper-isearch . ivy-posframe-display-at-window-bottom-left)
                                              (swiper         . ivy-posframe-display-at-window-bottom-left)
                                              (t              . ivy-posframe-display-at-point)))
      :custom-face
      (ivy-posframe-border ((t (:background "goldenrod"))))
      :config
      (ivy-posframe-mode 1))

  '';
}
