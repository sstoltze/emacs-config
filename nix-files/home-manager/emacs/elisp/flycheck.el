;; Next-error and prev-error are bound to M-g n and M-g p
;; Use C-c ! l to list all errors in a separate buffer
(use-package flycheck
  :ensure t
  :defer t
  ;; Always enabled, do not show in mode-line
  :diminish flycheck-mode
  :hook ((prog-mode . sstoltze/flycheck-if-not-remote)
         (text-mode . sstoltze/flycheck-if-not-remote))
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enable idle-buffer-switch))
  (flycheck-idle-change-delay          2)
  (flycheck-idle-buffer-switch-delay   2)
  (flycheck-elixir-credo-strict        t)
  :init
  ;; Disable flycheck for some modes on remote hosts, due to slowdowns when checking files
  (defun sstoltze/flycheck-if-not-remote ()
    "Do not start flycheck over TRAMP."
    (if (and (file-remote-p default-directory)
             (member major-mode (list 'python-mode)))
        (flycheck-mode -1)
      (flycheck-mode 1))))

(use-package flycheck-posframe
  :ensure t
  :defer t
  :hook ((flycheck-mode . flycheck-posframe-mode))
  :custom
  (flycheck-posframe-border-width 1)
  (flycheck-posframe-position 'point-window-center)
  :custom-face
  (flycheck-posframe-border-face ((t (:foreground "goldenrod"))))
  :config
  (flycheck-posframe-configure-pretty-defaults))
