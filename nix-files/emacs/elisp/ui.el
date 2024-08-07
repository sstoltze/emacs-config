(load-theme 'deeper-blue t)
(set-face-background 'cursor "burlywood")

;; Highlight current line
(add-hook 'prog-mode-hook 'hl-line-mode)

(use-package symbol-overlay
  :defer t
  :diminish symbol-overlay-mode
  :hook ((prog-mode . symbol-overlay-mode)))

;; Time in modeline
(use-package time
  :custom
  (display-time-24hr-format          t)
  (display-time-day-and-date         nil)
  (display-time-default-load-average nil)
  (display-time-use-mail-icon        t)
  :init
  (display-time-mode t))

;; Remove some things from modeline. Used by use-package.
(use-package diminish
  :config
  (diminish 'eldoc-mode "")
  (diminish 'company-mode "")
  (diminish 'auto-revert-mode ""))

(set-frame-font "Iosevka-14" nil t)
(toggle-frame-maximized)
