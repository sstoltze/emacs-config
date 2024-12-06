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

(use-package visible-mark
  :custom
  (visible-mark-max 1)
  :init
  ;; Set face for mark
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
  (defface visible-mark-face1
    '((((type graphic))        ;; Graphics support
       (:box t))               ;; (:underline (:color "green" :style wave))
      (t                       ;; No graphics support - no box
       (:inverse-video t)))    ;;
    "Style for visible mark"
    :group 'visible-mark-group)
  (defface visible-mark-face2
    '((((type graphic))        ;; Graphics support
       (:overline t :underline t))
      (t                       ;; No graphics support - no box
       (:inverse-video t)))
    "Style for secondary mark"
    :group 'visible-mark-group)
  (setq visible-mark-faces  '(visible-mark-face1
                              visible-mark-face2))
  (global-visible-mark-mode 1))
