(require 'package)
(setq package-enable-at-startup nil)

;; Setup directories in ~/.emacs.d/
(dolist (folder '("backups"
                  "temp"
                  "autosave"))
  (let ((dir (concat "~/.emacs.d/" folder)))
    (if (not (file-directory-p dir))
        (make-directory dir))))

(setq backup-directory-alist         '(("." . "~/.emacs.d/backups/"))
      temporary-file-directory       "~/.emacs.d/temp/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      ;; This is never loaded
      custom-file                    "~/.emacs.d/custom.el")

;; Make it easier to answer prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; General variables
(setq inhibit-startup-screen                t
      initial-scratch-message               nil

      ;; Load newest file from disk
      load-prefer-newer                     t

      ;; Delete to trash
      delete-by-moving-to-trash             t

      ;; Exit read-only buffers with q
      view-read-only                        t

      ;; Copy-paste
      select-enable-clipboard               t
      save-interprogram-paste-before-kill   t

      ;; Make case insensitive
      completion-ignore-case                t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t

      ;; Use disk space
      version-control                       t
      delete-old-versions                   t
      vc-make-backup-files                  t
      backup-by-copying                     t
      vc-follow-symlinks                    t
      kept-new-versions                     64

      ;; History
      history-length                        t
      history-delete-duplicates             t

      ;; Add newlines when scrolling a file
      next-line-add-newlines                t

      ;; Garbage collector
      gc-cons-threshold                     (* 100 1024 1024) ;; 100 mb

      ;; Disable the bell
      ring-bell-function                    'ignore

      ;; Add directory name to buffer if name is not unique
      uniquify-buffer-name-style            'forward

      ;; Prettify symbols
      prettify-symbols-unprettify-at-point  'right-edge

      ;; Scrolling forward and then back preserves point position
      scroll-preserve-screen-position       t

      show-paren-delay                      0

      ;; Add final newline when saving a file - set to 'visit to do it on visit but not save
      require-final-newline                 t)

;; Do not use tabs
(setq-default indent-tabs-mode              nil
              show-trailing-whitespace      t
              tab-width                     2
              tab-always-indent             'complete)

;; Random indentation
(setq-default standard-indent               2
              sh-basic-offset               2
              fish-indent-offset            2)

;; Enable various modes
(dolist (mode '(show-paren-mode
                ;; Prettify symbols
                global-prettify-symbols-mode
                ;; Column in modeline
                column-number-mode
                ;; Automatically reload changed files
                global-auto-revert-mode))
  (when (fboundp mode)
    (funcall mode 1)))

;; Disable various modes
(dolist (mode '(tool-bar-mode
                scroll-bar-mode
                tooltip-mode
                menu-bar-mode
                electric-indent-mode
                blink-cursor-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Automatic indent on pressing RET
(global-set-key (kbd "RET") 'newline-and-indent)

;; Do not ask to kill buffer every time
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Better behaviour for M-SPC
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Better DWIM behaviour
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; Delete extra lines and spaces when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package recentf
  :custom
  (recentf-max-saved-items 500)
  (recentf-exclude '("/auto-install/" ".recentf" "/elpa/" ".gz" "/tmp/" "/ssh:" "/sudo:" "/scp:"))
  :init
  (recentf-mode 1)
  :config
  (setq recentf-filename-handlers (append '(abbreviate-file-name) recentf-filename-handlers))
  (run-at-time nil (* 10 60)
               (lambda ()
                 (let ((save-silently t))
                   (recentf-save-list)))))

;;;; --- Dired ---
(use-package dired
  :hook ((dired-mode . hl-line-mode))
  :bind ((:map dired-mode-map
               ("b" . dired-up-directory)))
  :custom
  (ls-lisp-dirs-first                  t)
  (dired-recursive-copies              'always)
  (dired-recursive-deletes             'always)
  (dired-dwim-target                   t)
  ;; -F marks links with @
  (dired-ls-F-marks-symlinks           t)
  ;; Auto refresh dired
  (global-auto-revert-non-file-buffers t)
  ;; Make size listing human readable
  (dired-listing-switches              "-alh")
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :after dired
  :bind (("C-x C-j" . dired-jump))
  :config
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(use-package dired-aux
  :after dired)
