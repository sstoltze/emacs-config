(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-DVI-via-PDFTeX nil)
 '(TeX-PDF-mode nil)
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "693f5a81a3728c2548efb4118c81941933cf0f7b614f9f3133101395e5830152" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "935cc557b01242fc7b4d3f803902d14d1b3afae5123624a2f924255f641f7f01" "7ce5ae5476aadfa57ffbfffd41c2d3f4aaa4e7f21de6646a76f10b2a7eaa105b" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "007b69ffec046a5842e34fea287b23c49175dfd6c6d5a0d9cdf150a2e8a8979f" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(doc-view-continuous t)
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (ob-sql-mode company-irony company-irony-c-headers flycheck-irony irony ess company-ghci company-jedi company-ghc intero benchmark-init stan-snippets stan-mode ob elpy ess-smart-underscore flycheck-haskell ghc haskell-mode flycheck-ocaml merlin tuareg slime company company-auctex company-c-headers twittering-mode flycheck fish-completion fish-mode io-mode io-mode-inf magit auto-complete htmlize csv-mode csv auctex pdf-tools org-babel-eval-in-repl excorporate eww-lnum org use-package gnugo)))
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#b58900")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#859900")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#2aa198")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "slate blue"))))
 '(company-scrollbar-fg ((t (:background "dark slate blue"))))
 '(company-tooltip ((t (:inherit default :background "#1b1e2c"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(cursor ((t (:background "forest green")))))

;;; General setup ------------------------------------------------------
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)

(if (not (file-directory-p "~/.emacs.d/lisp"))
    (make-directory "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/lisp")
(if (not (file-directory-p "~/.emacs.d/backups"))
    (make-directory "~/.emacs.d/backups"))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(if (not (file-directory-p "~/.emacs.d/temp"))
    (make-directory "~/.emacs.d/temp"))
(setq temporary-file-directory "~/.emacs.d/temp")
(setq select-enable-clipboard t)

(if (eq system-type 'windows-nt)
    (setq default-directory "C:/Users/sisto/Desktop/"))

(setq display-time-24hr-format          t)
(setq display-time-day-and-date         nil)
(setq display-time-default-load-average nil)
(setq display-time-format               nil)
(setq display-time-use-mail-icon        nil)
(display-time-mode t)

(setq next-line-add-newlines t) ;; Newline at end of file
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Enable C-x C-u (upcase-region) and C-x C-l (downcase region)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)
(setq csv-separators (quote (";")))

(show-paren-mode  t)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))

(load-library "find-lisp") ;; Provides find-lisp-find-files

;; https://home.elis.nu/emacs/
(setq-default calendar-week-start-day 1) ; Weeks start monday
(defalias 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; https://writequit.org/org/settings.html
(global-font-lock-mode        t)
(setq gc-cons-threshold       (* 100 1024 1024)) ;; 100 mb
;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time   1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size     1000
      jit-lock-defer-time     0.05)
(setq inhibit-startup-screen  t)
(setq initial-scratch-message nil)
(setq ring-bell-function      (lambda ()))
(setq make-pointer-invisible  t)
(setq load-prefer-newer       t)

;;; Packages -----------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; --- Flycheck ---
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;; --- org-mode ---
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-ellipsis "…")
(setq org-startup-folded nil)
(setq org-startup-indented t)
(setq org-startup-with-inline-images t)
(setq org-default-notes-file "~/organizer.org")
(set-register ?o (cons 'file "~/organizer.org"))
(setq org-capture-templates
      (quote
       (("t" "Task" entry
         (file+headline "~/organizer.org" "Tasks")
         "* TODO %?
%U
%a
")
        ("r" "respond" entry
         (file "~/organizer.org")
         "* NEXT Respond to %:from on %:subject
SCHEDULED: %t
%U
%a
")
        ("n" "note" entry
         (file+headline "~/noter.org" "Notes")
         "* %? :NOTE:
%U
%a
")
        ("j" "Journal" entry
         (file+olp+datetree "~/organizer.org")
         "* %?
%U
")
        ("m" "Meeting" entry
         (file
          (lambda nil
            (buffer-file-name)))
         "* %? - %u
:ATTENDEES:
Simon Stoltze
:END:
"))))

(defun my-org-hook ()
  (progn
    (setq org-time-stamp-custom-formats (quote ("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")))
    (setq org-log-done t)

    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))
    ;; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    ;; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)
    ;; Use IDO for both buffer and file completion and ido-everywhere to t
    (setq org-completion-use-ido t)
    ;; Export to .docx
    (setq org-odt-preferred-output-format "docx")
    (setq org-odt-convert-processes '(("LibreOffice" "C:\\Progra~1\\LibreOffice\\program\\soffice.exe --headless --convert-to %f%x --outdir %d %i")))
    (if (eq system-type 'cygwin)
        (setq org-agenda-files
              (append
               (quote ("/cygdrive/c/Users/sisto/AppData/Roaming/noter.org"
                       "/cygdrive/c/Users/sisto/AppData/Roaming/calendar.org"
                       "/cygdrive/c/Users/sisto/AppData/Roaming/organizer.org"))
               (find-lisp-find-files
                "/cygdrive/c/Users/sisto/Desktop/noter"
                "\.org$")))
      (setq org-agenda-files
            (append
             (quote ("~/noter.org" "~/calendar.org" "~/organizer.org"))
             (find-lisp-find-files
              "C:\\Users\\sisto\\Desktop\\noter"
              "\.org$"))))
    ;; Refile settings
    ;; Exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets."
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'bh/verify-refile-target))
  ;; org babel evaluate
  (require' ob)
  (progn
    ;; make org mode allow eval of some langs
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)
       (lisp . t)
       (emacs-lisp . t)
       (python . t)
       (ruby . t)
       (R . t)
       (latex . t)
       (sql . t)))
    (setq org-confirm-babel-evaluate nil)
    (add-hook 'org-babel-after-execute-hook
              'org-display-inline-images)))
(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode)
                             (org-indent-mode)
                             (org-display-inline-images)
                             (my-org-hook)))

;; --- Ido ---
(use-package ido
  :ensure t
  :config
  (progn
    (setq ido-everywhere t)
    (setq ido-max-directory-size 100000)
    ;; Use the current window when visiting files and buffers with ido
    (setq ido-default-file-method 'selected-window)
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-enable-flex-matching t)
    (setq ido-confirm-unique-completion t)
    (ido-mode t)))

;; --- Company ---
(use-package company
  :ensure t
  :config
  (if (display-graphic-p)
      (progn
        (require 'color)
        (let ((bg (face-attribute 'default :background)))
          (custom-set-faces
           `(company-tooltip           ((t (:inherit default
                                                     :background
                                                     ,(color-lighten-name bg 2)))))
           `(company-scrollbar-bg      ((t (:background
                                            ,(color-lighten-name bg 10)))))
                                        ; "slate blue"))))
           `(company-scrollbar-fg      ((t (:background
                                            ,(color-lighten-name bg 5)))))
                                        ;"dark slate blue"))))
           `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
           `(company-tooltip-common    ((t (:inherit font-lock-constant-face))))))
        (delete 'company-clang 'company-backends))))
(add-hook 'after-init-hook
          (lambda ()
            (global-company-mode '(not python-mode
                                       elpy-mode))))

;; --- Semantic ---
(defun my-semantic-hook ()
  "Hook for semantic to add TAGS to menubar."
  (imenu-add-to-menubar "TAGS")
  (require 'semantic)
  (require 'semantic/ia)
  (require 'semantic/wisent)
  (add-to-list 'semantic-default-submodes
               'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-local-symbol-highlight-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-completions-mode)
  (add-to-list 'semantic-default-submodes
               'global-semantic-idle-summary-mode)
  (semantic-mode t))

;; --- Lisp ---
(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    ;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "sbcl --dynamic-space-size 2560")
    (setq slime-default-lisp "sbcl")
    (setq slime-contribs '(slime-fancy))))

;; --- LaTeX ---
(add-hook 'LaTeX-mode-hook
          'turn-on-auto-fill)

;; --- HTML/CSS ---
(add-hook 'css-mode-hook
          'rainbow-mode)

;; --- Haskell ---
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq haskell-indent-spaces 4)
    (use-package company-ghc
      :ensure t)
    (use-package company-ghci
      :ensure t)
    (add-to-list 'company-backends
                 '(company-ghc company-ghci))
    (use-package intero
      :ensure t
      :config
      (add-hook 'haskell-mode-hook
                'intero-mode))
    (use-package flycheck-haskell
      :ensure t
      :config
      (add-hook 'haskell-mode-hook
                'flycheck-haskell-setup))))
(add-hook 'haskell-mode-hook
          'turn-on-haskell-indent)

;; --- C/C++ ---
(defun my-c-hook ()
  "Hook for C/C++."
  (use-package company-c-headers
    :ensure t
    :init
    (add-to-list 'company-backends
                 'company-c-headers))
  (require 'semantic/bovine/gcc)
  (my-semantic-hook)
  (use-package irony
    :ensure t
    :init
    (progn
      (setq w32-pipe-read-delay 0)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024))
      (use-package company-irony-c-headers
        :ensure t)
      (add-to-list 'company-backends
                   '(company-irony-c-headers
                     company-irony))
      (add-hook 'irony-mode-hook
                'irony-cdb-autosetup-compile-options)
      (use-package flycheck-irony
        :ensure t
        :config
        (add-hook 'irony-mode-hook
                  'flycheck-irony-setup))))
  (irony-mode))
(add-hook 'c-mode-hook
          'my-c-hook)
(add-hook 'c++-mode-hook
          'my-c-hook)


;; --- Java ---
(add-hook 'java-mode-hook
          'my-semantic-hook)

;; --- Magit ---
(if (not (eq system-type 'cygwin))
    (use-package magit
      :ensure t
      :bind (("C-x g" . magit-status)     ; Display the main magit popup
             ("C-x M-g" . magit-dispatch-popup))) ; Display keybinds for magit
  )

;; --- Fish ---
(use-package fish-mode
  :defer t
  :ensure t)

;; --- ESS - Emacs Speaks Statistics ---
(use-package ess-site
  :ensure ess
  :defer t
  :config
  (progn
    (use-package ess-smart-underscore
      :defer t
      :ensure t)))

;; --- Stan ---
(use-package stan-mode
  :ensure t
  :defer t
  :config
  (progn
    (use-package stan-snippets
      :defer t
      :ensure t)))

;; --- Python ---
;; python -m pip install --upgrade jedi rope black flake8 yapf autopep8
(use-package elpy
  :ensure t
  :pin elpy
  :defer t
  :config
  (progn
    (setq elpy-shell-use-project-root nil)
    (setq elpy-rpc-backend "jedi")
    (elpy-enable)
    ;; Enable pyvenv, which manages Python virtual environments
    (pyvenv-mode 1)
    ;; Tell Python debugger (pdb) to use the current virtual environment
    ;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
    (setq gud-pdb-command-name "python -m pdb ")
    (defun my-restart-python-console ()
      "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
      (interactive)
      (kill-process "Python")
      (sleep-for 0.15)
      (kill-buffer "*Python*")
      (elpy-shell-send-region-or-buffer))
    (global-set-key (kbd "C-c C-x C-c") 'my-restart-python-console)
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (delete 'elpy-module-company
                        'elpy-modules)
                (company-mode 0)))))
(add-hook 'python-mode-hook
          (lambda ()
            (if (or (eq system-type 'windows-nt)
                    (eq system-type 'ms-dos))
                (setq python-shell-completion-native-disabled-interpreters
                      '("python")))
            (elpy-mode t)
            (company-mode 0)))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (company-mode 0)
            (python-shell-switch-to-shell)))

;; --- Ocaml ---
(use-package tuareg
  :ensure t
  :defer t
  :config
  (setq tuareg-font-lock-symbols t)
  (use-package merlin
    :ensure t
    :config
    (add-to-list 'company-backends
                 'merlin-company-backend)
    (use-package flycheck-ocaml
      :ensure t
      :config
      (flycheck-ocaml-setup))))
(add-hook 'tuareg-mode-hook
          'merlin-mode)

;; Twitter
(if (eq system-type 'gnu/linux)
    (use-package twittering-mode
      :ensure t
                                        ;  :init
                                        ; (add-to-list 'load-path )
                                        ; (require 'twittering-mode)
      :config
      (setq twittering-use-master-password t)
      (setq twittering-icon-mode t)))

;; Macaulay 2
(if (eq system-type 'gnu/linux)
    (load "~/.emacs-Macaulay2" t))

;; SAGE
(if (eq system-type 'gnu/linux)
    (use-package sage
      :load-path "/usr/lib/sagemath/local/share/emacs"
                                        ;  :init
                                        ;  (add-to-list 'load-path (expand-file-name "/usr/lib/sagemath/local/share/emacs"))
      :config
      (setq sage-command "/usr/lib/sagemath/sage")))

;; Rotate windows on C-<tab>
; http://whattheemacsd.com/buffer-defuns.el-02.html#disqus_thread
(defun rotate-windows ()
  "Rotate your windows."
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (let ((i 1)
               (numWindows (count-windows)))
           (while  (< i numWindows)
             (let* ((w1 (elt (window-list) i))
                    (w2 (elt (window-list) (+ (% i numWindows) 1)))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i (1+ i))))))))

(global-set-key (kbd "<C-tab>") 'rotate-windows)
