;;; .emacs --- Init-file

;;; Commentary:
;;;   Inspiration:
;;;    - https://www.masteringemacs.org/
;;;    - https://writequit.org/org/settings.html
;;;    - https://home.elis.nu/emacs/
;;;    - https://pages.sachachua.com/.emacs.d/Sacha.html
;;;    - https://github.com/jorgenschaefer/Config/blob/master/emacs.el

;;; Code:
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
    ("491417843dee886b649cf0dd70c8c86c8bccbbe373239058ba9900b348bad5cf" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(doc-view-continuous t)
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#073642")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (haskell-mode auctex rainbow-mode org guru-mode multiple-cursors cobol-mode paredit modern-cpp-font-lock visible-mark merlin stan-mode ess flycheck use-package twittering-mode tuareg stan-snippets slime pdf-tools org-babel-eval-in-repl ob-sql-mode magit io-mode-inf io-mode intero htmlize gnugo flycheck-ocaml flycheck-haskell fish-mode fish-completion eww-lnum ess-smart-underscore elpy csv-mode csv benchmark-init)))
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
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "burlywood"))))
 '(ediff-odd-diff-B ((t (:background "Grey60")))))

;;; *** General setup ***

;; Prepare use-package
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
;; For TLS on windows: http://alpha.gnu.org/gnu/emacs/pretest/windows/
;; Download correct -dep file (x86_64) and unpack in emacs install directory
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --- Benchmark init ---
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Encoding
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)

;; Setup directories in ~/.emacs.d/
(dolist (folder '("lisp" "backups" "temp" "autosave"))
  (let ((dir (concat "~/.emacs.d/" folder)))
    (if (not (file-directory-p dir))
        (make-directory dir))))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq backup-directory-alist         '(("." . "~/.emacs.d/backups/"))
      temporary-file-directory        "~/.emacs.d/temp/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen    t
      initial-scratch-message   nil
      load-prefer-newer         t
      select-enable-clipboard   t
      delete-by-moving-to-trash t)

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(setq next-line-add-newlines t)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)

;; Disable various modes
(dolist (mode '(tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(show-paren-mode t)

;; Prettify symbols
(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Enable C-x C-u (upcase-region) and C-x C-l (downcase region)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Press 'C-x r j e' to go to .emacs
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; Personal info
(setq user-full-name    "Simon Stoltze"
      user-mail-address "sstoltze@gmail.com")

(setq ring-bell-function (lambda ()))

(setq make-pointer-invisible t)

(global-font-lock-mode        t)
(setq gc-cons-threshold       (* 100 1024 1024)) ;; 100 mb
;; Allow font-lock-mode to do background parsing
(setq jit-lock-stealth-time   1
      ;; jit-lock-stealth-load 200
      jit-lock-chunk-size     1000
      jit-lock-defer-time     0.05)

;; Use disk space
(setq delete-old-versions  -1
      version-control      t
      vc-make-backup-files t
      backup-by-copying    t)

;; --- Modeline ---
;; Column in modeline
(column-number-mode 1)

;; Time in modeline
(setq display-time-24hr-format          t)
(setq display-time-day-and-date         nil)
(setq display-time-default-load-average nil)
(setq display-time-format               nil)
(setq display-time-use-mail-icon        nil)
(display-time-mode t)

;; --- Calendar ---
;; Weeks start monday
(setq-default calendar-week-start-day 1)
;; Week number in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.6
                    :foreground "dim grey")
(copy-face font-lock-constant-face 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 0.6
                    :foreground "dark slate grey")
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(setq calendar-intermonth-header
      (propertize "Wk"
                  'font-lock-face 'calendar-iso-week-header-face))

;; --- Save history ---
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; --- List of recent files ---
(require 'recentf)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(recentf-mode t)
(setq recentf-max-saved-items 50
      recentf-max-menu-items 15)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; --- re-builder ---
;; M-x re-builder for making regex and searching current buffer
;; 'string avoids double-escaping in eg. \\.
(require 're-builder)
(setq reb-re-syntax 'string)

;; Make C-x C-x not activate region
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Better C-a
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)
(global-set-key (kbd "RET")
                'newline-and-indent)

;;; *** Packages ***
;; --- Visible mark ---
(use-package visible-mark
  :ensure t
  :init
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
  (defface visible-mark-active
    '((((type graphic))        ;; Graphics support
       (:box t))               ;; (:underline (:color "green" :style wave))
      (t                       ;; No graphics support - no box
       (:inverse-video t)))    ;;
    "Style for visible mark"
    :group 'visible-mark-group)
  (setq visible-mark-max    2)
  (setq visible-mark-faces  '(visible-mark-active
                              visible-mark-active))
  (global-visible-mark-mode 1))

(use-package guru-mode
  :ensure t
  :init
  (setq guru-warn-only t)
  (guru-global-mode 1))

;; --- Dired ---
(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (use-package dired-x
      ;; :init
      ;; (setq-default dired-omit-files-p t)
      :config
      (add-to-list 'dired-omit-extensions ".DS_Store"))
    (customize-set-variable 'diredp-hide-details-initially-flag nil)
    (use-package dired-aux
      :init
      (use-package dired-async))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq ls-lisp-dirs-first                  t
          dired-recursive-copies              'always
          dired-recursive-deletes             'always
          dired-dwim-target                   t
          ;; -F marks links with @
          dired-ls-F-marks-symlinks           t
          delete-by-moving-to-trash           t
          ;; Auto refresh dired
          global-auto-revert-non-file-buffers t
          wdired-allow-to-change-permissions  t)
    (add-hook 'dired-mode-hook
              (lambda ()
                (hl-line-mode 1)))))

;; --- Paredit ---
;; http://pub.gajendra.net/src/paredit-refcard.pdf
(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      #'enable-paredit-mode))

;; --- Flycheck ---
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode))

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
(let ((default-org-file "~/.emacs.d/organizer.org"))
  (if (not (file-exists-p default-org-file))
      (write-region ""                ; Start - What to write
                    nil               ; End - Ignored when start is string
                    default-org-file  ; Filename
                    t                 ; Append
                    nil               ; Visit
                    nil               ; Lockname
                    'excl))           ; Mustbenew - error if already exists
  (setq org-default-notes-file default-org-file)
  (setq org-agenda-files (list default-org-file))
  (set-register ?o (cons 'file default-org-file))
  (setq org-capture-templates
        (quote
         (("t" "Task" entry
           (file+headline default-org-file "Tasks")
           "* TODO %?
%U
%a
")
          ("r" "respond" entry
           (file default-org-file)
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
           (file+olp+datetree default-org-file)
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
")))))

(defun my-org-hook ()
  "Org mode hook."
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
    ;; At work
    (when (and (eq system-type 'windows-nt)
               (file-exists-p "C:\\Progra~2\\LibreOffice\\program\\soffice.exe")
               (equal (user-login-name) "sisto")) ;; Just for work
      ;; Export to .docx
      (setq org-odt-preferred-output-format "docx")
      (setq org-odt-convert-processes '(("LibreOffice" "C:\\Progra~2\\LibreOffice\\program\\soffice.exe --headless --convert-to %f%x --outdir %d %i"))))
    ;; Refile settings
    ;; Exclude DONE state tasks from refile targets
    (defun bh/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets."
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'bh/verify-refile-target))
  ;; org babel evaluate
  (require' ob)
  (progn
    ;; Make org mode allow eval of some langs
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa      . t)
       (lisp       . t)
       (emacs-lisp . t)
       (python     . t)
       (ruby       . t)
       (R          . t)
       (latex      . t)
       (sql        . t)))
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

;; --- Multiple cursors ---
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)))

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
    (when  (eq system-type 'cygwin)
      (defun cyg-slime-to-lisp-translation (filename)
        (replace-regexp-in-string "\n" ""
                                  (shell-command-to-string
                                   (format "cygpath.exe --windows %s" filename))))
      (defun cyg-lisp-to-slime-translation (filename)
        (replace-regexp-in-string "\n" "" (shell-command-to-string
                                           (format "cygpath.exe --unix %s" filename))))
      (setq slime-to-lisp-filename-function #'cyg-slime-to-lisp-translation)
      (setq lisp-to-slime-filename-function #'cyg-lisp-to-slime-translation))
    (setq inferior-lisp-program "sbcl --dynamic-space-size 2560")
    (setq slime-default-lisp "sbcl")
    (setq slime-contribs '(slime-fancy))))

;; --- LaTeX ---
;; Install auctex
(use-package tex
  :ensure auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook
            'turn-on-auto-fill))

;; --- Text ---
;; visual-line-mode only pretends to insert linebreaks
(remove-hook 'text-mode-hook
             'turn-on-auto-fill)
(add-hook    'text-mode-hook
             'turn-on-visual-line-mode)

;; --- Ediff ---
;; Ignore whitespace, no popup-window and split horizontally
(setq ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; --- HTML/CSS ---
(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (add-hook 'css-mode-hook
            'rainbow-mode))

;; --- CSV ---
(setq csv-separators (quote (";")))

;; --- Haskell ---
(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            'turn-on-haskell-indent)
  :config
  (progn
    (setq haskell-indent-spaces 4)
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

;; --- C/C++ ---
(defun common-c-hook ()
  "Hook for C/C++."
  (c-set-style "bsd")
  (setq c-basic-offset 2
        tab-width 2)
                                        ;(require 'semantic/bovine/gcc)
  (my-semantic-hook))
(defun my-cpp-hook ()
  "C++ specific packages."
  (use-package modern-cpp-font-lock
    :ensure t))
(add-hook 'c-mode-hook
          'common-c-hook)
(add-hook 'c++-mode-hook
          (lambda ()
            (common-c-hook)
            (my-cpp-hook)))

;; --- Java ---
(add-hook 'java-mode-hook
          'my-semantic-hook)

;; --- Magit ---
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)           ; Display the main magit popup
         ("C-x M-g" . magit-dispatch-popup))) ; Display keybinds for magit

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
;; python -m pip install --upgrade jedi rope black flake8 yapf autopep8 elpy
(use-package elpy
  :ensure t
  :pin elpy
  :defer t
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (if (or (eq system-type 'windows-nt)
                      (eq system-type 'ms-dos))
                  (setq python-shell-completion-native-disabled-interpreters
                        '("python")))
              (elpy-mode t)
              ))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (python-shell-switch-to-shell)))
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
                        'elpy-modules)))))

;; --- Ocaml ---
(use-package tuareg
  :ensure t
  :defer t
  :init
  (add-hook 'tuareg-mode-hook
            'merlin-mode)
  :config
  (use-package merlin
    :ensure t
    :config
    (use-package flycheck-ocaml
      :ensure t
      :config
      (flycheck-ocaml-setup))))

;; --- Twitter ---
(use-package twittering-mode
  :ensure t
  :defer t
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t))

;; --- Windows specific ---
(when (eq system-type 'windows-nt)
  (setq default-directory (concat "C:/Users/"
                                  (user-login-name)
                                  "/Desktop/"))
  ;; tramp
  (let ((plink-file "C:\\Program Files (x86)\\PuTTY\\plink.exe"))
    (when (file-exists-p plink-file)
      (setq tramp-default-method "plink")
      (when (not (string-match plink-file
                               (getenv "PATH")))
        (setenv "PATH" (concat plink-file
                               ";"
                               (getenv "PATH")))
        (add-to-list 'exec-path
                     plink-file)))))

;; --- Linux specific ---
(when (eq system-type 'gnu/linux)
  ;; SAGE
  (when (file-directory-p "/usr/lib/sagemath")
    (use-package sage
      :defer t
      :load-path "/usr/lib/sagemath/local/share/emacs"
      :config
      (setq sage-command "/usr/lib/sagemath/sage"))))

;; --- Work specific ---
(when (and (eq system-type 'windows-nt)
           (equal (user-login-name) "sisto"))
  (use-package cobol-mode
    :ensure t
    :init
    (setq auto-mode-alist
          (append
           '(("\\.cob\\'" . cobol-mode)
             ("\\.cbl\\'" . cobol-mode)
             ("\\.cpy\\'" . cobol-mode))
           auto-mode-alist))))

;; Rotate windows on C-<tab>
;; http://whattheemacsd.com/buffer-defuns.el-02.html#disqus_thread
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

;; Set initial frame size and position
(defun my/set-normal-frame ()
  (let* ((width-factor  0.80)
         (height-factor 0.80)
         (position-factor 3)
 	 (a-width  (* (display-pixel-width)  width-factor))
         (a-height (* (display-pixel-height) height-factor))
         (a-left (truncate (/ (- (display-pixel-width)  a-width)  position-factor)))
 	 (a-top  (truncate (/ (- (display-pixel-height) a-height) position-factor))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size     (selected-frame) (truncate a-width) (truncate a-height) t)))
(defun my/set-left-small-frame ()
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size     (selected-frame) (truncate (/ (display-pixel-width) 2.2)) (truncate (* (display-pixel-height) 0.9)) t))
(defun my/set-right-small-frame ()
  (set-frame-position (selected-frame) -1 0)
  (set-frame-size     (selected-frame) (truncate (/ (display-pixel-width) 2.2)) (truncate (* (display-pixel-height) 0.9)) t))

;; Frame resizing
(setq frame-resize-pixelwise t)
(when window-system
  (my/set-normal-frame))

;; Alt-enter toggles screensize
(defmacro handle-fullscreen-mode (func)
  `(progn
     (when *fullscreen-set*
       (toggle-frame-fullscreen)
       (setq *fullscreen-set* nil))
     (,func)))
(defvar *window-status* 0)
(defvar *window-options* (list
                          (lambda ()
                            (when (not *fullscreen-set*)
                              (toggle-frame-fullscreen)
                              (setq *fullscreen-set* t)))
                          (lambda ()
                            (handle-fullscreen-mode my/set-left-small-frame))
                          (lambda ()
                            (handle-fullscreen-mode my/set-right-small-frame))
                          (lambda ()
                            (handle-fullscreen-mode my/set-normal-frame))))
(defvar *fullscreen-set* nil)
(defun toggle-window (arg)
  (interactive "P")
  (when arg
    (message "%s" arg)
    (setq *window-status* (mod (prefix-numeric-value arg)
                               (length *window-options*))))
  (funcall (nth *window-status* *window-options*))
  (setq *window-status* (mod (1+ *window-status*)
                             (length *window-options*))))
(global-set-key (kbd "M-RET") 'toggle-window)

(provide '.emacs)
;;; .emacs ends here
