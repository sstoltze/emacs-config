;;; init.el --- Init-file

;;; Commentary:
;;   Inspiration:
;;    - https://www.masteringemacs.org/
;;    - https://writequit.org/org/settings.html
;;    - https://home.elis.nu/emacs/
;;    - https://pages.sachachua.com/.emacs.d/Sacha.html
;;    - https://github.com/jorgenschaefer/Config/blob/master/emacs.el

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("491417843dee886b649cf0dd70c8c86c8bccbbe373239058ba9900b348bad5cf" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(doc-view-continuous t)
 '(package-selected-packages
   (quote
    (ivy-rich avy flx counsel ob-async diminish hc-zenburn-theme outline-magic mu4e-alert haskell-mode auctex rainbow-mode org guru-mode multiple-cursors cobol-mode paredit modern-cpp-font-lock visible-mark merlin stan-mode ess flycheck use-package twittering-mode tuareg stan-snippets slime pdf-tools org-babel-eval-in-repl ob-sql-mode magit io-mode-inf io-mode intero htmlize gnugo flycheck-ocaml flycheck-haskell fish-mode fish-completion eww-lnum elpy csv-mode csv benchmark-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; *** General setup ***

;;;; --- Use-package ---
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

;;;; --- Benchmark init ---
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;; --- Encoding ---
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)

;;;; --- Setup ---
;; Setup directories in ~/.emacs.d/
(dolist (folder '("lisp" "backups" "temp" "autosave" "org-files" "org-files/gtd"))
  (let ((dir (concat "~/.emacs.d/" folder)))
    (if (not (file-directory-p dir))
        (make-directory dir))))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq backup-directory-alist         '(("." . "~/.emacs.d/backups/"))
      temporary-file-directory       "~/.emacs.d/temp/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      savehist-file                  "~/.emacs.d/savehist")

;; Disable various modes
(dolist (mode '(tool-bar-mode scroll-bar-mode tooltip-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Make it easier to answer prompts
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen                t
      initial-scratch-message               nil
      load-prefer-newer                     t
      select-enable-clipboard               t
      delete-by-moving-to-trash             t

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

      ;; Add newlines when scrolling a file
      next-line-add-newlines                t

      ;; Remove mouse pointer while typing
      make-pointer-invisible                t

      ;; Garbage collector
      gc-cons-threshold (* 100 1024 1024) ;; 100 mb

      ;; Personal info
      user-full-name    "Simon Stoltze"
      user-mail-address (cond ((equal (user-login-name)
                                      "sisto")          "sisto@sd.dk" )
                              (t                        "sstoltze@gmail.com"))

      ;; Disable the bell
      ring-bell-function 'ignore

      ;; Add directory name to buffer if name is not unique
      uniquify-buffer-name-style 'forward)


;; Weeks start monday
(setq-default calendar-week-start-day 1
              ;; Do not use tabs
              indent-tabs-mode        nil)

;; Delete extra lines and spaces when saving
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Show matching parens
(show-paren-mode t)

;; Prettify symbols
(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; C-x 8 RET to find and insert unicode char
(add-hook 'prog-mode-hook (lambda ()
                            (mapc (lambda (pair)
                                    (push pair prettify-symbols-alist))
                                  '(("<="  . ?≤)
                                    (">="  . ?≥)
                                    ("!="  . ?≠) ;; C
                                    ("/="  . ?≠) ;; Lisp
                                    ("->"  . ?→)
                                    ("<-"  . ?←)
                                    ("=>"  . ?⇒)
                                    ("..." . ?…)))))

;; Enable C-x C-u (upcase-region) and C-x C-l (downcase region)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; Press 'C-x r j e' to go to init.el
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; Font lock
(global-font-lock-mode t)
(setq jit-lock-stealth-time   1
      jit-lock-chunk-size     1000
      jit-lock-defer-time     0.05)

;; Save history
(savehist-mode 1)
(setq history-length                   t
      history-delete-duplicates        t
      savehist-save-minibuffer-history 1
      savehist-additional-variables    '(kill-ring
                                         search-ring
                                         regexp-search-ring))

;;;; --- Modeline ---
;; Column in modeline
(column-number-mode 1)

;; Time in modeline
(setq display-time-24hr-format          t
      display-time-day-and-date         nil
      display-time-default-load-average nil
      display-time-use-mail-icon        t)
(display-time-mode t)

;;;; --- Calendar ---
;; Week number in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.6
                    :foreground "dim grey")
(copy-face font-lock-constant-face 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil
                    :height 0.6
                    :foreground "dark slate grey")
(setq calendar-intermonth-text '(propertize
                                 (format "%2d"
                                         (car
                                          (calendar-iso-from-absolute
                                           (calendar-absolute-from-gregorian (list month day year)))))
                                 'font-lock-face 'calendar-iso-week-face)
      calendar-intermonth-header (propertize "Wk"
                                             'font-lock-face 'calendar-iso-week-header-face))

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

;;;; --- Diminish ---
;; Remove some things from modeline. Used by use-package.
(use-package diminish
  :ensure t)

;;;; --- Visible mark ---
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
  (setq visible-mark-max    1)
  (setq visible-mark-faces  '(visible-mark-active
                              visible-mark-active))
  (global-visible-mark-mode 1))

;;;; --- Guru-mode ----
(use-package guru-mode
  :ensure t
  ;; Always enabled, do not show in mode-line
  :diminish guru-mode
  :init
  (setq guru-warn-only t)
  (guru-global-mode 1))

;;;; --- Dired ---
(use-package dired
  :bind (("C-x C-j"  . dired-jump))
  :hook ((dired-mode . hl-line-mode))
  :config
  (use-package dired-x
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
        wdired-allow-to-change-permissions  t))

;;;; --- Eshell ---
(use-package eshell
  :bind (("C-c e" . eshell))
  :hook ((eshell-mode . (lambda ()
                          (eshell-smart-initialize)
                          (eshell/alias "emacs"  "find-file $1")
                          (eshell/alias "magit"  "magit-status")
                          (eshell/alias "less"   "cat $1")
                          (eshell/alias "python" "python3 $*")
                          (local-set-key (kbd "C-c h")
                                         (lambda ()
                                           "Ivy interface to eshell history."
                                           (interactive) ;; Maybe insert move-to-end-of-buffer here
                                           (insert
                                            (ivy-completing-read "History: "
                                                                 (delete-dups
                                                                  (ring-elements eshell-history-ring))))))
                          (local-set-key (kbd "C-c C-h") 'eshell-list-history)))
         ;; Send message when command finishes and buffer is not active
         ;; Alternatively, look at package 'alert'
         (eshell-kill . (lambda (process status)
                          "Shows process and status in minibuffer when a command finishes."
                          (let ((buffer (process-buffer process)))
                            ;; To check buffer not focused, use
                            ;;   (eq buffer (window-buffer (selected-window)))
                            ;; Check buffer is not visible
                            (if (not (get-buffer-window buffer))
                                (message "%s: %s."
                                         process
                                         ;; Replace final newline with nothing
                                         (replace-regexp-in-string "\n\\'" ""
                                                                   status)))))))
  :config
  (require 'em-smart)
  (require 'esh-module)
  (with-eval-after-load 'esh-module
    ;; If not wrapped in this, eshell complains
    (add-to-list 'eshell-load-hook
                 (lambda ()
                   (add-to-list 'eshell-modules-list 'eshell-tramp)
                   ;; enable password caching
                   (setq password-cache t
                         ;; time in seconds
                         password-cache-expiry 600))))
  (setq eshell-ls-use-colors                    t
        ;; History
        eshell-save-history-on-exit             t
        eshell-history-size                     256000
        eshell-hist-ignoredups                  t
        ;; Globbing
        eshell-glob-case-insensitive            t
        eshell-error-if-no-glob                 t
        ;; Completion
        eshell-cmpl-cycle-completions           nil
        eshell-cmpl-ignore-case                 t
        ;; Remain at start of command after enter
        eshell-where-to-jump                    'begin
        eshell-review-quick-commands            nil
        ;; Close buffer on exit
        eshell-destroy-buffer-when-process-dies t
        ;; Scrolling
        eshell-scroll-to-bottom-on-input        nil
        ehsell-scroll-to-bottom-on-output       nil
        eshell-scroll-show-maximum-output       t
        eshell-smart-space-goes-to-end          t)
  (setq eshell-banner-message ""
        eshell-prompt-function
        (lambda ()
          (let ((standard-colour "light goldenrod")
                (time-colour     "gray")
                (user-colour     "light sky blue"))
            (concat (propertize (format-time-string "%H:%M"
                                                    (current-time))
                                'face (list :foreground time-colour))
                    " "
                    (propertize (user-login-name)
                                'face (list :foreground user-colour))
                    " "
                    (propertize (fish-path (eshell/pwd) 20)
                                'face (list :foreground standard-colour))
                    (sstoltze/make-vc-prompt)
                    (propertize " >"
                                'face (list :foreground standard-colour))
                    ;; This resets text properties
                    " ")))
        eshell-prompt-regexp "^[0-9]\\{1,2\\}:[0-9]\\{2\\} .+ .+ > "))

;; Could consider making the colours parameters to be
;; able to change them when calling in eshell-prompt-function
(defun sstoltze/make-vc-prompt ()
  "Small helper for eshell-prompt-function.
If includes git branch-name if magit is loaded
and tries to emulate the fish git prompt.

Can be replaced with:
\(or (ignore-errors (format \" (%s)\"
                           (vc-responsible-backend
                            default-directory)))
    \"\")"
  (let ((standard-colour  "pale goldenrod")
        (untracked-colour "red")
        (unstaged-colour  "yellow green")
        (staged-colour    "royal blue")
        (vc-response (or (ignore-errors (format "%s"
                                                (vc-responsible-backend
                                                 default-directory)))
                         "")))
    (cond ((equal vc-response "Git")
           (let ((branch    (or (ignore-errors
                                  (magit-get-current-branch))
                                "Git"))
                 (untracked (or (ignore-errors
                                  (length (magit-untracked-files)))
                                0))
                 (unstaged  (or (ignore-errors
                                  (length (magit-unstaged-files)))
                                0))
                 (staged    (or (ignore-errors
                                  (length (magit-staged-files)))
                                0)))
             (concat (propertize " ("
                                 'face (list :foreground
                                             standard-colour))
                     (propertize branch
                                 'face (list :foreground
                                             standard-colour))
                     (propertize (if (> (+ untracked unstaged staged) 0)
                                     "|"
                                   (if (equal branch "Git")
                                       ""
                                     "|✔"))
                                 'face (list :foreground
                                             standard-colour))
                     (propertize (if (> untracked 0)
                                     (format "…%s" untracked)
                                   "")
                                 'face (list :foreground
                                             untracked-colour))
                     (propertize (if (> unstaged 0)
                                     (format "+%s" unstaged)
                                   "")
                                 'face (list :foreground
                                             unstaged-colour))
                     (propertize (if (> staged 0)
                                     (format "→%s" staged)
                                   "")
                                 'face (list :foreground
                                             staged-colour))
                     (propertize ")"
                                 'face (list :foreground
                                             standard-colour)))))
          ((equal vc-response "")
           (propertize  ""
                        'face (list :foreground
                                    standard-colour)))
          (t
           (propertize (format " (%s)" vc-response)
                       'face (list :foreground
                                   standard-colour))))))

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (seq-reduce '+ (mapcar 'length components) 0)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (seq-reduce (lambda (a b) (concat a "/" b)) (cdr components) (car components)))))

;;;; --- Paredit ---
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

;;;; --- Flycheck ---
(use-package flycheck
  :ensure t
  :defer t
  ;; Always enabled, do not show in mode-line
  :diminish flycheck-mode
  :hook ((prog-mode . flycheck-mode)
         (text-mode . flycheck-mode)))

;;;; --- Auto-insert ---
;; Insert into file, but mark unmodified
(setq auto-insert       'other
      ;; Do not ask when inserting
      auto-insert-query nil)
;; Only do it for org-mode
(add-hook 'org-mode-hook 'auto-insert)
(with-eval-after-load 'autoinsert
  (define-auto-insert '("\\.org\\'" . "Org header")
    '(nil
      "#+AUTHOR: " user-full-name \n
      "#+EMAIL: " user-mail-address \n
      "#+DATE: " (format-time-string "%Y-%m-%d" (current-time)) \n
      "#+OPTIONS: toc:nil title:nil author:nil email:nil date:nil creator:nil" \n
      "* " )))

;;;; --- Org ---
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-ellipsis                   "…"
      org-startup-folded             nil
      org-startup-indented           t
      org-startup-with-inline-images t
      org-export-backends (quote (ascii beamer html icalendar latex md odt)))
;; Most GTD setup is taken from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(let ((default-org-file "~/.emacs.d/org-files/gtd/default.org")   ;; Unsorted items
      (project-org-file "~/.emacs.d/org-files/gtd/projects.org")  ;; Currently active projects
      (archive-org-file "~/.emacs.d/org-files/gtd/archive.org")   ;; Projects that are currently waiting for something else
      (schedule-org-file "~/.emacs.d/org-files/gtd/schedule.org") ;; C-c C-s to schedule. C-c C-d to deadline
      (journal-org-file "~/.emacs.d/org-files/journal.org"))
  (dolist (org-file (list default-org-file project-org-file archive-org-file schedule-org-file journal-org-file))
    (if (not (file-exists-p org-file))
        (write-region (concat "#+AUTHOR: "
                              user-full-name
                              "\n#+EMAIL: "
                              user-mail-address
                              "\n#+DATE: "
                              (format-time-string "%Y-%m-%d" (current-time))
                              "\n#+OPTIONS: toc:nil title:nil author:nil email:nil date:nil creator:nil\n") ; Start - What to write
                      nil               ; End - Ignored when start is string
                      org-file          ; Filename
                      t                 ; Append
                      nil               ; Visit
                      nil               ; Lockname
                      'excl)))          ; Mustbenew - error if already exists
  (setq org-capture-templates
        `(("j" "Note"      entry (file+olp+datetree ,journal-org-file)
           "* %?"
           :empty-lines 1)
          ("t" "TODO"      entry (file+headline ,default-org-file "Unsorted")
           "* TODO %?\n%U\n%a\n"
           :clock-in t :clock-resume t
           :empty-lines 1)
          ("m" "Meeting"   entry (file ,default-org-file) ;; (lambda () (or (buffer-file-name) ,default-org-file))
           "* %? - %u :MEETING:\n:ATTENDEES:\nSimon Stoltze\n:END:\n"
           :clock-in t :clock-resume t
           :empty-lines 1)
          ("n" "Next"      entry (file+headline ,default-org-file "Unsorted")
           "* NEXT %?\n%U\nDEADLINE: %t"
           :clock-in t :clock-resume t
           :empty-lines 1)
          ("s" "Schedule"  entry (file+headline ,schedule-org-file "Schedule")
           "* %i%?\n%U"
           :empty-lines 1))
        org-default-notes-file journal-org-file
        org-agenda-files (list default-org-file
                               project-org-file
                               schedule-org-file
                               journal-org-file)
        ;; Possibly change levels here
        org-refile-targets `((,project-org-file  :maxlevel . 3)
                             (,schedule-org-file :level    . 1)
                             (,archive-org-file  :level    . 1)
                             (,journal-org-file  :maxlevel . 3)))
  (set-register ?d (cons 'file default-org-file))
  (set-register ?a (cons 'file archive-org-file))
  (set-register ?p (cons 'file project-org-file))
  (set-register ?s (cons 'file schedule-org-file))
  (set-register ?j (cons 'file journal-org-file)))
(defun my-org-hook ()
  "Org mode hook."
  (setq
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w)"))
   org-time-stamp-custom-formats (quote ("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
   org-use-fast-todo-selection t
   ;; Allow editing invisible region if it does that you would expect
   org-catch-invisible-edits 'smart
   org-log-done t
   org-refile-use-outline-path t
   ;; Targets complete directly with IDO
   org-outline-path-complete-in-steps nil
   ;; Allow refile to create parent tasks with confirmation
   org-refile-allow-creating-parent-nodes (quote confirm)
   ;; Use the current window for indirect buffer display
   org-indirect-buffer-display 'current-window
   ;; Author, email, date of creation, validation link at bottom of exported html
   org-html-postamble nil
   org-html-html5-fancy t
   org-html-doctype "html5")
  ;; Two options for literate programming.
  ;; Usage is as for SRC and EXAMPLE blocks, <pr<TAB> to expand
  (add-to-list 'org-structure-template-alist ;; A property drawer with correct settings for org-babel
               '("pr" ":PROPERTIES:\n:header-args: :results output :tangle yes :session *?*\n:END:"))
  (add-to-list 'org-structure-template-alist ;; A source block with header-args for exporting an image
               '("si" "#+BEGIN_SRC ? :results graphics :file ./images/\n\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist ;; A source block with silent enabled
               '("ss" "#+BEGIN_SRC ? :results silent\n\n#+END_SRC"))
  ;; At work
  (when (and (eq system-type 'windows-nt)
             (file-exists-p "C:/Progra~2/LibreOffice/program/soffice.exe")
             (equal (user-login-name) "sisto")) ;; Just for work
    ;; Export to .docx
    (setq org-odt-preferred-output-format "docx"
          org-odt-convert-processes '(("LibreOffice" "C:/Progra~2/LibreOffice/program/soffice.exe --headless --convert-to %f%x --outdir %d %i"))))
  ;; Refile settings
  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  ;; org babel evaluate
  (require 'ob)
  (use-package ob-async
    :ensure t)
  ;; Make org mode allow eval of some langs
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp       . t)
     (emacs-lisp . t)
     (python     . t)
     (ruby       . t)
     (R          . t)
     (latex      . t)
     (sql        . t)
     (stan       . t)))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively   t
        org-babel-python-command   "python3")
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images))
(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode 1)
                             (org-indent-mode  1)
                             (my-org-hook)))

;;;; --- Counsel / Swiper / Ivy ---
;;;;; Counsel pulls in ivy and swiper
;;;;; Doing C-x C-f, C-M-j will create currently entered text as file-name
(use-package counsel
  :ensure t
  ;; Always enabled, do not show in mode-line
  :diminish counsel-mode
  :diminish ivy-mode
  ;; This doesn't really work as expected
  ;;(global-set-key (kbd "C-r") 'swiper)
  :bind (("C-s"     . swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-c C-r" . ivy-resume))
  :init
  ;; Better fuzzy-matching
  (use-package flx
    :ensure t)
  (ivy-mode 1)
  (counsel-mode 1)
  ;; Allow "M-x lis-pac" to match "M-x list-packages"
  (setq ivy-re-builders-alist        '((swiper . ivy--regex-plus)
                                       (t      . ivy--regex-fuzzy))
        ;; With the above, we do not need the initial ^ in the prompts
        ivy-initial-inputs-alist     '()
        ;; Allows selecting the prompt with C-p (same as C-M-j)
        ivy-use-selectable-prompt    t
        ;; Use ivy while in minibuffer to e.g. insert variable names
        ;; when doing counsel-set-variable
        enable-recursive-minibuffers t)
  ;; Show how deep the minibuffer goes
  (minibuffer-depth-indicate-mode 1)
  ;; Sort recentf by timestamp
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-recentf . file-newer-than-file-p))
  ;; Add info to ivy-buffers like 'M-x' or 'C-x b'
  (use-package ivy-rich
    :ensure t
    :defer 1
    :config
    (ivy-rich-mode 1)
    (setq ivy-rich-path-style 'abbrev)
    ;; The default "Yellow" of deeper-blue is not great
    (set-face-foreground 'warning "goldenrod1")))

;;;; --- Multiple cursors ---
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

;;;; --- Semantic ---
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

;;;; --- Lisp ---
(use-package slime
  :ensure t
  :defer t
  :config
  (when (eq system-type 'cygwin)
    (defun cyg-slime-to-lisp-translation (filename)
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string
                                 (format "cygpath.exe --windows %s" filename))))
    (defun cyg-lisp-to-slime-translation (filename)
      (replace-regexp-in-string "\n" "" (shell-command-to-string
                                         (format "cygpath.exe --unix %s" filename))))
    (setq slime-to-lisp-filename-function #'cyg-slime-to-lisp-translation
          lisp-to-slime-filename-function #'cyg-lisp-to-slime-translation))
  (setq inferior-lisp-program "sbcl --dynamic-space-size 2560"
        slime-default-lisp "sbcl"
        slime-contribs '(slime-fancy)))

;;;; --- LaTeX ---
(use-package tex
  :ensure auctex
  :defer t
  :init
  (add-hook 'LaTeX-mode-hook
            'turn-on-auto-fill)
  :config
  (setq TeX-view-program-selection
        '(((output-dvi style-pstricks) "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Evince")
          (output-html "xdg-open"))
        TeX-PDF-mode nil
        TeX-DVI-via-PDFTeX nil))

;;;; --- Text-mode ---
;; visual-line-mode only pretends to insert linebreaks
(remove-hook 'text-mode-hook
             'turn-on-auto-fill)
(add-hook    'text-mode-hook
             'turn-on-visual-line-mode)

;;;; --- Ediff ---
;; Ignore whitespace, no popup-window and split horizontally
(add-hook 'ediff-before-setup-hook
          (lambda ()
            (setq ediff-diff-options "-w"
                  ediff-window-setup-function 'ediff-setup-windows-plain
                  ediff-split-window-function 'split-window-horizontally)))
(with-eval-after-load 'ediff
  (set-face-background 'ediff-odd-diff-B "Grey60"))

;;;; --- HTML/CSS ---
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode . rainbow-mode)))

;;;; --- CSV ---
(setq csv-separators (quote (";")))

;;;; --- Haskell ---
(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode-hook . turn-on-haskell-indent))
  :init
  (use-package intero
    :ensure t
    :hook ((haskell-mode-hook . intero-mode)))
  :config
  (setq haskell-indent-spaces 4)
    ;; (use-package flycheck-haskell ;; Should be part of intero
    ;;   :ensure t
    ;;   :hook ((haskell-mode-hook . flycheck-haskell-setup)))
    )

;;;; --- C/C++ ---
(defun common-c-hook ()
  "Hook for C/C++."
  (c-set-style "bsd")
  (setq c-basic-offset 2
        tab-width 2)
  ;;(require 'semantic/bovine/gcc)
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

;;;; --- Java ---
(add-hook 'java-mode-hook
          'my-semantic-hook)

;;;; --- Magit ---
(use-package magit
  :ensure t
  :defer 1
  ;; Magit turns on auto-revert so a file changed on disk is changed in Emacs
  ;; This could be an issue...
  :diminish auto-revert-mode
  :bind (("C-x g"   . magit-status)          ; Display the main magit popup
         ("C-x M-g" . magit-dispatch-popup)) ; Display keybinds for magit
  :init
  ;; Remove the startup message about turning on auto-revert
  (setq magit-no-message (list "Turning on magit-auto-revert-mode..."))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))


;;;; --- Fish ---
(use-package fish-mode
  :defer t
  :ensure t)

;;;; --- ESS - Emacs Speaks Statistics ---
(use-package ess
  :ensure t
  :defer t)

;;;; --- Stan ---
(use-package stan-mode
  :ensure t
  :defer t
  :config
  (use-package stan-snippets
    :defer t
    :ensure t))

;;;; --- Python ---
;; python -m pip install --upgrade jedi rope black flake8 yapf autopep8 elpy
(use-package elpy
  :ensure t
  :pin elpy
  :defer t
  :hook ((python-mode . (lambda ()
                          (if (or (eq system-type 'windows-nt)
                                  (eq system-type 'ms-dos))
                              (setq python-shell-completion-native-disabled-interpreters
                                    '("python")))
                          (elpy-mode t)))
         (inferior-python-mode . (lambda ()
                                   (python-shell-switch-to-shell))))
  :init
  ;; Silence warning when guessing indent, default is 4 spaces
  (setq python-indent-guess-indent-offset-verbose nil)
  (with-eval-after-load 'python
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_"))))
  :config
  (setq elpy-shell-use-project-root nil
        elpy-rpc-backend "jedi"
        python-shell-interpreter "python3")
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
                      'elpy-modules))))

;;;; --- Ocaml ---
(use-package tuareg
  :ensure t
  :defer t
  :config
  (use-package merlin
    :ensure t
    :hook ((tuareg-mode . merlin-mode))
    :config
    (use-package flycheck-ocaml
      :ensure t
      :config
      (flycheck-ocaml-setup))))

;;;; --- EPA ---
(defun my/setup-epa ()
  "Quick setup for EPA."
  ;; These allow entry of passphrase in emacs
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;;; --- Twitter ---
(use-package twittering-mode
  :ensure t
  :defer t
  :config
  (my/setup-epa)
  (setq twittering-use-master-password t
        twittering-icon-mode           t))

;;;; --- Outline ---
;; For elisp:
;; - ;;; is a headline
;; - ;;;; is on the same level as a top-level sexp
(use-package outline-magic
  :ensure t
  :hook ((prog-mode . outline-minor-mode))
  ;; Always enabled, do not show in mode-line
  :diminish outline-minor-mode
  :bind        (("<C-tab>" . outline-cycle))
  :bind-keymap (("C-z"     . outline-mode-prefix-map)))

;;;; --- Frame-setup ---
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

;; Frame resizing and theme
(cond ((display-graphic-p) ;; Window system
       (setq frame-resize-pixelwise t)
       (my/set-normal-frame))
      (t ;; Terminal
       (use-package hc-zenburn-theme
         :ensure t
         :config
         (load-theme 'hc-zenburn t))))
(set-face-background 'cursor           "burlywood")

;;;; --- System specific ---
(cond
 ;; --- Windows specific ---
 ((eq system-type 'windows-nt)
  ;; Default directory
  (let ((desktop-dir (concat "C:/Users/"
                             (user-login-name)
                             "/Desktop/")))
    (setq default-directory desktop-dir)
    (set-register ?d (cons 'file desktop-dir)))

  ;; Alt-enter toggles screensize
  (defmacro handle-fullscreen-mode (func)
    `(progn
       (when *fullscreen-set*
         (toggle-frame-fullscreen)
         (setq *fullscreen-set* nil))
       (,func)))
  (defvar *fullscreen-set* nil)
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

  ;; --- Tramp - Windows ---
  ;; C-x C-f /plink:<user>@host: ENTER
  (let ((plink-file "C:/Program Files (x86)/PuTTY/plink.exe"))
    (when (file-exists-p plink-file)
      (setq tramp-default-method "plink")
      (when (not (string-match plink-file
                               (getenv "PATH")))
        (setenv "PATH" (concat plink-file
                               ";"
                               (getenv "PATH")))
        (add-to-list 'exec-path
                     plink-file))))

  ;;;;; --- Work specific ---
  (when (and (eq system-type 'windows-nt)
             (equal (user-login-name) "sisto"))
    (use-package cobol-mode
      :ensure t
      :defer t
      :init
      (setq auto-mode-alist
            (append
             '(("\\.cob\\'" . cobol-mode)
               ("\\.cbl\\'" . cobol-mode)
               ("\\.cpy\\'" . cobol-mode))
             auto-mode-alist)))))

 ;; --- Linux specific ---
 ((eq system-type 'gnu/linux)
  ;; --- Tramp - Linux ---
  (setq tramp-default-method "ssh")

  ;; --- Mu4e ---
  (when (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
    (use-package mu4e
      :defer t
      :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
      :bind (("C-c q" . mu4e))
      :config
      (my/setup-epa)
      ;; Authinfo - open in emacs and add lines for each context, e.g.
      ;; machine <smtp.foo.com> login <mail@address.com> password <secret> port <587>
      (add-to-list 'auth-sources
                   "~/.mail/.smtp-auth.gpg")
      (setq mu4e-maildir "~/.mail"
            ;; gpg-agent is set to use pinentry-qt for a dialog box
            mu4e-get-mail-command "mbsync -a"
            mu4e-view-show-images t
            ;; Why would I want to leave my message open after I've sent it?
            message-kill-buffer-on-exit t
            ;; Don't keep message buffers around
            message-kill-buffer-on-exit t
            ;; Don't ask for a 'context' upon opening mu4e
            mu4e-context-policy 'pick-first
            ;; Don't ask to quit... why is this the default?
            mu4e-confirm-quit nil
            ;; Fix "Duplicate UID" when moving messages
            mu4e-change-filenames-when-moving t
            mu4e-html2text-command 'mu4e-shr2text
            mu4e-completing-read-function 'completing-read
            ;; Header view
            mu4e-headers-time-format "%R"
            mu4e-headers-date-format "%F"
            ;; Common options for servers
            message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            ;; Set account-specific details here
            mu4e-contexts (list
                           (make-mu4e-context
                            :name "gmail"
                            :match-func (lambda (msg) (when msg
                                                   (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
                            :vars '((user-mail-address . "sstoltze@gmail.com")
                                    (mu4e-trash-folder . "/gmail/[Gmail].Trash")
                                    (mu4e-refile-folder . "/gmail/[Gmail].Archive")
                                    ;; Gmail handles sent messages for us
                                    (mu4e-sent-messages-behavior . delete)
                                    (smtpmail-default-smtp-server . "smtp.gmail.com")
                                    (smtpmail-smtp-server . "smtp.gmail.com")
                                    (smtpmail-smtp-service . 587)))
                           (make-mu4e-context
                            :name "work"
                            :match-func (lambda (msg) (when msg
                                                   (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
                            :vars '((user-mail-address . "sisto@sd.dk")
                                    (mu4e-trash-folder . "/work/Deleted Items")
                                    (mu4e-refile-folder . exchange-mu4e-refile-folder)
                                    ;; Exchange does not handle this for us
                                    (mu4e-sent-messages-behavior . sent)
                                    (smtpmail-default-smtp-server . "smtp.office365.com")
                                    (smtpmail-smtp-server . "smtp.office365.com")
                                    (smtpmail-smtp-service . 587)
                                    (mu4e-compose-signature . (concat "\n"
                                                                      "Venlig hilsen\n"
                                                                      "\n"
                                                                      "Simon Stoltze\n"
                                                                      "Developer\n"
                                                                      "Silkeborg Data A/S"))))))
      ;; Include a bookmark to open all of my inboxes
      (add-to-list 'mu4e-bookmarks
                   (make-mu4e-bookmark
                    :name "All Inboxes"
                    :query "maildir:/work/Inbox OR maildir:/gmail/Inbox"
                    :key ?i))
      (add-to-list 'mu4e-bookmarks
                   (make-mu4e-bookmark
                    :name "Gmail"
                    :query "maildir:/gmail/Inbox"
                    :key ?g)
                   t)
      (add-to-list 'mu4e-bookmarks
                   (make-mu4e-bookmark
                    :name "Work"
                    :query "maildir:/work/Inbox"
                    :key ?e)
                   t)
      ;; Headers to see which account a mail is stored in
      (add-to-list 'mu4e-header-info-custom
                   '(:account . (:name "Account"
                                       :shortname "Account"
                                       :help "The account/folder the mail was in."
                                       :function (lambda (msg)
                                                   (let ((path (or (mu4e-message-field msg :maildir)
                                                                   "")))
                                                     (if (string= path "")
                                                         "Mail file is not accessible"
                                                       (nth 1 (split-string path "/"))))))))
      ;; Add the new header to headers
      (add-to-list 'mu4e-headers-fields
                   '(:account . 8))
      (use-package mu4e-alert
        :ensure t
        :init
        (setq mu4e-alert-interesting-mail-query
              (concat
               "flag:unread maildir:/work/Inbox"
               " OR "
               "flag:unread maildir:/Gmail/Inbox")
              mu4e-alert-email-notification-types '(count))
        (mu4e-alert-enable-mode-line-display)
        (defun gjstein-refresh-mu4e-alert-mode-line ()
          (interactive)
          (mu4e~proc-kill)
          (mu4e-alert-enable-mode-line-display))
        ;; Refresh every 10 minutes
        (run-with-timer 600 600 'gjstein-refresh-mu4e-alert-mode-line))))

  ;; --- SAGE ---
  (when (file-directory-p "/usr/lib/sagemath")
    (use-package sage
      :defer t
      :load-path "/usr/lib/sagemath/local/share/emacs"
      :config
      (setq sage-command "/usr/lib/sagemath/sage")))))

(provide 'init)
;;; init.el ends here
