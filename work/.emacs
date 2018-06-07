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
 '(csv-separators (quote (";")))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "693f5a81a3728c2548efb4118c81941933cf0f7b614f9f3133101395e5830152" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "935cc557b01242fc7b4d3f803902d14d1b3afae5123624a2f924255f641f7f01" "7ce5ae5476aadfa57ffbfffd41c2d3f4aaa4e7f21de6646a76f10b2a7eaa105b" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "007b69ffec046a5842e34fea287b23c49175dfd6c6d5a0d9cdf150a2e8a8979f" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-format nil)
 '(display-time-mode t)
 '(display-time-use-mail-icon nil)
 '(doc-view-continuous t)
 '(excorporate-configuration
   (quote
    ("sisto@eg.dk" . "https://outlook.office365.com/EWS/Exchange.asmx")))
 '(fci-rule-color "#073642")
 '(haskell-indent-spaces 4)
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
 '(ido-confirm-unique-completion t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/noter.org" "~/calendar.org" "~/organizer.org" "c:/Users/sisto/Desktop/noter/test/test.org" "c:/Users/sisto/Desktop/noter/test/cl-shorter.org" "c:/Users/sisto/Desktop/noter/straksvalidering/programmer.org" "c:/Users/sisto/Desktop/noter/rpa/fkm-randers-noter.org" "c:/Users/sisto/Desktop/noter/rpa/aalborg/spørgsmaal.org" "c:/Users/sisto/Desktop/noter/rpa/aalborg/noter.org" "c:/Users/sisto/Desktop/noter/rpa/aalborg/møder.org" "c:/Users/sisto/Desktop/noter/forv-fods/opbygning-ka0.org" "c:/Users/sisto/Desktop/noter/forv-fods/noter.org" "c:/Users/sisto/Desktop/noter/forv-fods/møde-2018-01-29.org" "c:/Users/sisto/Desktop/noter/forv-fods/møde-2017-11-13.org" "c:/Users/sisto/Desktop/noter/forv-fods/møde-2017-11-08.org" "c:/Users/sisto/Desktop/noter/forv-fods/komp-til-sdpw-og-b109.org" "c:/Users/sisto/Desktop/noter/fejl/udbetaling-af-ferie-tilbageførsel-2017-11-01.org" "c:/Users/sisto/Desktop/noter/fejl/pro3464-dw-især-910-929-2018-02-01.org" "c:/Users/sisto/Desktop/noter/fejl/planlagt-udbetalt-ferie-kontovisning-2017-11-28.org" "c:/Users/sisto/Desktop/noter/fejl/Done/udbetaling-af-ferie-knap-2018-01-10.org" "c:/Users/sisto/Desktop/noter/fejl/Done/syl0534-2017-11-09.org" "c:/Users/sisto/Desktop/noter/fejl/Done/Srf0flb-dagplejere-2017-12-15/srf0flb-2017-12-15.org" "c:/Users/sisto/Desktop/noter/fejl/Done/kg-bo-lyngholm-andersen-2017-11-06.org")))
 '(org-capture-templates
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
 '(org-ellipsis "…")
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-startup-with-inline-images t)
 '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (flycheck irony fish-completion fish-mode io-mode io-mode-inf magit auto-complete htmlize 2048-game csv-mode csv auctex paperless pdf-tools org-babel-eval-in-repl slime excorporate org-outlook eww-lnum org use-package gnugo)))
 '(show-paren-mode t)
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
 '(tool-bar-mode nil)
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
 '(cursor ((t (:background "forest green")))))

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq select-enable-clipboard t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives
;             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; Brug zenburn fra terminal
;(unless (display-graphic-p)
;  (load-theme (quote hc-zenburn)))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(setq next-line-add-newlines t)

;; SAGE
;(use-package sage
;  :load-path "/usr/lib/sagemath/local/share/emacs"
;                                        ;  :init
;                                        ;  (add-to-list 'load-path (expand-file-name "/usr/lib/sagemath/local/share/emacs"))
;  :config
;  (setq sage-command "/usr/lib/sagemath/sage"))
;(require 'sage "sage")

;; If you want sage-view to typeset all your output and have plot()
;; commands inline, uncomment the following line and configure sage-view:
;; (require 'sage-view "sage-view")
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view)
;; You can use commands like
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-output)
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view-disable-inline-plots)
;; to enable some combination of features.  Using sage-view requires a
;; working LaTeX installation with the preview package.

(require 'org-install)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-default-notes-file "~/organizer.org")
(set-register ?o (cons 'file "~/organizer.org"))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode)
                             (org-indent-mode)))

(load-library "find-lisp")
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

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Macaulay 2 start
;(load "~/.emacs-Macaulay2" t)
;; Macaulay 2 end

;(use-package iso-transl)

(add-hook 'css-mode-hook 'rainbow-mode)

;(use-package twittering-mode
;  :load-path "~/Documents/git/twittering-mode/"
;  :init
;                                        ; (add-to-list 'load-path )
;                                        ; (require 'twittering-mode)
;  :config
;  (setq twittering-use-master-password t)
;  (setq twittering-icon-mode t))

(use-package ido
  :init
  (progn
    (ido-mode t)
    (setq ido-everywhere t)
    (setq ido-max-directory-size 100000)
    (ido-mode (quote both))
    ;; Use the current window when visiting files and buffers with ido
    (setq ido-default-file-method 'selected-window)
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-enable-flex-matching t)))

;; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

(setq-default indent-tabs-mode nil)

(use-package slime
  :init
  (progn
    (setq inferior-lisp-program "sbcl")
    (setq slime-default-lisp "sbcl")
    (setq slime-contribs '(slime-fancy))))

;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

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

(setq default-directory "C:/Users/sisto/Desktop/")

(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)

;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;(use-package mu4e)

(require 'ob)

;; make org mode allow eval of some langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (lisp . t)
   (emacs-lisp . t)
   (python . t)
   (ruby . t)))
(setq org-confirm-babel-evaluate nil)

(if (not (eq system-type 'cygwin))
    (use-package magit
      :ensure t
      :bind (("C-x g" . magit-status)     ; Display the main magit popup
             ("C-x M-g" . magit-dispatch-popup))) ; Display keybinds for magit
  )

(put 'upcase-region 'disabled nil)

(use-package fish-mode :ensure t)

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

; Numbered lines
;(setq-default linum-format "%3d")
;(add-hook 'prog-mode-hook 'linum-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(semantic-mode 1)
