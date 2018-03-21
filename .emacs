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
    ("40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "693f5a81a3728c2548efb4118c81941933cf0f7b614f9f3133101395e5830152" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "ac2b1fed9c0f0190045359327e963ddad250e131fbf332e80d371b2e1dbc1dc4" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "935cc557b01242fc7b4d3f803902d14d1b3afae5123624a2f924255f641f7f01" "7ce5ae5476aadfa57ffbfffd41c2d3f4aaa4e7f21de6646a76f10b2a7eaa105b" "108b3724e0d684027c713703f663358779cc6544075bc8fd16ae71470497304f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "007b69ffec046a5842e34fea287b23c49175dfd6c6d5a0d9cdf150a2e8a8979f" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(doc-view-continuous t)
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
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/Documents/tourdekontor/fangerne/oversigt.org")))
 '(package-selected-packages
   (quote
    (org zenburn-theme web-mode warm-night-theme w3 use-package sx shakespeare-mode rainbow-mode pdf-tools org-ac magit linum-relative latex-preview-pane io-mode-inf io-mode hc-zenburn-theme fish-mode diminish darkmine-theme darkburn-theme csharp-mode auto-complete-sage auto-complete-auctex afternoon-theme achievements ac-slime ac-math ac-ispell ac-html abyss-theme 2048-game)))
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
 )

(add-to-list 'load-path "~/.emacs.d/lisp")
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq x-select-enable-clipboard t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (display-graphic-p)
  (load-theme (quote hc-zenburn)))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(setq next-line-add-newlines t)

;; SAGE
(use-package sage
  :load-path "/usr/lib/sagemath/local/share/emacs"
                                        ;  :init
                                        ;  (add-to-list 'load-path (expand-file-name "/usr/lib/sagemath/local/share/emacs"))
  :config
  (setq sage-command "/usr/lib/sagemath/sage"))
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

(use-package org-install
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-cb" 'org-iswitchb)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))

;; Macaulay 2 start
(load "~/.emacs-Macaulay2" t)
;; Macaulay 2 end

(use-package iso-transl)

(add-hook 'css-mode-hook 'rainbow-mode)

(use-package twittering-mode
  :load-path "~/Documents/git/twittering-mode/"
  :init
                                        ; (add-to-list 'load-path )
                                        ; (require 'twittering-mode)
  :config
  (setq twittering-use-master-password t)
  (setq twittering-icon-mode t))

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(setq-default indent-tabs-mode nil)
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp "sbcl")
(setq slime-contribs '(slime-fancy))


(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

; http://whattheemacsd.com/buffer-defuns.el-02.html#disqus_thread
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
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
             (setq i (1+ i)))))))

(global-set-key (kbd "<C-tab>") 'rotate-windows)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(use-package mu4e)

