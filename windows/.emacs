(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(backup-directory-alist (quote (("." . "F:/Program Files/emacs-24.3/backup"))))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(fringe-mode nil nil (fringe))
 '(inferior-lisp-program "clisp -i C:/Users/simon/.clisprc.lisp" t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (use-package fish-mode magit slime rainbow-mode org io-mode-inf io-mode haskell-mode csharp-mode auctex)))
 '(show-paren-mode t)
 '(temporary-file-directory "F:/Users/simon/AppData/Local/Temp/")
 '(tool-bar-mode nil)
 '(tooltip-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))

(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(require 'iso-transl)

(add-hook 'css-mode-hook 'rainbow-mode)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq-default indent-tabs-mode nil)

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

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-default-coding-systems  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)

;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;(use-package mu4e)

(require 'org)
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

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)     ; Display the main magit popup
         ("C-x M-g" . magit-dispatch-popup))) ; Display keybinds for magit

(put 'upcase-region 'disabled nil)

(use-package fish-mode :ensure t)

;; Unset suspend keys. Never used anyway
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(global-prettify-symbols-mode 1)
(setq prettify-symbols-unprettify-at-point 'right-edge)
