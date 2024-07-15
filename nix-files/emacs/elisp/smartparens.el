(use-package smartparens
  :ensure t
  :defer t
  :diminish smartparens-mode
  :hook ((prog-mode . turn-on-smartparens-strict-mode))
  :bind ((:map smartparens-mode-map
               ("M-s"     . sp-splice-sexp)
               ("M-S"     . sp-split-sexp)
               ("M-J"     . sp-join-sexp)
               ("C-M-SPC" . sp-mark-sexp)
               ("C-("     . sp-backward-slurp-sexp)
               ("C-)"     . sp-forward-slurp-sexp)
               ("C-{"     . sp-backward-barf-sexp)
               ("C-}"     . sp-forward-barf-sexp)
               ("M-("     . sp-wrap-round)
               ("M-{"     . sp-wrap-curly)
               ("M-["     . sp-wrap-square)
               ("M-\""    . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
               ("C-M-q"   . sp-indent-defun)
               ("M-R"     . sp-raise-sexp)
               ("M-?"     . sp-convolute-sexp)
               ("C-M-u"   . sp-backward-up-sexp)
               ("C-M-n"   . sp-up-sexp)
               ("C-M-d"   . sp-down-sexp)
               ("C-M-p"   . sp-backward-down-sexp)
               ("C-x n s" . sp-narrow-to-sexp)))
  :custom
  (sp-highlight-pair-overlay nil)
  :config
  ;; Make smartparens work with M-:
  (setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  ;; Ensure ' works in lisps and does other setup
  (require 'smartparens-config))
