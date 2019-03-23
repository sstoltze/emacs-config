
;;;; --- Paredit ---
;; http://pub.gajendra.net/src/paredit-refcard.pdf
;; (use-package paredit
;;   :ensure t
;;   :defer t
;;   :hook ((emacs-lisp-mode                  . enable-paredit-mode)
;;          (eval-expression-minibuffer-setup . enable-paredit-mode)
;;          (ielm-mode                        . enable-paredit-mode)
;;          (lisp-mode                        . enable-paredit-mode)
;;          (lisp-interaction-mode            . enable-paredit-mode)
;;          (scheme-mode                      . enable-paredit-mode)
;;          (clojure-mode                     . enable-paredit-mode)
;;          (cider-repl-mode                  . enable-paredit-mode)
;;          (racket-mode                      . enable-paredit-mode)))


;; racket-mode
:hook
(racket-mode . racket-unicode-input-method-enable)

;; LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (prog-mode . lsp)
  :config
  (use-package lsp-ui
    :ensure t))

;; Save history
(use-package savehist
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables    '(kill-ring
                                      search-ring
                                      regexp-search-ring))
  (savehist-file                    "~/.emacs.d/savehist")
  :init
  (savehist-mode 1))

;;;; --- Hydra ---
(use-package hydra
  :ensure t
  :init
  (define-prefix-command 'sstoltze/hydra-map)
  (global-set-key (kbd "C-c h") 'sstoltze/hydra-map)
  :config
  ;; Marking and movement
  (defhydra hydra-movement (nil nil
                                :color pink ;; Can only quit by pressing q
                                :pre   (linum-mode 1)
                                :post  (linum-mode -1)
                                :hint  nil)
    "
^  Characters        Words           Lines                  Buffer
----------------------------------------------------------------------
_f_ Forward         _F_ Forward       _n_ Next                 _v_ Scroll up
_b_ Backward        _B_ Backwards     _p_ Previous             _V_ Scroll down
                                  _a_ Beginning            _>_ End
                                  _e_ End                  _<_ Beginning
                                  _g_ Goto line            _l_ Recenter
"
    ("g" avy-goto-line)
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("F" forward-word)
    ("b" backward-char)
    ("B" backward-word)
    ("a" my/smarter-move-beginning-of-line)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("l" recenter-top-bottom)
    ("m" set-mark-command "mark")
    ("q" nil "quit"))
  (define-key sstoltze/hydra-map (kbd "m") 'hydra-movement/body)
  ;; Apropos
  (defhydra hydra-apropos (:color blue)
    "Apropos"
    ("a" counsel-apropos       "apropos")
    ("c" apropos-command       "cmd")
    ("d" apropos-documentation "doc")
    ("e" apropos-value         "val")
    ("l" apropos-library       "lib")
    ("o" apropos-user-option   "option")
    ("u" apropos-user-option   "option")
    ("v" apropos-variable      "var")
    ("i" info-apropos          "info")
    ("t" xref-find-apropos     "tags"))
  (define-key sstoltze/hydra-map (kbd "a") 'hydra-apropos/body)
  ;; Ediff
  (defhydra hydra-ediff (:color blue :hint nil)
    "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
    ("b" ediff-buffers)
    ("B" ediff-buffers3)
    ("=" ediff-files)
    ("f" ediff-files)
    ("F" ediff-files3)
    ("c" ediff-current-file)
    ("r" ediff-revision)
    ("l" ediff-regions-linewise)
    ("w" ediff-regions-wordwise)
    ("q" nil "quit"))
  (define-key sstoltze/hydra-map (kbd "d") 'hydra-ediff/body)
  ;; Flycheck
  (defhydra hydra-flycheck
    (nil nil
         :pre (progn (setq hydra-lv t)
                     (flycheck-list-errors))
         :post (progn (setq hydra-lv nil)
                      (quit-windows-on "*Flycheck errors*"))
         :hint nil)
    "Errors"
    ("c" flycheck-buffer                                           "Check")
    ("f" flycheck-error-list-set-filter                            "Filter")
    ("n" flycheck-next-error                                       "Next")
    ("p" flycheck-previous-error                                   "Previous")
    ("<" flycheck-first-error                                      "First")
    (">" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil                                                       "Quit"))
  (define-key sstoltze/hydra-map (kbd "!") 'hydra-flycheck/body)
  ;; Org
  (with-eval-after-load 'org
    (defhydra hydra-global-org (:color blue)
      "Org"
      ("t" org-timer-start "Start Timer")
      ("s" org-timer-stop "Stop Timer")
      ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
      ("p" org-timer "Print Timer")     ; output timer value to buffer
      ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
      ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
      ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
      ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
      ("l" org-capture-goto-last-stored "Last Capture"))
    (define-key sstoltze/hydra-map (kbd "c") 'hydra-global-org/body))
  ;; Outline
  (with-eval-after-load 'outline
    (defhydra hydra-outline (:color pink :hint nil)
      "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_h_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree
"
      ;; Hide
      ("h" outline-hide-sublevels) ; Hide everything but the top-level headings
      ("t" outline-hide-body) ; Hide everything but headings (all body lines)
      ("o" outline-hide-other)          ; Hide other branches
      ("c" outline-hide-entry)          ; Hide this entry's body
      ("l" outline-hide-leaves) ; Hide body lines in this entry and sub-entries
      ("d" outline-hide-subtree) ; Hide everything in this entry and sub-entries
      ;; Show
      ("a" outline-show-all)            ; Show (expand) everything
      ("e" outline-show-entry)          ; Show this heading's body
      ("i" outline-show-children) ; Show this heading's immediate child sub-headings
      ("k" outline-show-branches) ; Show all sub-headings under this heading
      ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
      ;; Move
      ("u" outline-up-heading)               ; Up
      ("n" outline-next-visible-heading)     ; Next
      ("p" outline-previous-visible-heading) ; Previous
      ("f" outline-forward-same-level)       ; Forward - same level
      ("b" outline-backward-same-level)      ; Backward - same level
      ("TAB" outline-cycle "cycle")
      ("q" nil "quit"))
    (define-key sstoltze/hydra-map (kbd "o") 'hydra-outline/body))
  (with-eval-after-load 'cider
    (use-package cider-hydra
      :ensure t
      :config
      (defhydra cider-hydra-top (:color blue :exit t)
        "cider-hydra"
        ("d" cider-hydra-doc/body "Doc")
        ("e" cider-hydra-eval/body "Eval")
        ("r" cider-hydra-repl/body "REPL")
        ("t" cider-hydra-test/body "Test"))
      (define-key sstoltze/hydra-map (kbd "j") 'cider-hydra-top/body))))


;; org
("C-c b" . org-iswitchb)
;; At work
  (when (and at-work
             (file-exists-p "C:/Progra~2/LibreOffice/program/soffice.exe"))
    (with-eval-after-load 'ox-odt
      ;; Export to .docx
      (setq org-odt-preferred-output-format "docx"
            org-odt-convert-processes '(("LibreOffice" "C:/Progra~2/LibreOffice/program/soffice.exe --headless --convert-to %f%x --outdir %d %i")))))

;; org-tempo is untested
(require 'org-tempo)
;; Usage is as for SRC and EXAMPLE blocks, <pr<TAB> to expand
(add-to-list 'org-structure-template-alist ;; A property drawer with correct settings for org-babel
             '("pr" ":PROPERTIES:\n:header-args: :results output :tangle yes :session *?*\n:END:"))
(add-to-list 'org-structure-template-alist ;; A source block with header-args for exporting an image
             '("si" "#+BEGIN_SRC ? :results graphics :file ./images/\n\n#+END_SRC"))
(add-to-list 'org-structure-template-alist ;; A source block with silent enabled
             '("ss" "#+BEGIN_SRC ? :results silent\n\n#+END_SRC"))

;;;; --- Projectile ---
(use-package projectile
  :ensure t
  :defer t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien))

;;;; --- Ido ---
;;;;; Tips:
;;;;; C-p makes ido only match beginning of names
;;;;; While doing C-x C-f:
;;;;; - C-d will open dired
;;;;; - M-d will search in subdirs
;;;;; - M-m will create a subdirectory
(use-package ido
  :ensure t
  :config
  (progn
    (setq ido-everywhere t
          ido-max-directory-size 100000
          ;; Use the current window when visiting files and buffers with ido
          ido-default-file-method 'selected-window
          ido-default-buffer-method 'selected-window
          ido-enable-flex-matching t
          ido-confirm-unique-completion t
          ;; Do not need to confirm when creating new buffer
          ido-create-new-buffer 'always
          ;; Ignore case when searching
          ido-case-fold t
          ;; Order files are shown in
          ido-file-extensions-order '(".org" ".py" ".el" ".emacs"
                                      ".lisp" ".c" ".hs" ".txt" ".R"))
    (ido-mode t)
    ;; Allow editing of read-only files
    (defun help/ido-find-file ()
      "Find file as root if necessary.

Attribution: URL `http://emacsredux.com/blog/2013/04/21/edit-files-as-root/'"
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

    (advice-add #'ido-find-file :after #'help/ido-find-file)))

;;;;; --- Work specific ---
  (when at-work
    (use-package cobol-mode
      :ensure t
      :defer t
      :mode "\\.cbl\\'"
      :mode "\\.cob\\'"))

(make-mu4e-context
                            :name "work"
                            :match-func (lambda (msg) (when msg
                                                   (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
                            :vars '((user-mail-address            . "sisto@sd.dk")
                                    (mu4e-trash-folder            . "/work/Deleted Items")
                                    (mu4e-refile-folder           . "/work/Archive")
                                    ;; Exchange does not handle sent messages for us
                                    (mu4e-sent-messages-behavior  . sent)
                                    (smtpmail-default-smtp-server . "smtp.office365.com")
                                    (smtpmail-smtp-server         . "smtp.office365.com")
                                    (smtpmail-smtp-service        . 587)
                                    (mu4e-compose-signature       . (concat "\n"
                                                                            "Venlig hilsen\n"
                                                                            "\n"
                                                                            "Simon Stoltze\n"
                                                                            "Developer\n"
                                                                            "Silkeborg Data A/S"))))
(add-to-list 'mu4e-bookmarks
                   (make-mu4e-bookmark
                    :name "Work"
                    :query "maildir:/work/Inbox"
                    :key ?e)
                   t)


;; Ivy
;; Work specific views
                     (when at-work
                       (list '("sas {}"
                               (horz
                                (file "C:/Users/sisto/Desktop/noter/dw/sas/noter.org")
                                (vert
                                 (file "C:/Users/sisto/Desktop/noter/dw/sas/servere.org")
                                 (file "C:/Users/sisto/Desktop/noter/dw/sas/scripts.org"))))
                             '("noter {}"
                               (file "C:/Users/sisto/Desktop/noter/"))))
