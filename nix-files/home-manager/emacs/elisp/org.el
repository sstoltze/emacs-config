(use-package autoinsert
  ;; Only do it for org-mode
  :hook ((org-mode . auto-insert))
  :custom
  ;; Insert into file, but mark unmodified
  (auto-insert       'other)
  ;; Do not ask when inserting
  (auto-insert-query nil)
  :config
  (add-to-list 'auto-insert-alist
               '(("\\.org\\'" . "Org header")
                 nil
                 "#+AUTHOR: " user-full-name n
                 "#+EMAIL: "  user-mail-address n
                 "#+DATE: "   (format-time-string "%Y-%m-%d" (current-time)) n
                 "#+OPTIONS: toc:nil title:nil author:nil email:nil date:nil creator:nil" n)))


(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-agenda-mode . hl-line-mode)
         (org-babel-after-execute . org-display-inline-images)
         (org-clock-in . (lambda ()
                           ;; Start timer, use default value, replace any running timer
                           (org-timer-set-timer '(16)))))
  :diminish org-indent-mode
  :diminish visual-line-mode
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture) ;; counsel-org-capture requires more keypresses
         ("C-c a" . org-agenda)
         ;; Use counsel for org tag selection (C-c C-q)
         ([remap org-set-tags-command] . counsel-org-tag))
  :custom
  ;; Startup
  (org-ellipsis                           "…")
  (org-startup-folded                     nil)
  (org-startup-indented                   t)
  (org-startup-with-inline-images         t)
  ;; Use the current window for most things
  (org-agenda-window-setup                'current-window)
  (org-agenda-restore-windows-after-quit  t)
  (org-agenda-start-on-weekday            nil)
  (org-agenda-skip-deadline-if-done       t)
  (org-agenda-skip-scheduled-if-done      t)
  (org-indirect-buffer-display            'current-window)
  ;; Export
  (org-export-backends                    '(ascii beamer html icalendar latex md odt))
  ;; Author, email, date of creation, validation link at bottom of exported html
  (org-html-postamble                     nil)
  (org-html-html5-fancy                   t)
  (org-html-doctype                       "html5")
  ;; Todo
  (org-todo-keywords                      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s/!)" "|" "DONE(d/!)")
                                            (sequence "WAITING(w@/!)" "|" "CANCELED(c@/!)")))
  (org-time-stamp-custom-formats          '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (org-use-fast-todo-selection            t)
  (org-enforce-todo-dependencies          t)
  (org-log-done                           'time)
  ;; Round clock to 5 minute intervals, delete anything shorter
  (org-clock-rounding-minutes             5)
  (org-log-note-clock-out                 t)
  ;; Allow editing invisible region if it does that you would expect
  (org-catch-invisible-edits              'smart)
  ;; Refile
  (org-refile-use-outline-path            'file)
  ;; Targets complete directly with Ivy
  (org-outline-path-complete-in-steps     nil)
  ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; Reverse note order
  (org-reverse-note-order                 t)
  ;; Pomodoro timer
  ;; Check in with C-c C-x C-i (or I on heading)
  ;; Check out with C-c C-x C-o (or O on heading)
  (org-timer-default-timer                25)
  ;; I'm not sure I like this
  ;; (org-hide-emphasis-markers              t)
  (org-src-fontify-natively               t))

(use-package org-tree-slide
  :ensure t
  :bind (("C-c i" . org-tree-slide-mode))
  :custom
  (org-tree-slide-slide-in-effect nil))
