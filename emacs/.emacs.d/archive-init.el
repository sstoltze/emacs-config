
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

;;;; Ivy
;; Avy for finding matches in swiper, C-'
    (use-package avy
      :ensure t)
