{
  programs.emacs = {
    extraConfig = ''
      (defconst sstoltze/racket--paren-shapes
        '( (?\( ?\[ ?\] )
           (?\[ ?\{ ?\} )
           (?\{ ?\( ?\) ))
        "This is not user-configurable because we expect them have to have actual ?\( and ?\) char syntax.")

      (defun sstoltze/racket-cycle-paren-shapes ()
        "In an s-expression, move to the opening, and cycle the shape among () [] {}.
      Stolen from racket-mode because I miss it."
        (interactive)
        (save-excursion
          (unless (eq ?\( (char-syntax (char-after)))
            (backward-up-list))
          (pcase (assq (char-after) sstoltze/racket--paren-shapes)
            (`(,_ ,open ,close)
             (delete-char 1)
             (insert open)
             (backward-char 1)
             (forward-sexp 1)
             (backward-delete-char 1)
             (insert close))
            (_
             (user-error "Don't know that paren shape")))))
      (use-package racket-mode
        :ensure t
        :defer t
        :hook ((racket-mode . racket-xp-mode)
               (racket-mode . font-lock-mode))
        :bind ((:map racket-mode-map
               ("C-c SPC" . racket-align))))

      (use-package scribble-mode
        :ensure t
        :defer t)
    '';

    extraPackages =
      epkgs: with epkgs; [
        racket-mode
        scribble-mode
      ];
  };
}
