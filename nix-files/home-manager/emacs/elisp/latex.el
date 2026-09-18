(use-package tex
  :ensure auctex
  :defer t
  :hook ((LaTeX-mode . turn-on-auto-fill)
         (LaTeX-mode . TeX-source-correlate-mode))
  :custom
  (TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (TeX-view-program-selection
   '(((output-dvi style-pstricks) "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open")))
  ;; Not sure if this belongs here
  (doc-view-continuous t))
