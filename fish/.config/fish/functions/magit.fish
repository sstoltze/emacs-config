function magit
    emacs -nw --eval "(let ((pop-up-windows nil)) (call-interactively #'magit-status))"
end
