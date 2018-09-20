function magit
    emacs --eval "(let ((pop-up-windows nil)) (call-interactively #'magit-status))"
end
