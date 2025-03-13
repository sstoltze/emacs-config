;; Fix nix remote paths when using tramp
(require 'tramp-sh)
 (setq tramp-remote-path
       (append tramp-remote-path
 	        '(tramp-own-remote-path)))
