(require 'dired)
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")
(setq dired-details-hide-link-targets nil)

(global-set-key (kbd "C-M-D") 'dired-toggle)

(global-set-key (kbd "C-x C-b") 'bs-show)

(ffap-bindings)
