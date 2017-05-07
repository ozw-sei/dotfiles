;; helm-ls-git

(require 'helm-ls-git)

(global-set-key (kbd "C-x C-d") 'helm-browse-project)

(define-key helm-map (kbd "C-h")  'delete-backward-char)
