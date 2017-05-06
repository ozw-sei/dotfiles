(require 'helm)

(helm-migemo-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-for-files)

(global-set-key (kbd "M-z") 'zop-up-to-char)

;; backup を作らない
(setq make-backup-files nil)

;; yasnippet の設定

(yas-global-mode 1)

(auto-insert-mode 1)

;; helmでripgrep検索する
(setq helm-ag-base-command "rg --vimgrep --no-heading")

(global-set-key (kbd "C-u") 'helm-ag)
