; ace-jump の設定
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-word-mode-use-query-char nil)

(setq ace-jump-mode-move-keys
      (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))


(global-set-key (kbd "C-o") 'ace-jump-word-mode)
