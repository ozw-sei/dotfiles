;; C-mをインデント付き改行に変更
(define-key global-map (kbd "C-m") 'newline-and-indent)

;;C-hをバックスペースにする
(keyboard-translate ?\C-h ?\C-?)
