;; undo-treeモードの設定
(require 'undo-tree)
(global-set-key (kbd "C-c u") 'undo-tree-visualize)

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

