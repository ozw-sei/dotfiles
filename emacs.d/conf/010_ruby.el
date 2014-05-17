(setq ryby-indent-level 3
      ruby-deep-indent-paren-style nil)

(require 'ruby-electric nil t)
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(defun ruby-mode-hooks()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)
  
(defun flymake-ruby-init()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
			   'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.rb\\" flymake-ruby-init))

(add-to-list 'flymake-err-line-patterns
	     '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))
