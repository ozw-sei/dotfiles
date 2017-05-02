(elpy-enable)

(defun turn-on-flycheck-mode ()
  (flycheck-mode 1))
(add-hook 'python-mode-hook 'turn-on-flycheck-mode)
