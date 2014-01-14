;; emacs 23以前の場合はDefaultディレクトリを変更
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-pathを追加
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; elisp public_repo confのファイルを読み込む
(add-to-load-path "elisp" "public_repos" "conf") 
(require 'init-loader)

;; 設定ファイルを読み込む
(init-loader-load "~/.emacs.d/conf")

;; Macのみの文字コード変更
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
