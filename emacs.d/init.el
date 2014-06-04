;; emacs 23以前の場合はDefaultディレクトリをつくれ
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; 文字コード
;; (set-language-enviroment "Japanese")

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
(unless (eq window-system 'ns)
  (menu-bar-mode 0))


(add-to-list 'load-path "~/.emacs.d/elisp/init-loader.el")
(add-to-load-path "elisp" "public_repos")
(require 'init-loader)

;; 設定ファイルを読み込む
;; (init-loader-load "~/.emacs.d/conf")

;; Macのみの文字コード変更
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'package)


(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t) ; ついでにmarmaladeも追加


(package-initialize)

;;自動バックアップを中止
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq completion-ignore-case t)
(global-auto-revert-mode 1)

					;undotree
(el-get 'sync 'undo-tree)
(require 'undo-tree)
(define-key global-map (kbd "C-x C-u") 'undo-tree-visualize)

					; maggit
(el-get 'sync 'magit)
(require 'magit)

					; undohist
(el-get 'sync 'undohist)
(require 'undohist)
(undohist-initialize)

;; php-mode
(el-get 'sync 'php-mode)
(require 'php-mode)

;; helm
(el-get 'sync 'helm)
(when (require 'helm-config)
  (helm-mode t)
  (setq helm-idle-delay 0.3))


;;;------------ Helm git grep ------------
(require 'helm-config)
(require 'helm-files)

;; List files in git repos
(defun helm-c-sources-git-project-for (pwd)
  (loop for elt in
	'(("Modified files" . "--modified")
	  ("Untracked files" . "--others --exclude-standard")
	  ("All controlled files in this project" . nil))
	for title  = (format "%s (%s)" (car elt) pwd)
	for option = (cdr elt)
	for cmd    = (format "git ls-files %s" (or option ""))
	collect
	`((name . ,title)
	  (init . (lambda ()
		    (unless (and (not ,option) (helm-candidate-buffer))
		      (with-current-buffer (helm-candidate-buffer 'global)
			(call-process-shell-command ,cmd nil t nil)))))
	  (candidates-in-buffer)
	  (type . file))))

(defun helm-git-project-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-git-project ()
  (interactive)
  (let ((topdir (helm-git-project-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let* ((default-directory topdir)
	   (sources (helm-c-sources-git-project-for default-directory)))
      (helm-other-buffer sources
			 (format "*helm git project in %s*" default-directory)))))
(define-key global-map (kbd "M-p") 'helm-git-project)

(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

(el-get 'sync 'helm-descbinds)
(when (require 'helm-descbinds)
  ;; describe-bindings をhelmに変更
  (helm-descbinds-mode t)
  (define-key global-map (kbd "C-x f") 'helm-for-files)
  (define-key global-map (kbd "C-x l") 'helm-find-files)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring) 
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-p") 'helm-project))


;;; popwin.el
(el-get 'sync 'popwin)
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilation*" :noselect t)
					("helcm" :regexp t :height 0.4))))

;; auto-complete
(el-get 'sync 'auto-complete)
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)


(define-key ac-completing-map (kbd "C-s") 'ac-next) ; C-sで次候補選択
(define-key ac-completing-map (kbd "C-r") 'ac-previous)  ; C-rで前候補選択
(ac-set-trigger-key "TAB")  ; TABで補完開始(トリガーキー)

(put 'set-goal-column 'disabled nil)

;; color-moccur
(el-get 'sync 'color-moccur)
(when (require 'color-moccur)
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
					;スペースで区切られた複数の単語にマッチさせる
  (setq moccur-split-word t)
  ;;ディレクトリ検索の際に除外
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; helm-c-moccu
(when (require 'helm-c-moccur)
  (global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
  (global-set-key (kbd "C-M-o") 'helm-c-moccur-dmoccur)
  (global-set-key (kbd "C-M-s") 'helm-c-moccur-isearch-forward)
  (global-set-key (kbd "C-M-r") 'helm-c-moccur-isearch-backward))


;; git-gutter
(el-get 'sync 'git-gutter)
(when (require 'git-gutter)
  (global-git-gutter-mode t))

;; theme

;;(load-theme 'sanityinc-solarized-dark t)

;; paredit
;; http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html
;(el-get 'sync 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; (require 'wgrep nil t)
(require 'jinja2-mode nil t)

;; 現在行をハイライト
(global-hl-line-mode 0)

;;; ファイル内のカーソル位置を保存する
(setq-default save-place t)

;;; 対応する括弧を表示させる
(show-paren-mode 1)
(el-get 'sync 'scala-mode2)
(require 'scala-mode2)
(el-get 'sync 'coffee-mode)
(require 'coffee-mode)
;; (el-get 'sync 'flymake-coffee)
;; (require 'flymake-coffee)
(el-get 'sync 'js3-mode)
(require 'js3-mode)
(require 'cl)

(el-get 'sync 'yasnippet)
(require 'yasnippet)
(yas/global-mode 1)

