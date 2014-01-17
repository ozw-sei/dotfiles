;; emacs 23以前の場合はDefaultディレクトリを変更
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; 文字コード
;; (set-language-enviroment "Japanese")
;; ツールバーを非表示
;; (tool-bar-mode -1)
;; メニューバーを非表示
;; (menu-bar-mode -1)
;; フォント
;;(fixed-width-set-fontset "monaco" 14)

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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'package)

; Add package-archives
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; ついでにmarmaladeも追加

; Initialize
(package-initialize)

;;自動バックアップを中止
(setq auto-save-default nil)

; undotree
(require 'undo-tree)
(define-key global-map (kbd "C-x C-u") 'undo-tree-visualize)

; maggit
(require 'magit)

; undohist
(require 'undohist)

;; php-mode
(require 'php-mode)

;; helm
(when (require 'helm-config)
  (helm-mode t)
  (setq helm-idle-delay 0.3))

(when (require 'helm-descbinds)
  ;; describe-bindings をhelmに変更
  (helm-descbinds-mode t))
  
(define-key global-map (kbd "C-x f") 'helm-for-files)
(define-key global-map (kbd "C-x l") 'helm-find-files)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring) 
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)

;;; popwin.el
(when (require 'popwin)
  (setq helm-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config '(("*compilation*" :noselect t)
					("helcm" :regexp t :height 0.4)
					)))
;; 
;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(define-key ac-completing-map (kbd "C-s") 'ac-next) ; M-nで次候補選択
(define-key ac-completing-map (kbd "C-r") 'ac-previous)  ; M-pで前候補選択
(ac-set-trigger-key "TAB")  ; TABで補完開始(トリガーキー)
  
(put 'set-goal-column 'disabled nil)

 ;; color-moccur
;; (when (require 'color-moccur)
;;   (define-key global-map (kbd "M-o") 'occur-by-moccur)
;;   ;スペースで区切られた複数の単語にマッチさせる
;;   (setq moccur-split-word t)
;;   ;;ディレクトリ検索の際に除外
;;   (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
;;   (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; helm-c-moccur
(when (require 'helm-c-moccur)
  (global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
  (global-set-key (kbd "C-M-o") 'helm-c-moccur-dmoccur)
  (global-set-key (kbd "C-s") 'helm-c-moccur-isearch-forward)
  (global-set-key (kbd "C-r") 'helm-c-moccur-isearch-backward))

;; python-mode
;;(require 'pyton-mode)

;; git-gutter
(when (require 'git-gutter+)
  (global-git-gutter+-mode t))
  


