;; パッケージのp設定
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(require 'cask "$HOME/.cask/cask.el")
(cask-initialize)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC …でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; モードラインに時刻を表示する
(display-time)

;;; 行番号・桁番号を表示する
(line-number-mode 1)
(column-number-mode 1)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; make what-whereでSKK modulesで表示されるディレクトリを指定
(add-to-list 'load-path "/usr/local/share/emacs/24.5/site-lisp/skk")
;; M-x skk-tutorialでNo file found as 〜とエラーが出たときにskk-tut-fileを設定
;; make what-whereでSKK tutorialsで表示されるディレクトリ上のSKK.tutを指定
(setq skk-tut-file "/usr/share/skk/SKK.tut")
(require 'skk)
(global-set-key "\C-x\C-j" 'skk-mode)

(when (locate-library "migemo")
  (setq migemo-command "/usr/local/bin/cmigemo") ; HERE cmigemoバイナリ
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))


(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-word-mode-use-query-char nil)

(setq ace-jump-mode-move-keys
      (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))

(global-set-key (kbd "C-o") 'ace-jump-word-mode)

(global-set-key (kbd "C-@") 'dabbrev-expand)

(setq skk-dcomp-activate t)

(global-set-key (kbd "C-t") 'other-window)


(require 'popwin)
(popwin-mode 1)

(global-set-key [f12] 'eval-buffer)

(require 'dired)
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")
(setq dired-details-hide-link-targets nil)

(global-set-key (kbd "C-M-D") 'dired-toggle)

(ido-mode 1)
(ido-everywhere 1)

(global-set-key (kbd "C-x C-b") 'bs-show)

(ffap-bindings)


;; 最近のファイル500個を保存する
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを
;; 正規表現で指定する
(setq recentf-exclude
      '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(require 'helm)

(helm-migemo-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-for-files)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "M-z") 'zop-up-to-char)

(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)

(setq make-backup-files nil)

(yas-global-mode 1)

;;; スニペット名をidoで選択する
(setq yas-prompt-functions '(yas-ido-prompt))

(auto-insert-mode 1)

;; helmでripgrep検索する
(setq helm-ag-base-command "rg --vimgrep --no-heading")
