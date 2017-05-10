;; パッケージのp設定
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)


(package-initialize)

(require 'cask "$HOME/.cask/cask.el")
(cask-initialize)

(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

(add-to-list 'load-path "~/.emacs.d/elisp")


(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(load-theme 'tango)

;; elscreen
;; プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-q"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

(setenv "PYTHONPATH" "~/venv/bin/python")

(elpy-enable)

(defun turn-on-flycheck-mode ()
  (flycheck-mode 1))

(add-hook 'python-mode-hook 'turn-on-flycheck-mode)

(require 'ng2-mode)

; 必要なパッケージのロード
(require 'go-mode)
(require 'company-go)

;; 諸々の有効化、設定
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
           (add-hook 'before-save-hook' 'gofmt-before-save)
           (local-set-key (kbd "M-.") 'godef-jump)
           (set (make-local-variable 'company-backends) '(company-go))
           (company-mode)
           (setq indent-tabs-mode nil)    ; タブを利用
           (setq c-basic-offset 4)        ; tabサイズを4にする
           (setq tab-width 4)))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

 ;; ビープ音禁止
 (setq ring-bell-function 'ignore)

(require 'disable-mouse)
(global-disable-mouse-mode)


;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

;; C-sで絞り込む
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)

;; TABで候補を設定
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(define-key company-mode-map (kbd "C-;") 'company-complete)

(add-hook 'after-init-hook 'global-company-mode)

(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)

;; 自動改行しない
(setq auto-fill-mode 0)

;; 鬼軍曹
(require 'drill-instructor)
(setq drill-instructor-global t)

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(require 'tide)
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (company-mode-on)))

(exec-path-from-shell-initialize)

