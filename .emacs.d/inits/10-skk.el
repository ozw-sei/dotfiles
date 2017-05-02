;;; 10-skk.el ---                                    -*- lexical-binding: t; -*-

;; make what-whereでSKK modulesで表示されるディレクトリを指定
(add-to-list 'load-path "/usr/local/share/emacs/24.5/site-lisp/skk")
;; M-x skk-tutorialでNo file found as 〜とエラーが出たときにskk-tut-fileを設定
;; make what-whereでSKK tutorialsで表示されるディレクトリ上のSKK.tutを指定
(setq skk-tut-file "/usr/share/skk/SKK.tut")
(require 'skk)
(global-set-key "\C-x\C-j" 'skk-mode)

(setq skk-dcomp-activate t)
