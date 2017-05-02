(when (locate-library "migemo")
  (setq migemo-command "/usr/local/bin/cmigemo") ; HERE cmigemoバイナリ
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))
