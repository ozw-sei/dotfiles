(require 'ctags nil t)
(setq tags-revert-without-query t)
;;(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
