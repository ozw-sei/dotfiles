#!/bin/sh

author=`git config user.name`
today=`date +"%Y-%m-%d"`
log=`git log --since="$today 00:00:00" --no-merges --author=$author --oneline --pretty=format:'%C(yellow)%h %Cgreen%cr %Cblue%cn%Cred%d %Creset%s %C(cyan)%b\n'`

echo $log
