git branch -a --merged | grep remotes |awk '$0~/^[^\*]/ {print $1}'| while read branch; do echo $branch; git show --summary --pretty=format:'%aN (%aE)%n' $branch |head -n 2; done
