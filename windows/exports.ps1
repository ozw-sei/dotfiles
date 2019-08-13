# Make vim the default editor
Set-Environment "EDITOR" "nvim --nofork"
Set-Environment "GIT_EDITOR" $Env:EDITOR

# Disable the Progress Bar
$ProgressPreference='SilentlyContinue'
