if status is-interactive
    set -U fish_greeting
end

# PATH
fish_add_path -p ~/.bin
fish_add_path -p ~/.local/bin
fish_add_path -p ~/.config/emacs/bin
fish_add_path -p ~/src/flutter/bin
fish_add_path -p ~/.pub-cache/bin

# Abbreviations
abbr -a aci arc commit
abbr -a aco arc checkout
abbr -a acob arc checkout -b
abbr -a adi arc diff
abbr -a apl arc pull
abbr -a aps arc push
abbr -a ast arc status
abbr -a dco dotdrop compare
abbr -a din dotdrop install
abbr -a gci git commit
abbr -a gco git checkout
abbr -a gcob git checkout -b
abbr -a gdi git diff
abbr -a gpl git pull
abbr -a gps git push
abbr -a gst git status

# Aliases
alias dotdrop '~/.dotfiles/dotdrop.sh --cfg=~/.dotfiles/config.yaml'
alias nv nvim

# nvm
set -U nvm_default_version lts/iron
