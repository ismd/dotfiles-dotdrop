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
abbr -a dc dotdrop compare
abbr -a di dotdrop install
abbr -a gci git commit
abbr -a gco git checkout
abbr -a gcob git checkout -b
abbr -a gd git diff
abbr -a gp git pull
abbr -a gs git status

# Aliases
alias dotdrop '~/.dotfiles/dotdrop.sh --cfg=~/.dotfiles/config.yaml'
alias nv nvim

# nvm
set -U nvm_default_version lts/iron
