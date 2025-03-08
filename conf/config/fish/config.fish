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
abbr -a aa arc add
abbr -a aci arc commit
abbr -a aco arc checkout
abbr -a acob arc checkout -b
abbr -a ad arc diff
abbr -a ap arc pull
abbr -a apush arc push
abbr -a as arc status

abbr -a dc dotdrop compare
abbr -a di dotdrop install

abbr -a ga git add 
abbr -a gci git commit
abbr -a gco git checkout
abbr -a gcob git checkout -b
abbr -a gd git diff
abbr -a gp git pull
abbr -a gpush git push
abbr -a gs git status

# Aliases
alias dotdrop '~/.dotfiles/dotdrop.sh --cfg=~/.dotfiles/config.yaml'
alias nv nvim

# nvm
set -U nvm_default_version lts/jod
set -U nvm_data ~/.nvm

# Secrets
if test -f ~/.config/fish/secrets.fish
    source ~/.config/fish/secrets.fish
end
