if status is-interactive
    function postexec_newline --on-event fish_postexec
        echo
    end

    set -U fish_greeting
end

# PATH
fish_add_path -p ~/.bin
fish_add_path -p ~/.local/bin
fish_add_path -p ~/.local/share/nvm/v20.18.2/bin
fish_add_path -p ~/.config/emacs/bin
fish_add_path -p ~/src/flutter/bin
fish_add_path -p ~/.pub-cache/bin

# Abbreviations
abbr -a aci arc commit
abbr -a aco arc checkout
abbr -a acob arc checkout -b
abbr -a gci git commit
abbr -a gco git checkout
abbr -a gcob git checkout -b

# Aliases
alias dotdrop '~/.dotfiles/dotdrop.sh --cfg=~/.dotfiles/config.yaml'
alias nv nvim
