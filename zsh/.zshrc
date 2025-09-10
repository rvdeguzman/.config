export ZSH="$HOME/.oh-my-zsh"

# Flutter path
export PATH=$HOME/Developer/flutter/bin:$PATH

# Doom emacs path
export PATH=$HOME/.emacs.d/bin:$PATH
export PATH=$HOME/.local/bin/:$PATH

# API Keys
export ANTHROPIC_API_KEY=
export GEMINI_API_KEY=
export OPENAI_API_KEY=

ZSH_THEME="geoffgarside"
plugins=(git)

source $ZSH/oh-my-zsh.sh

alias g="lazygit"
alias t="tmux"
alias y="yazi"
alias vim="nvim"
alias v="nvim"
alias mkvenv="python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
alias venv="source venv/bin/activate"

alias school='cd /Users/rv/Library/Mobile\ Documents/com~apple~CloudDocs/school'

. "$HOME/.atuin/bin/env"

eval "$(atuin init zsh)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/rv/.lmstudio/bin"
# End of LM Studio CLI section

. "$HOME/.local/bin/env"


# Load Angular CLI autocompletion.
source <(ng completion script)

# zoxide
eval "$(zoxide init zsh)"

# The following lines have been added by Docker Desktop to enable Docker CLI completions.
fpath=(/Users/rv/.docker/completions $fpath)
autoload -Uz compinit
compinit
# End of Docker CLI completions

export PATH="/Library/TeX/texbin:$PATH"

alias avante='nvim -c "lua vim.defer_fn(function()require(\"avante.api\").zen_mode()end, 100)"'
