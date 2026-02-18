export ZSH="$HOME/.oh-my-zsh"

# Doom emacs path
export PATH=$HOME/.config/emacs/bin:$PATH
export PATH=$HOME/.local/bin/:$PATH
export PATH="$HOME/bin/:$PATH"

export EDITOR=nvim

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
alias vi="nvim"
alias ogh="open-github"
alias mkvenv="python3 -m venv .venv && source .venv/bin/activate && pip install -r requirements.txt"
alias venv="source .venv/bin/activate"

# alias school='cd /Users/rv/Library/Mobile\ Documents/com~apple~CloudDocs/school'
# alias icloud='cd /Users/rv/Library/Mobile\ Documents/com~apple~CloudDocs'

# ln -s "/Users/rv/Library/Mobile\ Documents/com~apple~CloudDocs/school ~/school"
# ln -s "/Users/rv/Library/Mobile\ Documents/com~apple~CloudDocs/ ~/icloud"
