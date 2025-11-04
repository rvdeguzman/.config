export ZSH="$HOME/.oh-my-zsh"

export PATH=$HOME/.local/bin/:$PATH
export PATH=$HOME/.local/share/bob/nightly/bin:$PATH
export PATH=$PATH:$HOME/bin/
export PATH=$PATH:$HOME/.config/emacs/bin

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
alias mkvenv="python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
alias venv="source venv/bin/activate"
alias ogh=open-github
alias rl=roamlink
