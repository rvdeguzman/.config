#!/usr/bin/env bash
# tmux layout: 3 windows with custom pane splits
# Session name is set automatically by session-created hook in tmux.conf

SESSION="$(tmux new-session -d -n "main" -P -F '#{session_id}')"

# Window 2: 1 left, 2 right stacked
tmux new-window -t "$SESSION" -n "split"
tmux split-window -h -t "$SESSION"
tmux split-window -v -t "$SESSION"

  tmux send-keys -t "$SESSION:1.0" 'nvim' Enter

  # Window 2
  tmux send-keys -t "$SESSION:2.0" 'claude' Enter
  tmux send-keys -t "$SESSION:2.2" 'lazygit' Enter

# Focus window 1, left pane
tmux select-window -t "$SESSION:1"
tmux select-pane -t "$SESSION" -L

tmux attach-session -t "$SESSION"
