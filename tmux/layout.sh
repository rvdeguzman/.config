#!/usr/bin/env bash
# tmux layout: 3 windows with custom pane splits
# Session name is set automatically by session-created hook in tmux.conf

SESSION="$(tmux new-session -d -n "main" -P -F '#{session_id}')"

# Window 1: 80/20 left-right split
tmux split-window -h -t "$SESSION" -l 20%

# Window 2: 1 left, 2 right stacked
tmux new-window -t "$SESSION" -n "split"
tmux split-window -h -t "$SESSION"
tmux split-window -v -t "$SESSION"

# Window 3: 4 panes in quadrants
tmux new-window -t "$SESSION" -n "quad"
tmux split-window -h -t "$SESSION"
tmux select-pane -t "$SESSION" -L
tmux split-window -v -t "$SESSION"
tmux select-pane -t "$SESSION" -R
tmux split-window -v -t "$SESSION"

# Focus window 1, left pane
tmux select-window -t "$SESSION:1"
tmux select-pane -t "$SESSION" -L

tmux attach-session -t "$SESSION"
