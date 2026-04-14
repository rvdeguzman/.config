#!/bin/bash

set -euo pipefail

AEROSPACE_BIN="${AEROSPACE_BIN:-aerospace}"

focused_workspace="$("$AEROSPACE_BIN" list-workspaces --focused --format '%{workspace}')"

move_app_windows() {
  local bundle_id="$1"
  local workspace="$2"
  local window_id

  while IFS= read -r window_id; do
    [[ -n "$window_id" ]] || continue
    "$AEROSPACE_BIN" move-node-to-workspace --window-id "$window_id" "$workspace"
  done < <("$AEROSPACE_BIN" list-windows --all --app-bundle-id "$bundle_id" --format '%{window-id}')
}

# Workspace 1: terminal / nvim
move_app_windows 'com.mitchellh.ghostty' '1'

# Workspace 2: Doom Emacs
move_app_windows 'org.gnu.Emacs' '2'
move_app_windows 'org.gnu.EmacsMac' '2'

# Workspace 3: Zed
move_app_windows 'dev.zed.Zed' '3'

# Workspace 4: browsers
move_app_windows 'com.apple.Safari' '4'
move_app_windows 'com.google.Chrome' '4'
move_app_windows 'org.chromium.Chromium' '4'
move_app_windows 'company.thebrowser.Browser' '4'
move_app_windows 'org.mozilla.firefox' '4'
move_app_windows 'com.brave.Browser' '4'
move_app_windows 'com.microsoft.edgemac' '4'

# Workspace 5: AI tools
move_app_windows 'com.openai.chat' '5'
move_app_windows 'com.anthropic.claudefordesktop' '5'

# Workspace 6: agentic work
move_app_windows 'com.cmuxterm.app' '6'

# Workspace 9: file management
move_app_windows 'com.apple.finder' '9'

# Workspace 10: communications
move_app_windows 'com.apple.MobileSMS' '10'
move_app_windows 'com.microsoft.teams' '10'
move_app_windows 'com.microsoft.teams2' '10'
move_app_windows 'us.zoom.xos' '10'
move_app_windows 'com.hnc.Discord' '10'

# Return to whatever workspace you were using before the reshuffle.
"$AEROSPACE_BIN" workspace "$focused_workspace"
