(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! websocket)
(package! evil-easymotion :disable t)

(unpin! org-roam)
(unpin! websocket)

(package! kanagawa-themes)

(package! org-download)

(package! gptel)

(package! kitty-graphics
  :recipe (:host github :repo "cashmeredev/kitty-graphics.el"))

(package! leetcode
  :recipe (:host github :repo "kaiwk/leetcode.el"))

(package! flash
  :recipe (:host github :repo "Prgebish/flash"))
