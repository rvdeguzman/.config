;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; theme
(setq doom-theme 'doom-dark-funeral)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 24))
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org"
      org-roam-directory (file-truename "~/org/roam")
      org-agenda-files (list (file-truename "~/org/roam/dailies")))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(defun org-roam-dailies-capture-today-with-agenda ()
  "Capture today's daily and add it to org-agenda-files."
  (interactive)
  (org-roam-dailies-capture-today)
  (when (buffer-file-name)
    (unless (member (buffer-file-name) org-agenda-files)
      (setq org-agenda-files (append org-agenda-files (list (buffer-file-name)))))))

(defun org-roam-capture-project ()
  "Capture a new project and add it to org-agenda-files."
  (interactive)
  (org-roam-capture '(:keys "p"))
  (when (buffer-file-name)
    (unless (member (buffer-file-name) org-agenda-files)
      (setq org-agenda-files (append org-agenda-files (list (buffer-file-name)))))))

(setq org-roam-capture-templates
      '(("s" "subject" plain "%?"
         :if-new (file+head "subjects/${title}.org" "#+title: ${title}\n#+filetags: :subject:\n")
         :immediate-finish t :unnarrowed t)
        ("c" "concept" plain "%?"
         :if-new (file+head "concepts/${title}.org" "#+title: ${title}\n")
         :immediate-finish t :unnarrowed t)
        ("g" "goal" plain "%?"
         :if-new (file+head "goals/${title}.org" "#+title: ${title}\n#+filetags: :goal:\n")
         :immediate-finish t :unnarrowed t)
        ("p" "project" plain "* Elevator Pitch\n%?\n\n* Type\nSoftware / App / SaaS / Idea\n\n* Status\nActive / Paused / Completed / Idea\n\n* Tech Stack\n\n* Timesheet\n| Date | Hours | Notes |\n|------|-------|-------||\n\n* Next Steps\n\n* Notes\n"
         :if-new (file+head "projects/${title}.org" "#+title: ${title}\n#+filetags: :project:\n")
         :immediate-finish nil :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t :unnarrowed t)))

;; org-roam-dailies templates
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "** Habits\n- [ ] Fasting\n- [ ] Exercise\n- [ ] Work\n\n** Timesheet\n| Task | Hours | Notes |\n|------|-------|-------|\n|      |       |       |\n\n** Notes\n%?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n"))))

;; org-roam
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture)
          ("C-c n d" . org-roam-dailies-capture-today-with-agenda)
          ("C-c n p" . org-roam-capture-project))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))

(use-package! vterm
  :commands vterm)

(after! org
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options
        (plist-put (plist-put org-format-latex-options 
                             :scale 0.5)
                   :background "Transparent")))

(use-package! org-download
  :after org
  :bind (:map org-mode-map
         ("C-c i v" . org-download-clipboard))
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./images"
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-annotate-function (lambda (_link) "")
        org-download-screenshot-method "grimblast save area %s"))
(defun org-roam-capture-here ()
  "new org roam node in pwd"
  (interactive)
  (let ((org-roam-directory default-directory))
    (org-roam-capture)))

(map! :leader
      :desc "org-roam capture here"
      "n h" #'org-roam-capture-here)

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

(map! :leader
      :prefix "w"
      "h" #'evil-window-split
      "v" #'evil-window-vsplit)

(electric-pair-mode t)
