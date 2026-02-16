;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; theme
(setq doom-theme 'miasma)

(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 16))
(setq display-line-numbers-type 'relative)

(setq org-directory "~/org")

;; org-roam capture templates
(setq org-roam-capture-templates
      '(("p" "Project" plain
         "* Elevator Pitch\n%?\n\n* Type\nSoftware / App / SaaS / Idea\n\n* Status\nActive / Paused / Completed / Idea\n\n* Tech Stack\n\n* Timesheet\n| Date | Hours | Notes |\n|------|-------|-------|\n\n* Next Steps\n\n* Notes\n"
         :if-new (file+head "projects/${title}.org"
                            "#+title: ${title}\n#+filetags: :project:\n")
         :immediate-finish t :unnarrowed t)
        ("c" "Class" plain
         "* Overview\n\n\
- Instructor :: \n\
- Semester :: \n\
- Schedule :: \n\
- Location :: \n\
\n\
* Syllabus\n\n\
\n\
* Important Dates\n\n\
\n\
* Assignments\n\n\
\n\
* Exams\n\n\
\n\
* Resources\n\n\
\n\
* Lecture Index\n"
         :if-new (file+head "classes/%<%Y>/${slug}.org"
                            "#+title: ${title}\n#+filetags: :class:\n")
         :immediate-finish t :unnarrowed t)
        ("l" "Lecture" plain
         "* %<%Y-%m-%d> — ${title}\n\n\
** Key Topics\n\
- \n\n\
** Notes\n\n\n\
** Questions\n\
- \n\n\
** Action Items\n\
- [ ] \n"
         :if-new (file+head "lectures/%<%Y-%m-%d>-${slug}.org"
                            "#+title: ${title}\n#+filetags: :lecture:\n")
         :immediate-finish t :unnarrowed t)
        ("o" "Concept" plain
         "* Summary\n%?\n\n* Key Points\n- \n\n* Related\n"
         :if-new (file+head "concepts/${slug}.org"
                            "#+title: ${title}\n#+filetags: :concept:\n")
         :immediate-finish t :unnarrowed t)
        ("r" "Reference" plain "%?"
         :if-new (file+head "reference/${title}.org"
                            "#+title: ${title}\n")
         :immediate-finish t :unnarrowed t)))

(defun my/open-daily-note ()
  "Open today's daily journal, creating it from template if new."
  (interactive)
  (let* ((dir (expand-file-name "journal/" org-directory))
         (file (expand-file-name (format-time-string "%Y-%m-%d.org") dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (find-file file)
    (when (= (buffer-size) 0)
      (insert (format-time-string
               "#+title: %Y-%m-%d\n#+filetags: :daily:\n\n* Timesheet\n| Task | Hours | Notes |\n|------|-------|-------|\n|      |       |       |\n\n* Notes\n")))))

;; org-roam
(use-package! org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture))
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

(after! org
  ;; agenda — targeted directories only
  (setq org-agenda-files
        (append (list (expand-file-name "habits.org" org-directory)
                      (expand-file-name "journal/" org-directory)
                      (expand-file-name "roam/projects/" org-directory))
                (directory-files-recursively
                 (expand-file-name "roam/classes/" org-directory)
                 "\\.org$")))

  ;; org-habit
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 60)

  ;; inline images
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook #'org-display-inline-images)

  ;; latex preview
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent")))

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "./.images"
        org-download-heading-lvl nil
        org-download-timestamp "_%Y%m%d_%H%M%S"
        org-download-link-format "[[file:.images/%s]]\n"
        org-download-annotate-function (lambda (_link) "")
        org-download-screenshot-method (if (eq system-type 'darwin)
                                           "screencapture -i %s"
                                         "grimblast save area %s"))
  (defun my/org-download-clipboard (&optional basename)
    "Paste clipboard image without creating a properties drawer."
    (interactive)
    (cl-letf (((symbol-function 'org-id-get-create) #'ignore))
      (org-download-clipboard basename)))
  (map! :map org-mode-map "C-c i v" #'my/org-download-clipboard))
(defun org-roam-capture-here ()
  "new org roam node in pwd"
  (interactive)
  (let ((org-roam-directory default-directory))
    (org-roam-capture)))

(map! :leader
      :desc "org-roam capture here"
      "n h" #'org-roam-capture-here)

(map! :leader
      :desc "Open daily note"
      "n d" #'my/open-daily-note)
(map! "C-c n d" #'my/open-daily-note)

(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

(after! org
  (map! :map org-mode-map
        :n "C-j" #'evil-window-down))

(map! :leader
      :prefix "w"
      "h" #'evil-window-split
      "v" #'evil-window-vsplit)

;; gptel — LLM integration
(load! "secrets.el" doom-user-dir t)

(use-package! gptel
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key my/anthropic-api-key)

  (setq gptel-model 'private-gpt
        gptel-backend (gptel-make-openai "PrivateGPT"
                        :stream nil
                        :protocol "http"
                        :host "localhost:8001"
                        :models '(private-gpt)
                        :key ""
                        :request-params '(:use_context t))
        gptel-default-mode 'org-mode)

  ;; default system prompt for all requests
  (setq gptel--system-message
        "You are a concise teaching assistant. Explain concepts clearly with examples when helpful. Keep responses brief and focused. Use plain text, not markdown."))

(defun my/ai-ask-docs (question)
  "Ask PrivateGPT a question about ingested documents."
  (interactive "sAsk your docs: ")
  (gptel-request question
   :callback
   (lambda (response _info)
     (when response
       (with-current-buffer (get-buffer-create "*PrivateGPT*")
         (goto-char (point-max))
         (insert "\n** Q: " question "\n" response "\n")
         (org-mode)
         (pop-to-buffer (current-buffer)))))))

(defun my/ai-explain-region (start end)
  "Send selected region to LLM for explanation, insert response in a collapsible drawer."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (insert-point (save-excursion (goto-char end) (end-of-line) (point))))
    (gptel-request
     (format "Explain the following concisely:\n\n%s" text)
     :callback
     (lambda (response _info)
       (when response
         (save-excursion
           (goto-char insert-point)
           (insert "\n:AI_EXPLAIN:\n" response "\n:END:\n")))))))

(defun my/ai-ask (start end)
  "Ask AI a question about the selected region. Response goes in a collapsible drawer."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (prompt (read-string "Ask AI: "))
         (insert-point (save-excursion (goto-char end) (end-of-line) (point))))
    (gptel-request
     (format "Given the following code/text:\n\n%s\n\nUser question: %s\n\nIMPORTANT: Provide an explanation only. Do NOT rewrite or modify the code. Respond in plain text." text prompt)
     :callback
     (lambda (response _info)
       (when response
         (save-excursion
           (goto-char insert-point)
           (insert "\n:AI_EXPLAIN:\n" response "\n:END:\n")))))))

(map! :leader
      :prefix ("A" . "ai")
      :desc "Ask AI" "a" #'my/ai-ask
      :desc "Explain region" "e" #'my/ai-explain-region
      :desc "Open gptel chat" "c" #'gptel
      :desc "Ask docs (RAG)" "d" #'my/ai-ask-docs)
