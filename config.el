;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Bach W")

(setq doom-theme 'doom-flatwhite)

(when (doom-font-exists-p "BlexMono Nerd Font Mono")
  (setq doom-font (font-spec :family "BlexMono Nerd Font Mono" :size 14)))
(when (doom-font-exists-p "IBM Plex Sans")
  (setq doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 14)))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-functions :append (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Best Editor in History!")))
;; Replacing logo with image
(setq fancy-splash-image (concat doom-private-dir "/images/emacs.svg"))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(setq-default org-download-image-dir "~/org/assets/images")

(defun my/org-roam-load-templates ()
  (add-to-list 'org-roam-capture-templates
               '("e" "encrypted" plain "%?"
                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                                    "#+title: ${title}\n")
                 :unnarrowed t)))

(defun my/org-roam-create-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter (my/org-roam-create-filter-by-tag tag-name)
                      (org-roam-node-list))))

(defun my/org-agenda-refresh-list ()
  "function that reloads the notes registered to agenda. Looks for tracktodo"
  (interactive)
  (setq org-agenda-files (append
                          (my/org-roam-list-notes-by-tag "tracktodo")
                          '("~/org/todo.org"
                            "~/org/schedule.org"
                            "~/org/agenda/events.org"
                            "~/org/agenda/birthdays.org"
                            "~/org/agenda/reading-list.org"))))

(defun my/org-roam-copy-todo-to-today ()
  "function that registers the hook to move done task to today"
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(after! org-roam
  (my/org-agenda-refresh-list)
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))
  (setq org-startup-folded 'content
        org-log-into-drawer nil)
  (my/org-roam-load-templates))

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)

(setq org-pomodoro-length 50)
(setq org-pomodoro-short-break-length 10)

(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets t
        +corfu-want-tab-prefer-navigating-snippets t
        +corfu-want-tab-prefer-navigating-org-tables t))
