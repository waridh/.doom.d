;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight)

(when (doom-font-exists-p "BlexMono Nerd Font Mono")
  (setq doom-font (font-spec :family "BlexMono Nerd Font Mono" :size 14)))
(when (doom-font-exists-p "IBM Plex Sans")
  (setq doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 14)))


;; Dashboard modification
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-functions :append (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Best Editor in History!")))
;; Replacing logo with image
(setq fancy-splash-image (concat doom-private-dir "/images/emacs.svg"))

;; Uncomment the following line to enable transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95)) (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq-default org-download-image-dir "~/org/assets/images")

;; Doing a longer pomodoro
(setq org-pomodoro-length 50)
(setq org-pomodoro-short-break-length 10)

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

(setq +latex-viewers '(pdf-tools))
(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! org-roam
  (my/org-agenda-refresh-list)
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))
  (setq org-startup-folded 'content
        org-log-into-drawer nil))
(after! helm
  (set-face-attribute 'helm-selection nil
                      :background "yellow"
                      :foreground "black"))
(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets t
        +corfu-want-tab-prefer-navigating-snippets t
        +corfu-want-tab-prefer-navigating-org-tables t))
