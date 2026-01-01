;;; $DOOMDIR/roam-helper.el -*- lexical-binding: t; -*-


(defun my/org-roam-load-templates ()
  (add-to-list 'org-roam-capture-templates
               '("e" "encrypted" plain "%?"
                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                                    "#+title: ${title}\n")
                 :unnarrowed t)))

