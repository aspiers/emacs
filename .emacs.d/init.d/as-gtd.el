;; Things to help me with Getting Things Done.

(defvar as-personal-todo "~/org/TODO.org")

(with-packages org
  :init
  ;;(require 'org-agenda)

  :bind (:map org-agenda-mode-map
         ("i" . 'org-agenda-clock-in)
         ("o" . 'org-agenda-clock-out))

  :config
  (require 'as-org-agenda)
  (require 'as-org-jump)
  (require 'as-org-stats)
  (require 'org-meeting-actions))

(use-package org-pomodoro
  :after org
  :bind (:map org-mode-map
         ("C-c C-x C-S-i" . 'org-pomodoro)
         :map org-agenda-mode-map
         ("I" . 'org-pomodoro)))

;; (use-package beeminder)

(provide 'as-gtd)
