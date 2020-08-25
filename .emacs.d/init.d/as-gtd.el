;; Things to help me with Getting Things Done.

(require 'as-mairix)

(defvar as-personal-todo "~/org/TODO.org")

(use-package org
  :after as-org-mode

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

;; (use-package beeminder)

(provide 'as-gtd)
