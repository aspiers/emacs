;; http://orgmode.org/worg/org-gtd-etc.php

(require 'as-org-mode)

;; FIXME: something changed here, but I use Pomodroido now anyway.
;;(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :
(add-hook 'org-clock-in-hook '(lambda () 
      (if (not org-timer-current-timer) 
      (org-timer-set-timer '(16)))))

(provide 'as-pomodoro)
