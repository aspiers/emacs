;; http://orgmode.org/worg/org-gtd-etc.php

(require 'as-org-mode)

;; FIXME: something changed here, but I use Pomodroido now anyway.
;;(setq org-timer-default-timer 25)

;; Modify the org-clock-in so that a timer is started with the default
;; value except if a timer is already started :

(defun as-start-countdown-on-clock-in ()
  "Start a countdown timer unless there's already a timer active."
    (unless (or org-timer-start-time org-timer-countdown-timer)
      (org-timer-set-timer)))

(add-hook 'org-clock-in-hook 'as-start-countdown-on-clock-in)

(provide 'as-pomodoro)
