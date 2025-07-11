;; Things to help me with Getting Things Done.
;; http://orgmode.org/worg/org-gtd-etc.php
;;
;; See also as-org-jump

(defvar as-personal-todo "~/org/TODO.org")

(with-packages org
  :init
  ;;(require 'org-agenda)

  :bind (:map org-agenda-mode-map
         ("i" . 'org-agenda-clock-in)
         ("o" . 'org-agenda-clock-out))

  :config
  (require 'as-org-agenda)
  (require 'as-org-stats)
  (require 'org-meeting-actions))

(use-package org-pomodoro
  :after (org)
  :bind (:map org-mode-map
              ("C-c C-x C-S-i" . 'org-pomodoro)
              :map org-agenda-mode-map
              ("P" . 'org-pomodoro))
  :custom
  ;; Test sounds with:
  ;;   (org-pomodoro-play-sound :start)
  ;;   (org-pomodoro-play-sound :pomodoro)  ;; pomodoro finished
  ;;   (org-pomodoro-play-sound :overtime)
  ;;   (org-pomodoro-play-sound :killed)
  ;;   (org-pomodoro-play-sound :short-break)
  ;;   (org-pomodoro-play-sound :long-break)
  ;;   (org-pomodoro-play-sound :tick)
  (org-pomodoro-audio-player "mplayer")
  (org-pomodoro-finished-sound-args "-volume 40")
  (org-pomodoro-overtime-sound "/usr/share/sounds/workrave/default/daily-limit.wav")
  (org-pomodoro-short-break-sound "/usr/share/sounds/workrave/default/micro-break-started.wav")
  (org-pomodoro-finished-sound "/usr/share/sounds/workrave/default/rest-break-started.wav")
  (org-pomodoro-long-break-sound "/usr/share/sounds/workrave/default/rest-break-started.wav")
  (org-pomodoro-killed-sound "/usr/share/sounds/workrave/default/break-ignored.wav"))

;; Unmaintained fork of org-habit; unfortunately breaks org-agenda:
;; https://github.com/myshevchuk/org-habit-plus/issues/4
;;
;; (use-package org-habit-plus
;;   :straight (:host github :repo "myshevchuk/org-habit-plus")
;;   :after (org))

;; Currently suffers from showstopper bugs:
;; https://github.com/Elilif/org-heatmap/issues/9
;; https://github.com/Elilif/org-heatmap/issues/10
;;
;; (use-package org-heatmap
;;   :straight (:host github :repo "Elilif/org-heatmap")
;;   :after (org)
;;   :config
;;   (org-heatmap-mode))

;; (use-package beeminder)

(provide 'as-gtd)
