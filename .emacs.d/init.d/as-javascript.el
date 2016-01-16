;; FIXME: this breaks forward-word?!  FFS.
;; https://gist.github.com/aspiers/775ce717bd06d43d7adb

;; Steve Yegge to the rescue
;; (req-package js2-mode
;;   :commands nil
;;   :mode ("\\.js\\(\.erb\\)?$" . js2-mode))

(req-package coffee-mode)

(provide 'as-javascript)
