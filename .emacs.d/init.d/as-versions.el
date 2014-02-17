(load "subr")
(use-package versions
  :if (not (fboundp 'version<=)))

(provide 'as-versions)
