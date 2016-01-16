(load "subr")
(req-package versions
  :if (not (fboundp 'version<=)))

(provide 'as-versions)
