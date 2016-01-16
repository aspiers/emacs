(defun as-check-feature-loaded (feature)
  "Check whether FEATURE is loaded: if so, returns it, otherwise
issues a warning and returns nil."
  (if (featurep feature)
      feature
    (message "Warning: failed to load %s" feature)
    nil))

(provide 'as-require)
