(defvar as-el-get-emacswiki-packages
  '(
    auto-recomp
    buff-menu+
    ediff-trees
    faith
    rcov-overlay
    versions
    ))

(defun as-el-get-missing-emacswiki-recipes ()
  "Return a list of emacswiki recipes which are required but
  missing from el-get"
  (let ((missing-recipes
         (remove-if (lambda (pkg)
                      (el-get-recipe-filename pkg))
                    as-el-get-emacswiki-packages)))
    (when missing-recipes
      (message "emacswiki recipes missing: %s" missing-recipes))
    missing-recipes))

(if (and (as-el-get-owner-p)
         (or ((not (file-exists-p
                    (concat el-get-el-get-dir "/recipes/emacswiki")))
              (as-el-get-missing-emacswiki-recipes))))
    (with-demoted-errors
      (el-get-emacswiki-build-local-recipes)))

(provide 'as-el-get-emacswiki)
