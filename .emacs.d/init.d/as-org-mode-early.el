;; Someorg-mode initialisation needs to be loaded before el-get
;; packages which depend on it, otherwise the version of org-mode
;; distributed with emacs will automatically get pulled in when el-get
;; packages which depend on org-mode are initialized, and that would
;; result in the default values for `org-disputed-keys' taking effect
;; and sticking as long as emacs stays running.

(custom-set-variables
 '(org-disputed-keys
   (quote
    (([(control shift right)] . [(control shift n)])
     ([(control shift left)]  . [(control shift p)])
     ([(control ?,)]          . [(control ?')])
     ([(control tab)]         . [(control meta tab)]))))
 '(org-replace-disputed-keys t))

(provide 'as-org-mode-early)
