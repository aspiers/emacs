(autoload 'as-duplicate-line      "as-editing" "Duplicate the current line" t)
(autoload 'as-join-line-with-next "as-editing" "Join line with next"        t)
(autoload 'as-copy-previous-line-suffix
                                  "as-editing" "Copy previous line suffix"  t)
(autoload 'as-align-to-previous-line
                                  "as-editing" "Align to previous line"     t)
;; as-transpose-lines autoloaded elsewhere
;; mark-list not bound
(autoload 'vim-yy                 "as-editing" "Simulate vim's yy command"  t)
;; bn-end-of-line-but-one autoloaded elsewhere

(autoload 'bn-make-region-into-secondary
                                  "as-editing" "Secondary region handling"  t)
(autoload 'bn-exchange-region-and-secondary
                                  "as-editing" "Secondary region handling"  t)
(autoload 'bn-keyboard-quit
                                  "as-editing" "Secondary region handling"  t)

(provide 'as-editing-auto)
