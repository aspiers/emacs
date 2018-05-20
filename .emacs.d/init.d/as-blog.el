(add-to-list 'auto-mode-alist
             '("blog\\.adamspiers\\.org\\..*\\.txt\\'" . web-mode))

(req-package org2blog
  :require org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (and (buffer-file-name)
                   (string-match "\\.o2b$" (buffer-file-name))
                   (org2blog/wp-mode)))))

(bind-key "C-c w l"  'org2blog/wp-login)
(bind-key "C-c w n"  'org2blog/wp-new-entry)

(provide 'as-blog)
