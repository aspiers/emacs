(use-package org2blog
  :after (org)
  :mode ("\\.o2b\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (and (buffer-file-name)
                   (string-match "\\.o2b$" (buffer-file-name))
                   (org2blog/wp-mode))))
  :bind (("C-c w l" . org2blog/wp-login)
         ("C-c w n" . org2blog/wp-new-entry)))

(provide 'as-blog)
