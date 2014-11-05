(require 'netrc)

(setq blog (netrc-machine (netrc-parse "~/.netrc") "blog.adamspiers.org" t))
(setq org2blog/wp-blog-alist
      `(("my-blog"
         :url "http://blog.adamspiers.org/xmlrpc.php"
         :username ,(netrc-get blog "login")
         :password ,(netrc-get blog "password"))))

(require 'as-require)
(when (as-check-feature-loaded 'org2blog)
  (as-progress "org2blog loaded")
  (add-to-list 'auto-mode-alist '("\\.o2b$" . org-mode)))

(provide 'as-org2blog)
