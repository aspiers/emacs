(req-package web-mode
  :mode "\\.\\(phtml\\|tpl\\.php\\|jsp\\|as[cp]x\\|erb\\|mustache\\|djhtml\\|html?\\)\\'"
  :config
  (define-key web-mode-map (kbd "C-;") nil))

(req-package gist)
(req-package haml-mode)
(req-package sass-mode
  :mode "\\.scss\\'")

(provide 'as-web)
