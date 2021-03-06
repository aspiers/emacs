;; Pull in autoloads
(require 'w3m-load "w3m-load" t)
(autoload 'w3m-find-file "w3m" nil t)

(eval-when-compile (require 'dired))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "\C-xm" 'dired-w3m-find-file)))

(autoload 'w3m-find-file "w3m")
(autoload 'dired-get-filename "dired")
(defun dired-w3m-find-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (y-or-n-p (format "Open 'w3m' %s " (file-name-nondirectory file)))
        (w3m-find-file file))))

(defun mhj-w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-" nil) ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))

(use-package web-mode
  :mode "\\.\\(phtml\\|tpl\\.php\\|jsp\\|as[cp]x\\|erb\\|mustache\\|djhtml\\|html?\\)\\'"
  :config
  (define-key web-mode-map (kbd "C-;") nil)
  ;; fci-mode *still* breaks web-mode :-(
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/46
  ;; (add-hook 'after-change-major-mode-hook
  ;;           (lambda () (if (string= major-mode "web-mode")
  ;;                          (turn-off-fci-mode))))
  )

(use-package gist)
(use-package haml-mode
  :mode "\\.haml\\'")
(use-package sass-mode
  :mode "\\.scss\\'"
  :hook (sass-mode . flycheck-mode))
(use-package flymake-sass)
(use-package flymake-css)

(provide 'as-web)
