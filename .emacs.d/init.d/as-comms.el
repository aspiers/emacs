;; Interaction / communication with other people

;;{{{ mutt

(autoload 'mutt-mode "mutt" "Mode for editing mutt files")
(add-to-list 'auto-mode-alist '("/mutt-\\|itsalltext.*mail\\.google" . mail-mode))
(eval-when-compile (require 'sendmail))
(add-hook
 'mail-mode-hook
 (lambda ()
   (turn-on-auto-fill)
   (as-setup-mode-for-discussion)
   (as-set-local-server-edit-keys)))

;;}}}
;;{{{ crm114-mode

(autoload 'crm114-mode "crm114-mode" "crm114-mode" t)
(add-to-list 'auto-mode-alist '("\\.crm$" . crm114-mode))

;;}}}
;;{{{ w3m-mode

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

;;}}}
;;{{{ wordpress

(add-to-list 'auto-mode-alist
             '("blog\\.adamspiers\\.org\\..*\\.txt\\'" . web-mode))

;;}}}
;;{{{ org2blog

(add-hook 'org-mode-hook
          (lambda ()
            (and (buffer-file-name)
                 (string-match "\\.o2b$" (buffer-file-name))
                 (org2blog/wp-mode))))
(global-set-key "\C-cwl"  'org2blog/wp-login)
(global-set-key "\C-cwn"  'org2blog/wp-new-entry)

;;}}}
;;{{{ ispell

(global-set-key [(control $)]             'ispell-complete-word)
(global-set-key [(control meta $)]        'ispell-buffer)

;;}}}
