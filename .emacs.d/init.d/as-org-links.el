(require 'as-org-mode)

;; See also as-mairix.el

(use-package org
  :bind (("C-c L" . org-store-link))

  :config
  (defun org-unlinkify ()
    "Replace an org-link with the path, or description."
    (interactive)
    (let ((eop (org-element-context)))
      (when (eq (org-element-type eop) 'link)
        (save-excursion
          (let* ((start (org-element-property :begin eop))
                 (end (org-element-property :end eop))
                 (contents-begin (org-element-property :contents-begin eop))
                 (contents-end (org-element-property :contents-end eop))
                 (path (org-element-property :path eop))
                 (desc (and contents-begin
                            contents-end
                            (buffer-substring contents-begin contents-end))))
            (setf (buffer-substring start end) (or desc path)))))))

  (defalias 'org-delinkify 'org-unlinkify)

  (defun org-refile-and-link ()
    "Refile heading, adding a link to the new location.
Prefix arguments are interpreted by `org-refile'."
    (interactive)
    (when (member current-prefix-arg '(3 (4) (16)))
      (user-error "Linking is incompatible with that prefix argument"))
    (let ((heading  (org-get-heading t t))
          (orig-file (buffer-file-name)))
      (call-interactively #'org-refile)
      (let* ((refile-file
              (bookmark-get-filename
               (assoc (plist-get org-bookmark-names-plist :last-refile)
                      bookmark-alist)))
             (same-file (string= orig-file refile-file))
             (link (if same-file
                       (concat "*" heading)
                     (concat refile-file "::*" heading)))
             (desc heading))
        (open-line 1)
        (insert (org-make-link-string link desc))))))

(provide 'as-org-links)
