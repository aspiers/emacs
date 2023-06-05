(defun org-olp-is-sibling (a b)
  "Return t if olp lists `a' and `b` are siblings."
  (and (equal (length a) (length b))
       (car a)
       (if (equal (length a) 1)
           (not (equal a b))
         (and (equal (car a) (car b)) (org-olp-is-sibling (cdr a) (cdr b))))))

(defvar org-refile-in-sibling-origin nil
  "Temporary variable containing an olp list pointing to the
subtree containing the point when `org-refile-in-sibling' was
invoked.")

(defvar org-refile-in-subtree-origin nil
  "Temporary variable containing an olp list pointing to the
subtree containing the point when `org-refile-in-subtree' was
invoked.")

(defun as/org-refile-verify-sibling ()
  "Return t for headings in the subtree defined by the outline path in
`org-refile-in-subtree-origin'."
  (let ((candidate (org-get-outline-path t)))
    (org-olp-is-sibling candidate org-refile-in-sibling-origin)))

(defun as/org-refile-verify-subtree ()
  "Return t for headings in the subtree defined by the outline path in
`org-refile-in-subtree-origin'."
  (let ((candidate (org-get-outline-path t)))
    (and (list-has-prefix (butlast candidate)
                          (butlast org-refile-in-subtree-origin))
         (not (equal candidate org-refile-in-subtree-origin)))))

(defun org-refile-in-sibling (&optional arg default-buffer rfloc msg)
  "Invoke org-refile while constraining candidate targets to
siblings of the heading currently at point."
  (interactive "P")
  (let ((org-refile-in-sibling-origin (org-get-outline-path t))
        (org-refile-target-verify-function 'as/org-refile-verify-sibling)
        (org-refile-cache nil))
    (org-refile arg default-buffer rfloc msg)))

(defun org-refile-in-subtree (&optional arg default-buffer rfloc msg)
  "Invoke org-refile while constraining candidate targets to headings
within the subtree containing the heading currently at point."
  (interactive "P")
  (let ((org-refile-in-subtree-origin (org-get-outline-path t))
        (org-refile-target-verify-function 'as/org-refile-verify-subtree)
        (org-refile-cache nil))
    (org-refile arg default-buffer rfloc msg)))

(provide 'org-refile-narrowed)
