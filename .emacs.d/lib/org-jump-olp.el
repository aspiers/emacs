;;; TODO: upstream this

;;;###autoload
(defun org-jump-olp (file olp &optional context)
  "Jump to an outline path in an org file"
  (find-file file)
  (let ((m (org-find-olp (cons file olp))))
    (set-buffer (marker-buffer m))
    (goto-char m)
    (set-marker m nil))
  (org-show-context (or context 'tree))
  (org-show-children))

(provide 'org-jump-olp)
