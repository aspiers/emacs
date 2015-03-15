(require 'as-org-mode)

(defun org-show-effort ()
  "Shows the effort of the entry at the current point."
  (interactive)
  (let ((effort (org-entry-get (point) org-effort-property)))
    (message (if effort (format "Effort is %s" effort)
               "No effort defined"))))

(add-hook
 'org-mode-hook
 (lambda ()
   ;; Zero effort is last (10th) element of global Effort_ALL property
   ;; so that we get zero effort when pressing '0' in the Effort column
   ;; in Column view, since this invokes `org-set-effort' with arg 0,
   ;; which stands for the 10th allowed value.
   (let ((effort-values
          (car
           (read-from-string
            (concat "("
                    (cdr (assoc "Effort_ALL" org-global-properties))
                    ")")))))
     (dotimes (effort-index 10)
       (let* ((effort (nth effort-index effort-values))
              (key-suffix (number-to-string
                           (if (= effort-index 9) 0 (1+ effort-index))))
              (fn-name (concat "org-set-effort-"
                               (number-to-string effort-index)))
              (fn (intern fn-name)))
         ;; (message "Binding M-o %s to %s which sets effort to %s"
         ;;          key-suffix fn-name effort)
         (fset fn `(lambda ()
                     ,(format "Sets effort to %s." effort)
                     (interactive)
                     (org-set-effort ,(1+ effort-index))))
         (local-set-key (concat "\eo" key-suffix) fn)
         (local-set-key "\eo\eo" 'org-show-effort))))))

(defun org-unset-effort ()
  "Unsets the Effort property for the current headline."
  (interactive)
  (org-delete-property org-effort-property))
(bind-key "M-o SPC" 'org-unset-effort)

(provide 'as-org-effort)
