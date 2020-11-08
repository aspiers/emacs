(require 'as-org-mode)

(with-packages org
  :config

  (defun as-org-convert-buffer-sub-to-effort ()
    "Convert all 'sub*' tags within a buffer into 'Effort' properties."
    (interactive)
    (org-map-entries 'as-org-convert-headline-sub-to-effort nil 'file))

  (defun as-org-convert-headline-sub-to-effort ()
    "Convert a headline with a 'sub*' tag into an 'Effort' property."
    (interactive)
    (unless (org-on-heading-p)
      (error "Not on heading"))
    (let ((origtags (org-get-tags)))
      (mapcar
       (lambda (tag)
         (when (equal (substring tag 0 3) "sub")
           (org-set-property "Effort"
                             (cdr (assoc (substring tag 3)
                                         '(("10"  . "0:10")
                                           ("30"  . "0:30")
                                           ("45"  . "0:45")
                                           ("60"  . "1:00")
                                           ("120" . "2:00")
                                           ("4"   . "4:00")
                                           ("day" . "8:00")))))
           (org-toggle-tag tag 'off)))
       origtags)))

  (defun org-show-effort ()
    "Shows the effort of the entry at the current point."
    (interactive)
    (let ((effort (org-entry-get (point) org-effort-property)))
      (message (if effort (format "Effort is %s" effort)
                 "No effort defined"))))

  (defun org-setup-effort-functions ()
    "Define a function with prefix `org-set-effort-' for each predefined
effort in the Effort_ALL global property, and bind convenient keys to it.

Zero effort is last (10th) element of global Effort_ALL
property so that we get zero effort when pressing '0' in the
Effort column in Column view, since this invokes `org-set-effort'
with arg 0, which stands for the 10th allowed value."
    (let ((effort-values
           (car
            (read-from-string
             (concat "("
                     (cdr (assoc "Effort_ALL" org-global-properties))
                     ")")))))
      (dotimes (effort-index 10)
        (let* ((effort-sym (nth effort-index effort-values))
               (key-suffix (number-to-string
                            (if (= effort-index 9) 0 (1+ effort-index))))
               (effort-string
                (if (eq effort-sym '0)
                    "0"
                  (let ((effort (symbol-name effort-sym)))
                    (cond ((string-prefix-p "0:" effort)
                           (concat (substring effort 2) "m"))
                          ((string-suffix-p ":00" effort)
                           (concat (string-remove-suffix ":00" effort) "h"))
                          (t (error "Couldn't parse effort %s" effort))))))
               (fn-name (concat "org-set-effort-" effort-string))
               (fn (intern fn-name)))
          (message "Binding M-o %s to %s which sets effort to %s"
                   key-suffix fn-name effort-string)
          (eval
           `(defun ,fn ()
                ,(format "Sets effort to %s." effort-string)
              (interactive)
              (org-set-effort nil ,effort-string)))
          ;; (declare-function fn
          ;;                   (or (symbol-file 'org-show-effort)
          ;;                       load-file-name))
          (define-key org-mode-map
            (concat "\M-o" key-suffix) fn)))))

  (org-setup-effort-functions)

  (defun org-unset-effort ()
    "Unsets the Effort property for the current headline."
    (interactive)
    (org-delete-property org-effort-property))

  :bind (("M-o SPC" . org-unset-effort)
         :map org-mode-map
         ("M-o o" . org-show-effort)))

(provide 'as-org-effort)
