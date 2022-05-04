(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; elpy has serious performance issues
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))

;; Allow modes such as jdl-mode to be given Python alignment rules
(defcustom align-python-modes '(python-mode)
  "A list of modes where Python syntax is to be seen."
  :type '(repeat symbol)
  :group 'align)

(defvar align-rules-list)
(defun as-tweak-align-rules ()
  "Replace occurrences of `python-mode' in `align-rules-list'
with references to `align-python-modes'.

FIXME: needs to tweak align-*-modes too."
  (dolist (rule align-rules-list)
    (let* ((title (car rule))
           (pairs (cdr rule))
           (modespair (assq 'modes pairs))
           (modes (cdr modespair))
           (search 'python-mode)
           (replacement 'align-python-modes)
           (result nil
                   ;(format "no change for %s" title)
                   ))
      ;(if (eq title 'basic-comma-delimiter) (edebug))
      (if modes
          (cond
           ;; modes could have form: align-foo-modes
           ((equal modes `(quote (,search)))
            (setcdr modespair replacement)
            (setq result (format "single change for %s" title)))

           ;; FIXME: handle modes like
           ;;   (append align-perl-modes '(python-mode))
           ;; as seen in basic-comma-delimiter

           ;; handle modes with form: '(mode1 mode2 ...)
           ;; i.e. (quote (mode1 mode2 ...))
           ((and (listp modes)
                 (eq 'quote (car modes))
                 (listp (cadr modes)))
            (let ((to-replace (memq search (cadr modes))))
              (when to-replace
                ;; remove this element from the list
                (setcar to-replace (cadr to-replace))
                (setcdr to-replace (cddr to-replace))
                (setcdr modespair `(append ,replacement ,modes))
                (setq result (format "list change for %s" title)))))))
       ;(message result)
       )))

(eval-after-load "align"
  '(progn
     (as-tweak-align-rules)
     (add-to-list 'align-rules-list
                  '(python-dictionary-braces
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\S-")
                    (group    . (2))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))
     (add-to-list 'align-rules-list
                  '(python-dictionary-pairs
                    (regexp   . "^\\(.*?{\\)?\\(\\s-*\\)\\([^:]+\\)\\(\\s-*\\):\\(\\s-*\\)")
                    (group    . (4 5))
                    (modes    . align-python-modes)
                    (tab-stop . nil)))))

;; Note: lsp-python is obsolete and broken
(use-package lsp-pyright)
;; (use-package lsp-pyre)
;; (use-package lsp-python-ms)

(provide 'as-python)
