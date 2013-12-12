;; http://hbin.me/blog/2012/08/26/clean-the-emacs-mode-line/

(defun hbin-remove-mm-lighter (mm)
  "Remove minor lighter from the mode line."
  (let ((lighter (assq mm minor-mode-alist)))
    (when lighter
      (setcar (cdr lighter) nil))))

(hbin-remove-mm-lighter 'global-whitespace-mode)
(hbin-remove-mm-lighter 'whitespace-mode)
(hbin-remove-mm-lighter 'guide-key-mode)
(hbin-remove-mm-lighter 'smartparens-mode)
