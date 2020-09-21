;; _T_oggles and settings (C-c t)

(defun as-toggle-indent-tabs-mode ()
  "Toggles the value of indent-tabs-mode in the current buffer"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode set to %s" indent-tabs-mode))

(defun as-set-tab-width (width)
  "Sets the tab-width variable to the given argument."
  (interactive "NNew hard tab width: ")
  (setq tab-width width))

(use-package hydra
  :config
  (defun format3 (var) (format "%3s" var))
  (defhydra hydra-toggle (:color blue)
    "
  _a_ abbrev-mode:       %s(format3 abbrev-mode) ^^^^^^^^^^^^^^     _d_ debug-on-error:    %s(format3 debug-on-error)
  _f_ auto-fill-mode:    %3s(if auto-fill-function \"yes\") ^^^     _S_ truncate-lines:    %s(format3 truncate-lines)
  _o_ overwrite-mode:    %3s(if overwrite-mode \"yes\") ^^^^^^^     _w_ tab-width:         %s(format3 tab-width) ^^^^^^^^^^^^^^^^
_SPC_ whitespace-mode:   %s(format3 whitespace-mode) ^^^^^^^^^^   _TAB_ indent-tabs-mode:  %s(format3 indent-tabs-mode)
"
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("o" overwrite-mode nil)
    ("s" toggle-truncate-lines nil)
    ("S" toggle-truncate-lines nil)
    ("w" as-set-tab-width nil)
    ("TAB" as-toggle-indent-tabs-mode nil)
    ("SPC" whitespace-mode nil)
    ("q" nil "quit"))

  :bind ("C-c t" . hydra-toggle/body))

(provide 'as-toggles)
