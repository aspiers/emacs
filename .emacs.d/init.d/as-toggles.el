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
  (defhydra hydra-toggle (:color pink)
    "
  _a_ abbrev-mode:       %`abbrev-mode
  _d_ debug-on-error:    %`debug-on-error
  _f_ auto-fill-mode:    %`auto-fill-function
  _S_ truncate-lines:    %`truncate-lines
  _w_ tab-width:         %`tab-width
_TAB_ indent-tabs-mode:  %`indent-tabs-mode
_SPC_ whitespace-mode:   %`whitespace-mode
"
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("S" toggle-truncate-lines nil)
    ("w" as-set-tab-width nil)
    ("TAB" as-toggle-indent-tabs-mode nil)
    ("SPC" whitespace-mode nil)
    ("q" nil "quit"))

  :bind ("C-c t" . hydra-toggle/body))

(provide 'as-toggles)
