;;; planner-xtla.el --- Xtla integration for the Emacs Planner

;; Copyright (C) 2005 Stefan Reichör

;; Author: Stefan Reichör <stefan@xsteve.at>
;; Keywords: planner, xtla

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ + Commentary:

;; This file allows you to refer to your tla changesets easily from within
;; a planner page.
;;
;; Example:
;; [[xtla://miles@gnu.org--gnu-2005/emacs--cvs-trunk--0--patch-19][patch-19]]
;;   can be browsed easily via xtla

;;; Code:

(require 'planner)
(require 'xtla)

(defgroup planner-xtla nil
  "Planner options for the xtla integration."
  :prefix "planner-xtla-"
  :group 'planner)

(defcustom planner-xtla-log-edit-include-files-flag
  t
  "Non-nil means include a list of committed files in the note."
  :type 'boolean
  :group 'planner-xtla)

(defcustom planner-xtla-log-edit-notice-commit-function nil
  "Function that should return non-nil if this commit should be noted.
The function will be run in the log buffer."
  :type '(choice
          (const :tag "Always note commits" t)
          function)
  :group 'planner-xtla)

(defcustom planner-xtla-url-transform-alist nil
  "List of associations between a branch name with a base
url. For example : '(\"hodique@lifl.fr--2005\" .
\"http://www.lifl.fr/~hodique/archives/2005\"). This overrides the
url given by `tla--archive-tree'. Useful when using a mirror."
  :type '(repeat (cons string string))
  :group 'planner-xtla)

;;;###autoload
(defun planner-annotation-from-xtla ()
  "If called from a xtla buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (cond ((eq major-mode 'tla-revision-list-mode)
         (planner-make-link (concat "xtla://"
                                    (cadr (tla--get-revision-info-at-point)))
                            (cadr (tla--get-revision-info-at-point))))))

;;;###autoload
(defun planner-xtla-browse-url (url)
  "If this is a xtla url, handle it."
  (when (string-match "^xtla:/?/?\\(.+\\)" url)
    (tla-get-changeset (match-string 1 url) t)
    t))

;;;###autoload
(defun planner-xtla-log-edit-add-note ()
  "Provide `planner-log-edit'-like functionality for xtla.
This function is automatically called by `tla-commit-hook'.
See also `planner-xtla-log-edit-notice-commit-function'."
  (interactive)
  (when (if (functionp planner-xtla-log-edit-notice-commit-function)
            (funcall planner-xtla-log-edit-notice-commit-function)
          planner-xtla-log-edit-notice-commit-function)
    (let ((arch-revision)
          (planner-xtla-link)
          (committed-files))
      ;; assume we are in the *tla-buffer* after the commit
      (goto-char (point-min))
      (re-search-forward "^\\* committed ")
      (setq arch-revision (buffer-substring-no-properties
                           (point) (planner-line-end-position)))
      (setq committed-files (buffer-substring-no-properties
                             (point-min) (planner-line-beginning-position)))
      (setq planner-xtla-link (planner-make-link
                               (concat "xtla://" arch-revision) arch-revision))
      (save-window-excursion
        (with-planner
          (planner-create-note nil)
          (insert "Commit")
          (insert (concat " " planner-xtla-link))
          (newline)
          (when planner-xtla-log-edit-include-files-flag
            (insert "Files:\n")
            (insert committed-files)
            (newline))
          (insert (replace-regexp-in-string "^\\*" " *" tla-last-commit-message)))))))

(defun planner-xtla-url-transform (target &rest ignored)
  "Transforms a xtla link into a http link to a public
location. The association is first searched in
`planner-xtla-url-transform-alist', and then in
`tla--archive-tree'"
  (tla--archive-tree-build-archives t)
  (let ((reg (concat "^xtla://\\(\\(?:"
                     (mapconcat 'car tla--archive-tree "\\|")
                     "\\)/\\(?:.*\\)\\)$")))
    (save-match-data
      (if (string-match reg target)
          (let* ((elts (tla--name-split (match-string 1 target)))
                 (subst (or (cdr (assoc (car elts) planner-xtla-url-transform-alist))
                            (car (cadr (assoc (car elts) tla--archive-tree))))))
            (when (nth 1 elts)
              (setq subst (concat subst "/" (nth 1 elts)))
              (when (nth 2 elts)
                (setq subst (concat subst "/" (nth 1 elts) "--" (nth 2 elts)))
                (when (nth 3 elts)
                  (setq subst (concat subst "/" (nth 1 elts) "--" (nth 2 elts) "--" (nth 3 elts)))
                  (when (nth 4 elts)
                    (setq subst (concat subst "/" (nth 4 elts)))))))
            subst)
        target))))

(add-hook 'tla-commit-done-hook 'planner-xtla-log-edit-add-note)

(planner-add-protocol "xtla" 'planner-xtla-browse-url 'planner-xtla-url-transform)
(add-hook 'planner-annotation-functions 'planner-annotation-from-xtla)
(custom-add-option 'planner-annotation-functions 'planner-annotation-from-xtla)

(provide 'planner-xtla)

;;; planner-xtla.el ends here
