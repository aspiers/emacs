;;; planner-id.el --- planner.el extension for global task IDs
;;
;; Copyright (C) 2003 Sacha Chua <sacha@free.net.ph>
;; Version: 2005.08.20-17.59-stable
;; URL: http://sacha.free.net.ph/notebook/emacs/dev/planner/
;;
;;; Commentary:
;;
;; After loading planner.el, place planner-id.el in your load path
;; and add this to your .emacs
;;
;;   (require 'planner-id)
;;
;; This module modifies the behavior of planner.el, adding global task
;; IDs so that tasks can be edited and updated.
;;
;; To automatically update linked tasks whenever you save a planner
;; file, set planner-id-update-automatically to a non-nil value. This
;; does not update completed or cancelled tasks. See documentation for
;; planner-id-update-tasks-on-page to find out how to force updates.
;;
;; Planner IDs are of the form {{Identifier:Number}}
;;
;; Alternatives: If you don't mind using a function to change your
;; task descriptions, you may find M-x planner-edit-task-description
;; easier to use. Other changes (A/B/C, status) can be applied with
;; M-x planner-update-task after you edit the buffer.
;;
;; You can get planner.el from
;; http://sacha.free.net.ph/notebook/emacs/dev/planner/
;;
;; Contributors:
;; - Oliver Krause <oik@gmx.net>
;;   Main idea, testing


(require 'planner)

;;; Code:
(defgroup planner-id nil
  "Planner ID options."
  :prefix "planner-id-"
  :group 'planner)

(defcustom planner-id-add-task-id-flag t
  "Non-nil means add task IDs to newly-created tasks."
  :type 'boolean
  :group 'planner-id)

(defcustom planner-id-tracking-file "~/.planner-id"
  "File that stores an alist with the current planner ids."
  :type 'file
  :group 'planner-id)

(defcustom planner-id-update-automatically t
  "Non-nil means update linked files automatically when file is saved."
  :type 'boolean
  :group 'planner-id)

(defface planner-id-face
  '((((class color) (background light))
     (:foreground "lightgray"))
    (t (:foreground "darkgray")))
  "Face for planner ID links."
  :group 'planner-id)

(defvar planner-id-values nil
  "Alist with (key nextvalue) pairs.")

(defvar planner-id-regexp "{{\\([^:]+\\):\\([0-9]+\\)}}"
  "Regexp matching planner IDs.")

(defun planner-id-get-id-from-string (string)
  "Return the planner ID in STRING as (identifier number)."
  (when (string-match planner-id-regexp string)
    (cons (planner-match-string-no-properties 1 string)
          (planner-match-string-no-properties 2 string))))

(defun planner-id-get-current-id ()
  "Return the planner ID on the current line as (identifier number)."
  (planner-id-get-id-from-string
   (buffer-substring (planner-line-beginning-position)
                     (planner-line-end-position))))

(defun planner-id-format-as-string (id)
  "Return the planner ID as a string of the form {{identifier:number}}."
  (concat "{{" (car id) ":" (cdr id) "}}"))

;;;###autoload
(defun planner-id-find-task (task-info &optional point)
  "Find task described by TASK-INFO. If POINT is non-nil, start from there.
If task is found, move point to line beginning and return non-nil.
If task is not found, leave point at POINT or the start of the buffer
and return nil."
  (goto-char (or point (point-min)))
  (let ((task-id (planner-id-get-id-from-string
                  (planner-task-description task-info)))
        (found nil))
    (when (re-search-forward
           (concat planner-task-regexp ".*"
                   (regexp-quote
                    (if task-id
                        (planner-id-format-as-string task-id)
                      (planner-task-description task-info))))
           nil t)
      (goto-char (planner-line-beginning-position)))))

;;; Redeclaration
;;;###autoload
(defun planner-id-jump-to-linked-task (&optional info)
  "Display the linked task page.
If INFO is specified, follow that task instead."
  (interactive)
  (let* ((task-info (or info (planner-current-task-info)))
         (link (and task-info (planner-task-link task-info))))
    (when (planner-local-page-p link)
      (planner-find-file link)
      (widen)
      (planner-id-find-task task-info))))

(defun planner-id-save ()
  "Save `planner-id-values' in `planner-id-tracking-file'."
  (with-temp-file planner-id-tracking-file
    (print planner-id-values (current-buffer))))

(defun planner-id-make-global-id (identifier)
  "Return a globally unique ID as (IDENTIFIER number)."
  (planner-id-load)
  (let ((result
         (cons
          identifier
          (number-to-string
           (let ((elem (assoc identifier planner-id-values)))
             (if elem
                 (setcdr elem (1+ (cdr elem)))
               (add-to-list 'planner-id-values (cons identifier 0))
               0))))))
    (planner-id-save)
    result))

(defun planner-id-load ()
  "Read the data from `planner-id-tracking-file'."
  (setq planner-id-values nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally planner-id-tracking-file)
          (goto-char (point-min))
          (setq planner-id-values (read (current-buffer))))
      (error
       (message "Could not read planner-id-values from %s. Setting it to nil."
                planner-id-tracking-file)))))

;;;###autoload
(defun planner-id-add-task-id-maybe ()
  "Add task ID if `planner-id-add-task-id-flag' is non-nil."
  (when planner-id-add-task-id-flag
    (planner-id-add-task-id)))

(defun planner-id-add-task-id ()
  "Add a task ID for the current task if it does not have one yet.
Update the linked task page, if any."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let* ((task-info (planner-current-task-info)))
        (unless (or (not task-info) (planner-id-get-current-id))
          (planner-edit-task-description
           (concat (planner-task-description task-info) " "
                   (planner-id-format-as-string
                    (planner-id-make-global-id "Tasks")))))))))

(defun planner-id-update-tasks-on-page (&optional force)
  "Update all tasks on this page.
Completed or cancelled tasks are not updated. This can be added
to `write-file-functions' (CVS Emacs) or `write-file-hooks'.
If FORCE is non-nil, completed and cancelled tasks are also updated."
  (interactive (list current-prefix-arg))
  ;; Prevent planner-id updates from cascading
  (let ((planner-id-update-automatically nil))
    (with-planner-update-setup
     (goto-char (point-min))
     (while (re-search-forward
             (concat
              (if force
                  planner-task-regexp
                planner-live-task-regexp)
              ".*?{{Tasks:[0-9]+}}")
             nil t)
       (planner-update-task)
       ;; Force the next line to be considered even if
       ;; planner-multi-update-task kicked in.
       (forward-line 1))))
  nil)

(defun planner-id-remove-tasks-on-page ()
  "Remove the task IDs from all tasks on this page.
This function does _not_ update tasks on linked pages."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat planner-task-regexp
                    "\\(.*?\\)\\(\\s-+{{Tasks:[0-9]+}}\\)") nil t)
      (replace-match "" t t nil 1))))

(defun planner-id-add-task-id-to-all ()
  "Add a task ID for all the tasks on the page.
Update the linked page, if any."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward planner-task-regexp nil t)
      (planner-id-add-task-id))
    ;; Force refontification
    (font-lock-fontify-buffer)))

(defun planner-id-at-point (&optional pos)
  "Return non-nil if a URL or Wiki link name is at POS."
  (if (or (null pos)
          (and (char-after pos)
               (not (eq (char-syntax (char-after pos)) ? ))))
      (let ((case-fold-search nil)
            (here (or pos (point))))
        (save-excursion
          (goto-char here)
          (skip-chars-backward " \t\n")
          (or (looking-at "{{Tasks:[^}]+}}")
              (and (search-backward "{{" (planner-line-beginning-position) t)
                   (looking-at "{{Tasks:[^}]+}}"))
              (<= here (match-end 0)))))))

(eval-and-compile
  (require 'compile)
  (unless (boundp 'grep-command)
    ;; Emacs 21 CVS
    (require 'grep)))

(defun planner-id-search-id (id)
  "Search for all occurrences of ID."
  (interactive "MID: ")
  (grep (concat (or grep-command  "grep") " "
                (shell-quote-argument id) " "
                (shell-quote-argument
                 (expand-file-name (planner-directory))) "/*")))

(defun planner-id-follow-id-at-point ()
  "Display a list of all pages containing the ID at point."
  (interactive current-prefix-arg)
  (if (planner-id-at-point)
      (planner-id-search-id (match-string 0))
    (error "There is no valid link at point")))

;; Very ugly compatibility hack.
(defmacro planner-follow-event (event)
  (if (featurep 'xemacs)
      `(progn
         (set-buffer (window-buffer (event-window event)))
         (and (event-point event) (goto-char (event-point event))))
    `(progn
       (set-buffer (window-buffer (posn-window (event-start event))))
       (goto-char (posn-point (event-start event))))))

(defun planner-id-follow-id-at-mouse (event)
  "Display a list of all pages containing the ID at mouse.
EVENT is the mouse event."
  (interactive "eN")
  (save-excursion
    (planner-follow-event event))
  (when (planner-id-at-point)
    (planner-id-search-id (match-string 0))))

;; (defvar planner-id-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [return] 'planner-id-follow-id-at-point)
;;     (define-key map [(control ?m)] 'planner-id-follow-id-at-point)
;;     (define-key map [(shift return)] 'planner-id-follow-id-at-point)
;;     (if (featurep 'xemacs)
;;         (progn
;;           (define-key map [(button2)] 'planner-id-follow-id-at-mouse)
;;           (define-key map [(shift button2)] 'planner-id-follow-id-at-mouse))
;;       (define-key map [(mouse-2)] 'planner-id-follow-id-at-mouse)
;;       (define-key map [(shift mouse-2)] 'planner-id-follow-id-at-mouse))
;;     (unless (eq emacs-major-version 21)
;;       (set-keymap-parent map planner-mode-map))
;;     map)
;;   "Local keymap used by planner when on an ID.")

;;;###autoload
(defun planner-id-markup (beg end &optional verbose)
  "Highlight IDs as unobtrusive, clickable text from BEG to END.
VERBOSE is ignored."
  (goto-char beg)
  (while (re-search-forward "{{[^}]+}}" end t)
    (planner-highlight-region
     (match-beginning 0)
     (match-end 0)
     'planner-id 60
     (list
      'face 'planner-id-face
      'intangible nil
      ;;'keymap planner-id-keymap
      ))))

;;;###autoload
(defun planner-id-update-tasks-maybe ()
  "Update tasks depending on the value of `planner-id-update-automatically'."
  (when planner-id-update-automatically
    (planner-id-update-tasks-on-page)))

;;;###autoload
(defun planner-id-setup ()
  "Hook into `planner-mode'."
  (add-hook 'emacs-wiki-highlight-buffer-hook
            'planner-id-markup nil t)
  (add-hook
   (if (and (boundp 'write-file-functions)
            (not (featurep 'xemacs)))
       'write-file-functions
     'write-file-hooks)
   'planner-id-update-tasks-maybe nil t))

(add-hook 'planner-mode-hook 'planner-id-setup)
(add-hook 'planner-create-task-hook 'planner-id-add-task-id-maybe)
(setq planner-jump-to-linked-task-function 'planner-id-jump-to-linked-task)
(setq planner-find-task-function 'planner-id-find-task)

(provide 'planner-id)

;;; planner-id.el ends here
