;;; planner-mhe.el --- MH-E integration for the Emacs Planner

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Christophe Garion <garion@supaero.fr>
;; Author: Sandra Jean Chua <sacha@free.net.ph>
;; Created: <2004-08-09 17:16:57 tof planner-mhe.el>
;; Time-stamp: <12/04/2005 19:26:58 Yann Hodique>
;; Keywords: planner, mh-e

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

;;; Commentary:

;; Inspired by planner-gnus (thanks to Sacha Chua)

;; This file adds annotations for MH-E messages. You will then be able
;; to use M-x planner-create-task-from-buffer to create tasks from
;; MH-E folder or show buffers with the correct annotation. If you
;; create an annotation from a mh-index folder, the message "real"
;; folder will be taken into account.

;;; Code:

(require 'planner)
(require 'mh-e)

(defun planner-mhe-get-message-folder-from-index ()
  "Returns the name of the message folder in a index folder
  buffer."
  (save-excursion
    (mh-index-previous-folder)
    (buffer-substring
     (planner-line-beginning-position)
     (planner-line-end-position))
    )
)

(defun planner-mhe-get-message-real-folder ()
  "Return the name of the current message real folder, so if you use
  sequences, it will now work."
(save-excursion
  (let* ((folder
         (if (equal major-mode 'mh-folder-mode)
             mh-current-folder
           ;; Refer to the show buffer
           mh-show-folder-buffer))
        (end-index (min (length mh-index-folder) (length folder)))
        )
    ;; a simple test on mh-index-data does not work, because
    ;; mh-index-data is always nil in a show buffer.
    (if (string= mh-index-folder (substring folder 0 end-index))
        (if (equal major-mode 'mh-show-mode)
            (save-window-excursion
              (when (buffer-live-p (get-buffer folder))
                (progn
                  (pop-to-buffer folder)
                  (planner-mhe-get-message-folder-from-index)
                  )
              ))
          (planner-mhe-get-message-folder-from-index)
          )
      folder
      )
    )))

(defun planner-mhe-get-message-folder ()
  "Return the name of the current message folder."
  (if (equal major-mode 'mh-folder-mode)
      mh-current-folder
    ;; Refer to the show buffer
    mh-show-folder-buffer)
  )

(defun planner-mhe-get-message-field (field)
  "Return a particular field of the current message."
  (save-excursion
    (let ((num
           (if (equal major-mode 'mh-folder-mode)
               (mh-get-msg-num nil)
             ;; Refer to the show buffer
             (mh-show-buffer-message-number)))
          (folder (planner-mhe-get-message-folder)))
      (car (split-string
            (with-temp-buffer
              (call-process (expand-file-name "anno" mh-progs)
                            nil t nil
                            folder
                            "-list" "-component" field
                            (number-to-string num))
              (buffer-string))
            "\n")))))

;;;###autoload
(defun planner-mhe-annotation ()
  "If called from a MH-E folder or message buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (or (equal major-mode 'mh-folder-mode)
            (equal major-mode 'mh-show-mode))
    (let ((from-header (planner-mhe-get-message-field "From"))
          (to-header (planner-mhe-get-message-field "To")))
      (planner-make-link
       (concat "mhe://" (planner-mhe-get-message-real-folder) "/"
               (planner-mhe-get-message-field "Message-Id"))
       (concat "E-Mail "
               (if (and planner-ignored-from-addresses
                        from-header
                        (string-match planner-ignored-from-addresses
                                      from-header))
                   ;; Mail from me, so use the To: instead
                   (concat "to " (planner-get-name-from-address
                                  to-header))
                 ;; Mail to me, so use the From:
                 (concat "from " (planner-get-name-from-address
                                  from-header))))))))

;;;###autoload
(defun planner-mhe-browse-url (url)
  "If this is a MH-E URL, jump to it."
  (when (string-match "^mhe://\\(.+\\)/\\([^>]+\\)" url)
    (let* ((folder (match-string 1 url))
	   (num (match-string 2 url))
	   (show-buf (concat "show-" folder)))
      (save-window-excursion
        (mh-visit-folder folder)
        (get-buffer-create show-buf)
        (mh-display-msg
         (string-to-number
          (if (= (aref num 0) ?<) ; message-id
              (car (split-string
                    (with-temp-buffer
                      (call-process
                       (expand-file-name "pick" mh-progs)
                       nil t nil
                       folder
                       "--message-id"
                       num)
                      (buffer-string))
                    "\n"))
            num))
         folder)
        )
      (pop-to-buffer show-buf))))

;(fset 'planner-get-from 'planner-gnus-get-address)
;(fset 'planner-get-message-id 'planner-gnus-get-message-id)
(custom-add-option 'planner-annotation-functions
                   'planner-mhe-annotation)
(add-hook 'planner-annotation-functions 'planner-mhe-annotation)
(planner-add-protocol "mhe" 'planner-mhe-browse-url nil)

; to set the mh-path etc. variables
(mh-find-path)

(provide 'planner-mhe)
;;; planner-mhe.el ends here
