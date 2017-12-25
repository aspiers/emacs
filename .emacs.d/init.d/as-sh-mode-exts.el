;; insert a skeleton for empty shell script 
(defun sh-ins-template ()
       (if (< (point-max) 3)
           (let ((pos))
               (goto-char (point-min))
               (insert "#!/bin/bash\n\n")
;;                        "# Compile by: /bin/sh -ex $* \n"
;;                        "# --------------------------------------------------\n")
;;                (setq pos (point))
;;                (insert "\n\n\n\n\n\n"
;;                        "############################## "
;;                        (file-name-nondirectory
;;                            (or (buffer-file-name) "UnKnown"))
;;                        " ##############################\n")
;;                (goto-char pos)
               )))

;; ensure saving of script sets the the executable bit (x)
;; By Noah Friedman <friedman@splode.com>
;; Date: Sun, 11 Jul 1999 14:08:16 -0700 (PDT)

(require 'ange-ftp)

(defun make-buffer-file-executable-if-script-p ()
  "Calls `make-buffer-file-executable' if buffer begins with '#!'."
  (interactive)
  (save-restriction
    (widen)
    (save-match-data
      (and
       (>= (point-max) 3)
       (equal (buffer-substring 1 3) "#!")
       (make-buffer-file-executable)))))

(defun make-buffer-file-executable ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes.

If buffer's filename does not exist on the filesystem (e.g. file within
a tar archive), do nothing."
  (interactive)
  ;; FIXME: ange-ftp API has changed
  ;;(and (not (ange-ftp-get-hash-entry  (buffer-file-name) ange-ftp-inodes-hashtable))
  (let* ((current-mode (file-modes (buffer-file-name))))
    (when current-mode
      (let* ((add-mode (logand ?\111 (default-file-modes)))
             (new-mode (logior current-mode add-mode)))
        (cond
         ((/= (logand current-mode ?\111) 0)
          (message "%s already executable (%0o)" (buffer-file-name) current-mode))
         ((= add-mode 0)
          (message "(default-file-modes) didn't permit any executable bits"))
         (t
          (set-file-modes (buffer-file-name) new-mode)
          (message "%s set to %0o" (buffer-file-name) new-mode)))))))
;;)

(provide 'as-sh-mode-exts)
