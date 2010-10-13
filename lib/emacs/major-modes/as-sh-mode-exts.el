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
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  ;; FIXME: ange-ftp API has changed
  ;;(and (not (ange-ftp-get-hash-entry  (buffer-file-name) ange-ftp-inodes-hashtable))
  (save-restriction
    (widen)
    (save-match-data
      (and
       (>= (point-max) 3)
       (equal (buffer-substring 1 3) "#!")
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes)))
              (new-mode (logior current-mode add-mode)))
         (or (/= (logand current-mode ?\111) 0)
             (= add-mode 0)
             (set-file-modes (buffer-file-name) new-mode)))))))
;;)

