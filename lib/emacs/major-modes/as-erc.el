(defun erc-generate-log-directory-name-network (buffer target nick server port)
  "Generates a log-file name using the network name rather than server name.
This results in a directory name of the form #channel@network.txt.
This function is a possible value for `erc-log-channels-directory'."
  (require 'erc-networks)
  (let ((dir
         (expand-file-name
          (concat "~/.erc/log/"
                  (or (with-current-buffer buffer (erc-network-name)) server)))))
    (make-directory dir 'parents)
    dir))

(defun erc-generate-log-file-name-target (buffer target nick server port)
  "Generates a log-file name using the target (channel or /query user).
This results in a file name of the form #channel.txt or user.txt.
This function is a possible value for `erc-generate-log-file-name-function'."
  (require 'erc-networks)
  (let ((file (concat target ".txt")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))

(provide 'as-erc)
