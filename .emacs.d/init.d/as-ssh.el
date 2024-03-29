(defun as-read-ssh-agent-cache ()
  (interactive)
  (let* ((short-hostname (car (split-string (system-name) "\\.")))
         (cache-file (format "/tmp/.dsa-cache-%s-%s.el"
                             (user-real-login-name)
                             short-hostname)))
   (if (not (file-exists-p cache-file))
       (warn "Couldn't find %s" cache-file)
     (load cache-file)
     (message "Using ssh-agent with pid %s from %s"
              (getenv "SSH_AGENT_PID")
              cache-file))))

(as-read-ssh-agent-cache)

(provide 'as-ssh)
