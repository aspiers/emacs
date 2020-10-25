(defun as-read-ssh-agent-cache ()
  (interactive)
  (let* ((cache-file (format "/tmp/.dsa-cache-%s-%s.el"
                             (user-real-login-name)
                             (system-name))))
   (if (not (file-exists-p cache-file))
       (warn "Couldn't find %s" cache-file)
     (load cache-file)
     (message "Using ssh-agent with pid %s" (getenv "SSH_AGENT_PID")))))

(as-read-ssh-agent-cache)

(provide 'as-ssh)
