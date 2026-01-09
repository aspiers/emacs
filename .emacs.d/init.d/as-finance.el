(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "\\.journal\\'" buffer-file-name)
              (require 'ob-hledger nil t))))

(provide 'as-finance)