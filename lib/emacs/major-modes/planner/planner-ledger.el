;;; planner-ledger.el --- ledger support for planner

;; Copyright (c) 2004 Will Glozer (will AT glozer DOT net)

;;; Commentary:

;; This planner module provides integration between planner and
;; John Wiegley's ledger accounting program available at:
;;
;;    http://newartisans.com/johnw/ledger.tar.gz
;;
;; planner-ledger can insert a ledger balance overview and a list of pending
;; transactions into a planner day page.  To do so, simply add a hook:
;;
;;   (add-hook 'planner-goto-hook 'planner-ledger-insert-maybe)
;;
;; and make sure `planner-day-page-template' includes sections that match
;; `planner-ledger-balance-regexp' and `planner-ledger-pending-regexp'.
;;
;; planner-ledger can also create a new ledger entry based on a planner
;; task that matches `planner-ledger-payment-task-regexp', which by default
;; matches entries like:
;;
;;   #B0  _ payment due: Payee, $100.00 from 2004.07.01
;;
;; Bind `planner-ledger-add-entry-from-task' to a convenient key stroke and
;; execute it when in a payment task.

(require 'ledger)
(require 'planner)

;;; Code:
(defcustom planner-ledger-balance-regexp
  "^* Ledger\n\n$"
  "Section marker for insertion of ledger balance."
  :type 'regexp
  :group 'planner-ledger)

(defcustom planner-ledger-pending-regexp
  "^** Pending Transactions\n\n$"
  "Section marker for insertion of pending ledger transactions."
  :type 'regexp
  :group 'planner-ledger)

(defcustom planner-ledger-balance-accounts
  '("Assets" "Liabilities" "-Equity")
  "Accounts to include or exclude from ledger balance overview."
  :type '(repeat string)
  :group 'planner-ledger)

(defcustom planner-ledger-balance-args
  '("-s" "-e" "\"next month\"" "balance")
  "Command line arguments for ledger balance."
  :type '(repeat string)
  :group 'planner-ledger)

(defcustom planner-ledger-payment-task-regexp
  (concat planner-task-regexp
          "payment\\s-+due:\\s-+\\([^,]+?\\),\\s-*\\([[:graph:]]+\\)")
  "Regular expression matching planner tasks for ledger payment.
The first parenthesized group should match the payee. The second
group should match the amount.

Example task:
#A0 _ payment due: foobar, $1000.00 some comment here"
  :type 'regexp
  :group 'planner-ledger)

;;;###autoload
(defun planner-ledger-insert-maybe ()
  "Maybe insert ledger sections into a planner page."
  (interactive)
  (apply 'planner-ledger-insert-section-maybe
         planner-ledger-balance-regexp
         (append planner-ledger-balance-args
                planner-ledger-balance-accounts))
  (planner-ledger-insert-section-maybe planner-ledger-pending-regexp
                                       "-U" "register" "."))

(defun planner-ledger-insert-section-maybe (regexp &rest ledger-args)
  "Maybe insert a ledger section into a planner page.
Argument REGEXP is the section heading to find.  Optional argument
LEDGER-ARGS contains the arguments to pass to
`ledger-run-ledger'."
  (save-excursion
    (when (re-search-forward regexp nil t)
      (apply 'ledger-run-ledger ledger-args))))

(defun planner-ledger-add-entry-from-task ()
  "Add a new ledger entry from the task at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward planner-ledger-payment-task-regexp
                           (planner-line-end-position)
                           t)
        (let* ((payee  (match-string 1))
               (amount (match-string 2))
               (date   (planner-filename-to-calendar-date (buffer-name)))
               (buffer (find-buffer-visiting ledger-data-file)))
          (unless buffer (setq buffer (find-file ledger-data-file)))
          (pop-to-buffer buffer)
          (ledger-add-entry (format "%d/%d/%d %s %s"
                                    (extract-calendar-year date)
                                    (extract-calendar-month date)
                                    (extract-calendar-day date)
                                    payee
                                    amount)))
      (message "Not in a ledger payment task"))))

(provide 'planner-ledger)

;;; planner-ledger.el ends here
