;;; ell.el --- Browse the Emacs Lisp List
;; Author: Jean-Philippe Theberge (jphil@emacslisp.org)
;;         and Stephen Eglen (stephen@cogsci.ed.ac.uk)
;; Created: 2000-05-22 - last update: 2000-08-23
;; Version: 0.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; Ell.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; Ell.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;;  The Emacs Lisp Lisp is available at
;;  http://anc.ed.ac.uk/~stephen/emacs/ell.html
;;
;;  If Stephen changes the layout of his web page, this package may
;;  stop to work correctly.  You may then need to upgrade this
;;  package.

;; After loading this file, just do "M-x ell-packages" to view the
;; list in its own buffer.
;; Use a prefix argument (i.e. "C-u M-x ell-package") to sort by 
;; author name.
;; 
;; Variables.
;; 
;; Set ell-locate to t (default nil) if you want emacs to
;; indicate which lisp files are already available on your system.
;;
;; Set ell-goto-addr to t (default nil) if you want to turn the
;; URLs into hyperlinks using the goto-addr package.
;;
;; To Do:
;;
;; + Do the http fetching in the background so emacs is not
;;   freezed on slow connections
;;
;; + Take consideration for the accented character in the sort by 
;;   author.
;;
;; + replace sort* with something else so the need for cl.el is
;;   no more required. (is this really necessary?)

;;; Code:

(require 'cl)				;needed for `sort*' routine.
(defvar ell-host "anc.ed.ac.uk")
(defvar ell-path "~stephen/emacs/ell.html")

(defvar ell-proxy-host nil
  "*If nil dont use proxy, else name of proxy server")

(defvar ell-proxy-port nil
  "*Port number of proxy server. Default is 80.")

(defvar ell-locate nil
  "*Non-nil if we want to test whether package is available on local system.
This will considerably slow down viewing of this buffer.")

(defvar ell-goto-addr nil
  "*Non-nil if we want to use turn URLs into hyperlinks.
If nil, you may want to use another package, such as ffap, instead.
(This feature may not be available in XEmacs.)")

(if ell-goto-addr
    (require 'goto-addr))

(defun ell-packages-list (&optional byauthor)
  "Insert the contents of URL at point."
  (if (get-buffer "*ell-temp-buffer*")
      (kill-buffer "*ell-temp-buffer*"))
  (with-temp-buffer
    (let* ((host ell-host)
           (path ell-path)
           (coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           (http (open-network-stream
                  "ell-retrieval-process"
                  "*ell-temp-buffer*"
                  (if ell-proxy-host ell-proxy-host host)
                  (if ell-proxy-port ell-proxy-port 80)))
           (pbuf (process-buffer http))
           (packagesL nil))
      (process-send-string
       http (concat 
	     "GET " 
	     (if ell-proxy-host 
		 (concat "http://" ell-host "/")
	       "/")
	     path " HTTP/1.0\r\n\r\n"))
      (while (eq (process-status http) 'open)
        (sleep-for 1))
      (insert-buffer pbuf)
      (kill-buffer pbuf)
      (goto-char (point-min))
      (while
          (re-search-forward
           "<a href=\"\\(.*\\)\">\\(.*\\.el\\)</a> *--- *\\(.*\\)<br>\n\
Contact: *\\(.*\\)<br>" nil t)
        (add-to-list
	 'packagesL
	 (list (buffer-substring (match-beginning 1)(match-end 1))
	       (buffer-substring (match-beginning 2)(match-end 2))
	       (buffer-substring (match-beginning 3)(match-end 3))
	       (buffer-substring (match-beginning 4)(match-end 4)))))
      (if byauthor 
	  (sort*  (mapcar (lambda (x)
			    (let ((authorl (split-string (car (last x)))))
			      (list (car x)(cadr x)(caddr x)(cadddr x)(car (last authorl)))))
			  packagesL)
		  'string-lessp
		  :key #'(lambda (x) (car (last x))))
	packagesL))))

(define-derived-mode ell-mode view-mode "Ell"
  "Major mode to display the Emacs lisp list.
Special commands:
\\{ellmode-map}"
  (setq ell-font-lock-keywords
	(list
	 '("^\\*\\(.*\\.el\\) -" 1 font-lock-comment-face)
	 '("^\\(.*\\.el\\) -" 1 font-lock-keyword-face)
	 '("^\\(ht\\|f\\)tp.*$" . font-lock-warning-face)
	 )
	)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(ell-font-lock-keywords nil t)))

;;;###autoload
(defun ell-packages (byauthor)
  "Display the Emacs Lisp list in a Emacs buffer."
  (interactive "P")
  (if (get-buffer "*ell-packages*")
      (kill-buffer "*ell-packages*"))
  (switch-to-buffer "*ell-packages*")
  (insert "==========================================")
  (center-line)(insert "\n")
  (insert "The Emacs Lisp List")(center-line)(insert "\n")
  (insert "by Stephen Eglen: stephen@anc.ed.ac.uk")(center-line)(insert "\n")
  (insert "==========================================")
  (center-line)(insert "\n\n")
  (if ell-locate
      (insert "Note: Files with an asterisk (*) \
are already installed on your system.\n\n"))

  (mapcar (lambda (x)
            (insert (format "%s - %s (by %s)\n%s\n\n"
                            (let ((name (cadr x)))
                              (if (and ell-locate (locate-library name))
                                  (concat "*" name)
                                name))
                            (car (cdr (cdr x)))
                            (car (cdr (cdr (cdr x))))
			    (car x))))
	  (if byauthor
	      (ell-packages-list t)
	    (reverse (ell-packages-list))))
  (ell-mode)
  (if ell-goto-addr
      ;; ELL is a big file, so ensure the maximum size for fontifying
      ;; addresses is okay.
      (progn
	(set (make-local-variable 'goto-address-fontify-maximum-size)
	     (+ 10 (buffer-size)))
	(goto-address)))
  
  (goto-char (point-min)))

(provide 'ell)

;;; ell.el ends here
