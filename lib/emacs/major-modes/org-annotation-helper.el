;;; org-annotation-helper.el --- start remember from a web browser
;;
;; Author: bzg AT altern DOT org
;; Keywords: org remember
;;
;;; Commentary:
;;
;; [bzg:] This is an adapted version of the planner-mode extension the
;; was first posted by Geert Kloosterman <g.j.kloosterman@gmail.com> on
;; the Planner mailing list.  All comments below are his.
;;
;; We want to be able to pass a URL and document title directly from a
;; web browser to Emacs.
;;
;; We define a remember:// url handler in the browser and use a shell
;; script to handle the protocol.  This script passes the information
;; to a running Emacs process (using emacsclient/gnuclient).  We use 
;; bookmarklets to create the remember:// urls dynamicly.
;;
;; The protocol types currently recognized are:
;; 
;; remember://     start `remember' with the url and title filled in
;; annotation://   similar to `planner-annotation-as-kill' (org?)
;;
;; The urls used internally will have the following form:
;;
;;   remember://<the web page url>%1C<the title>
;;
;; The title will be url-hex-encoded.  "%1C" is the (url-encoded) low
;; ascii value for the field separator.
;;
;;
;; The bookmarklets:
;;
;; javascript:location.href='remember://' + location.href + '%1C' + escape(document.title)
;; javascript:location.href='annotation://' + location.href + '%1C' + escape(document.title)
;;
;; The helper script:
;;
;; #!/bin/sh
;; # org-annotation-helper -- pass a remember-url to emacs
;; #
;; # Author: Geert Kloosterman <g.j.kloosterman@gmail.com>
;; # Date: Sat Nov 19 22:33:18 2005
;; 
;; if [ -z "$1" ]; then
;;     echo "$0: Error: no arguments given!" 1>&2
;;     exit 1
;; fi
;; 
;; # For years I've been using Martin Schwenke's dtemacs script to start
;; # Emacs.  The script uses gnuclient to connect to Emacs and starts a
;; # new Emacs process when necessary.
;; # See http://www.meltin.net/hacks/emacs/
;; #
;; # dtemacs -batch -eval "(progn (bzg/org-annotation-helper \"$1\" ) \"\")"
;; 
;; # As of Emacs 22 emacsclient will work too
;; emacsclient --eval "(progn (bzg/org-annotation-helper \"$1\" ) nil)"
;; 
;; # EOF

;; Adding a protocol handler
;; -------------------------
;;
;; Firefox
;;
;; To add a protocol handler (eg: remember://) in Firefox, take the
;; following steps:
;;
;; - type in "about:config" in the location bar
;; - right click, select New --> String
;; - the name should be "network.protocol-handler.app.remember" 
;; - the value should be the executable, eg. "org-annotation-helper".
;;   At least under Linux this does not need to be the full path to 
;;   the executable.
;;
;; See http://kb.mozillazine.org/Register_protocol for more details.
;;
;; Opera
;;
;; In Opera add the protocol in the Preferences->Advanced->Programs
;; dialog.


;;; Code:

(require 'url)

(autoload 'url-unhex-string "url")

(defun bzg/org-annotation-helper (urlstring)
  "Process an externally passed remember:// style url.

URLSTRING consists of a protocol part and a url and title,
separated by %1C.

The protocol types currently recognized are:

remember://     start `remember' with the url and title
annotation://   similar to `org-annotation-as-kill'."
  (let ((remember-annotation-functions nil))
    ;; The `parse-url' functions break on the embedded url,
    ;; since our format is fixed we'll split the url ourselves.
    (if (string-match  "^\\([^:]*\\):\\(/*\\)\\(.*\\)" urlstring)
      (let* ((proto (match-string 1 urlstring))
            (url+title (match-string 3 urlstring))
            (splitparts (split-string url+title "%1C"))
            (url (car splitparts))
            (title (cadr splitparts))
            orglink)
            
        (setq title (if (> (length title) 0) (url-unhex-string title)))
        (setq orglink (org-make-link-string url title))

        (raise-frame)
        (cond ((equal proto "remember")
               (remember (concat "\n\n" orglink)))
              ((equal proto "annotation")
               (message "Copied '%s' to the kill-ring." orglink)
               (kill-new orglink))
              (t 
               (error "unrecognized org-helper protocol"))))
      (error "could not parse argument"))))

;;; org-annotation-helper.el ends here
