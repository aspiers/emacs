;;; emacs-wiki-httpd.el --- Provide support for serving Emacs Wiki pages

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-httpd.el
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain Emacs-friendly Wikis in a local directory
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki HTTP Server (using httpd.el)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki-project)
(require 'emacs-wiki)
(require 'httpd)
(require 'cgi)
(require 'vc)

(defgroup emacs-wiki-http nil
  "Options controlling the behaviour of the Emacs Wiki HTTP server.

So, you want to run a Wiki server based on Emacs?  It's simple.
First, you will need two other scripts: httpd.el and cgi.el.  Both of
them can be downloaded from Eric Mardsen's page:

  http://www.chez.com/emarsden/downloads/

Once you have those two scripts, you must decide between two different
methods of serving pages directly from Emacs:

* PERSISTED INVOCATION SERVER

This scheme keeps a dedicated Emacs process running, solely for the
purpose of rendering pages.  It has the disadvantage of occupying
virtual memory when no one is requesting pages.  It has the advantage
of being 50 times faster than the next method.

To use the persisted invoctaion server, you must download the Python
script `httpd-serve' from the same website where you downloaded
emacs-wiki:

  http://www.gci-net.com/~johnw/emacs.html

Once you have have downloaded the script, running it is simple:

  ./httpd-serve --daemon --port 8080 --load /tmp/my-emacs-wiki \
      [path to your HTML files]

The file `/tmp/my-emacs-wiki.el' should contain all the customizations
required by your Wiki setup.  This is how the server knows where to
find your pages.  This script MUST contain the following line:

  (load \"emacs-wiki\")

That's it.  You should now be able to access your Wiki repository at
localhost:8080.  Only world-readable will be visible, and only
world-writable can be edited over HTTP.

* AN EMACS SPAWNED PER REQUEST

The old method of serving Wiki pages directly is to spawn an Emacs
invocation for every request.  This has the advantage of being a far
simpler approach, and it doesn't consume memory if no one is
requesting pages.  The disadvantage is that it's hideously slow, and
multiple requests may bog down your machine's supply of virtual
memory.

Anyway, to use this approach, add the following line to your
/etc/inted.conf file:

  8080 stream tcp nowait.10000 nobody /usr/local/bin/emacs-httpd

The emacs-httpd script should look something like this:

  #!/bin/sh
  /usr/bin/emacs -batch --no-init-file --no-site-file \\
      -l httpd -l cgi -l emacs-wiki \\
      --eval \"(setq httpd-document-root emacs-wiki-publishing-directory \\
                    emacs-wiki-maintainer \\\"mailto:joe@where.com\\\")\" \\
      -f httpd-serve 2> /dev/null

Emacs-wiki will now serve pages directly on port 8080.  Note that if
you need to configure any variables in emacs-wiki, you will have to
repeat those configurations in the emacs-httpd script.

Note: If you have the 'stopafter' tool installed, it's a good idea to
put a limit on how much time each Emacs process is allowed.  And if
you want to render planner.el pages, you'll need to make another
modification.  Here is a more complete example:

  #!/bin/sh
  /usr/bin/stopafter 60 KILL /usr/bin/emacs \\
      -batch --no-init-file --no-site-file \\
      -l httpd -l cgi -l emacs-wiki -l planner \\
      --eval \"(progn \\
         (setq httpd-document-root emacs-wiki-publishing-directory \\
               emacs-wiki-maintainer \\\"mailto:joe@where.com\\\") \\
         (planner-update-wiki-project))\" \\
      -f httpd-serve 2> /dev/null"
  :group 'emacs-wiki)

(defcustom emacs-wiki-http-search-form
  "
<form method=\"GET\" action=\"/searchwiki?get\">
  <center>
    Search for: <input type=\"text\" size=\"50\" name=\"q\" value=\"\" />
    <input type=\"submit\" value=\"Search!\" />
  </center>
</form>\n"
  "The form presenting for doing searches when using httpd.el."
  :type 'string
  :group 'emacs-wiki-http)

(defcustom emacs-wiki-http-support-editing t
  "If non-nil, allow direct editing when serving over httpd.el.
Note that a page can be edited only if it is world-writable and
`emacs-wiki-use-mode-flags' is set, or if it matches one of the
regexps in `emacs-wiki-editable-pages'."
  :type 'boolean
  :group 'emacs-wiki-http)

(defcustom emacs-wiki-http-edit-form
  "
<form method=\"POST\" action=\"/changewiki?post\">
  <textarea name=\"%PAGE%\" rows=\"25\" cols=\"80\">%TEXT%</textarea>
  <center>
    <input type=\"submit\" value=\"Submit changes\" />
  </center>
</form>\n"
  "The form presenting for doing edits when using httpd.el."
  :type 'string
  :group 'emacs-wiki-http)

(defun emacs-wiki-http-send-buffer (&optional title modified code
                                              msg no-markup)
  "Markup and send the contents of the current buffer via HTTP."
  (unless no-markup (emacs-wiki-replace-markup title))
  (httpd-send (or code 200) (or msg "OK")
              "Server: emacs-wiki.el/2.40" httpd-endl
              "Connection: close" httpd-endl
              "MIME-Version: 1.0" httpd-endl
              "Date: " (format-time-string "%a, %e %b %Y %T %Z")
              httpd-endl
              "From: " (substring emacs-wiki-maintainer 7) httpd-endl)
  (when modified
    (httpd-send-data "Last-Modified: "
                     (format-time-string "%a, %e %b %Y %T %Z" modified)
                     httpd-endl))
  (httpd-send-data "Content-Type: text/html; charset=iso-8859-1" httpd-endl
                   "Content-Length: " (number-to-string (1- (point-max)))
                   httpd-endl httpd-endl
                   (buffer-string))
  (httpd-send-eof))

(defun emacs-wiki-http-reject (title msg &optional annotation)
  (emacs-wiki-with-temp-buffer
    (insert msg ".\n")
    (if annotation
        (insert annotation "\n"))
    (emacs-wiki-http-send-buffer title nil 404 msg)))

(defvar emacs-wiki-buffer-mtime nil)
(make-variable-buffer-local 'emacs-wiki-buffer-mtime)

(defun emacs-wiki-sort-buffers (l r)
  (let ((l-mtime (with-current-buffer l
                   emacs-wiki-buffer-mtime))
        (r-mtime (with-current-buffer r
                   emacs-wiki-buffer-mtime)))
    (cond
     ((and (null l-mtime) (null r-mtime)) l)
     ((null l-mtime) r)
     ((null r-mtime) l)
     (t (emacs-wiki-time-less-p r-mtime l-mtime)))))

(defun emacs-wiki-winnow-list (entries &optional predicate)
  "Return only those ENTRIES for which PREDICATE returns non-nil."
  (let ((flist (list t))
        valid p)
    (let ((entry entries))
      (while entry
        (if (funcall predicate (car entry))
            (nconc flist (list (car entry))))
        (setq entry (cdr entry))))
    (cdr flist)))

(defcustom emacs-wiki-max-cache-size 64
  "The number of pages to cache when serving over HTTP.
This only applies if set while running the persisted invocation
server.  See main documentation for the `emacs-wiki-http'
customization group."
  :type 'integer
  :group 'emacs-wiki-http)

(defun emacs-wiki-prune-cache ()
  "If the page cache has become too large, prune it."
  (let* ((buflist (sort (emacs-wiki-winnow-list
                         (buffer-list)
                         (function
                          (lambda (buf)
                            (with-current-buffer buf
                              emacs-wiki-buffer-mtime))))
                        'emacs-wiki-sort-buffers))
         (len (length buflist)))
    (while (> len emacs-wiki-max-cache-size)
      (kill-buffer (car buflist))
      (setq len (1- len)))))

(defun emacs-wiki-render-page (name)
  "Render the wiki page identified by NAME.
When serving from a dedicated Emacs process (see the httpd-serve
script), a maximum of `emacs-wiki-max-cache-size' pages will be cached
in memory to speed up serving time."
  (if (equal name emacs-wiki-index-page)
      (with-current-buffer (emacs-wiki-generate-index t t)
        (emacs-wiki-http-send-buffer "Wiki Index")
        (kill-buffer (current-buffer)))
    (let ((file (and (not (emacs-wiki-private-p name))
                     (cdr (assoc name (emacs-wiki-file-alist)))))
          (inhibit-read-only t))
      (if (null file)
          (emacs-wiki-http-reject "Page not found"
                                  (format "Wiki page %s not found" name))
        (set-buffer (get-buffer-create file))
        (let ((modified-time (nth 5 (file-attributes file))))
          (when (or (null emacs-wiki-buffer-mtime)
                    (emacs-wiki-time-less-p emacs-wiki-buffer-mtime
                                            modified-time))
            (erase-buffer)
            (setq emacs-wiki-buffer-mtime modified-time))
          (goto-char (point-max))
          (if (not (bobp))
              (emacs-wiki-http-send-buffer nil emacs-wiki-buffer-mtime
                                           nil nil t)
            (insert-file-contents file t)
            (cd (file-name-directory file))
            (emacs-wiki-maybe)
            (emacs-wiki-http-send-buffer nil emacs-wiki-buffer-mtime)))
        (set-buffer-modified-p nil)
        (emacs-wiki-prune-cache)))))

(defun emacs-wiki-wikify-search-results (term)
  "Convert the current buffer's grep results into a Wiki form."
  (goto-char (point-max))
  (forward-line -2)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (kill-line 2)
  (let ((results (list t)))
    (while (re-search-forward "^.+/\\([^/:]+\\):\\s-*[0-9]+:\\(.+\\)" nil t)
      (let ((page (match-string 1)))
        (unless (or (emacs-wiki-private-p page)
                    (string-match emacs-wiki-file-ignore-regexp page))
          (let ((text (match-string 2))
                (entry (assoc page results)))
            (if entry
                (nconc (cdr entry) (list text))
              (nconc results (list (cons page (list text)))))))))
    (delete-region (point-min) (point-max))
    (setq results
          (sort (cdr results)
                (function
                 (lambda (l r)
                   (string-lessp (car l) (car r))))))
    (while results
      (unless (emacs-wiki-private-p (caar results))
        (insert "[[" (caar results) "]] ::\n  <p>")
        (let ((hits (cdar results)))
          (while hits
            (while (string-match "</?lisp>" (car hits))
              (setcar hits (replace-match "" t t (car hits))))
            (while (string-match (concat "\\([^*?[/>]\\)\\<\\(" term "\\)\\>")
                                 (car hits))
              (setcar hits (replace-match "\\1<strong>\\2</strong>"
                                          t nil (car hits))))
            (insert "  > <verbatim>" (car hits) "</verbatim>\n")
            (setq hits (cdr hits))))
        (insert "</p>\n\n"))
      (setq results (cdr results)))))

(defun emacs-wiki-setup-edit-page (page-name)
  (insert "<verbatim>" emacs-wiki-http-edit-form "</verbatim>")
  (goto-char (point-min))
  (search-forward "%PAGE%")
  (replace-match page-name t t)
  (search-forward "%TEXT%")
  (let ((beg (match-beginning 0))
        (file (emacs-wiki-page-file page-name))
        end)
    (delete-region beg (point))
    (when file
      (insert-file-contents file)
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char (point-min))
        (emacs-wiki-escape-html-specials)))))

(defun emacs-wiki-http-changewiki (&optional content)
  "Change the contents of Wiki page, using the results of a POST request."
  (require 'cgi)
  (unless content
    (goto-char (point-min))
    (if (not (re-search-forward "Content-length:\\s-*\\([0-9]+\\)" nil t))
        (emacs-wiki-http-reject "Content-length missing"
                                "No Content-length for POST request"
                                (concat "Header received was:\n\n<example>"
                                        (buffer-string) "</example>\n"))
      (let ((content-length (string-to-number (match-string 1))))
        (erase-buffer)
        (read-event)                    ; absorb the CRLF separator
        (let ((i 0))
          (while (< i content-length)
            (insert (read-event))
            (setq i (1+ i))))))
    (setq content (buffer-string)))
  (when content
    (let* ((result (cgi-decode content))
           (page (caar result))
           (text (cdar result))
           (len (length text))
           (require-final-newline t)
           (pos 0) illegal user)
      (if (not (emacs-wiki-editable-p page))
          (emacs-wiki-http-reject
           "Editing not allowed"
           (format "Editing Wiki page %s is not allowed" page))
        (while (and (null illegal)
                    (setq pos (string-match
                               (concat "<\\s-*\\([^>"
                                       emacs-wiki-regexp-blank
                                       "]+\\)")
                               text pos)))
          (setq pos (match-end 0))
          (if (assoc (match-string 1 text) emacs-wiki-dangerous-tags)
              (setq illegal (match-string 1 text))))
        (if illegal
            (emacs-wiki-http-reject
             "Disallowed tag used"
             (format "Public use of &lt;%s&gt; tag not allowed" illegal))
          (emacs-wiki-find-file page)
          (if (setq user (file-locked-p buffer-file-name))
              (emacs-wiki-http-reject
               "Page is locked"
               (format "The page \"%s\" is currently being edited by %s."
                       page (if (eq user t) (user-full-name) user)))
            (let ((inhibit-read-only t)
                  (delete-old-versions t))
              (erase-buffer)
              (insert (if (eq (aref text (1- len)) ?%)
                          (substring text 0 (1- len))
                        text))
              (goto-char (point-min))
              (while (re-search-forward "\r$" nil t)
                (replace-match "" t t))
              (save-buffer)
              ;; this is 0666 - there is no read syntax for octals which
              ;; works across all emacsen
              (let ((oct 438))
                (when (/= (file-modes buffer-file-name) oct)
                  (set-file-modes buffer-file-name oct)))
              (kill-buffer (current-buffer)))
            (emacs-wiki-with-temp-buffer
              (emacs-wiki-file-alist)   ; force re-check
              (insert "<redirect url=\"" page "\" delay=\"3\">")
              (insert "Thank you, your changes have been saved to " page)
              (insert ".  You will be redirected to "
                      "the new page in a moment.")
              (insert "</redirect>")
              (emacs-wiki-http-send-buffer "Changes Saved"))))))))

(defvar httpd-vars nil)

(defsubst httpd-var (var)
  "Return value of VAR as a URL variable.  If VAR doesn't exist, nil."
  (cdr (assoc var httpd-vars)))

(defsubst httpd-var-p (var)
  "Return non-nil if VAR was passed as a URL variable."
  (not (null (assoc var httpd-vars))))

(defun emacs-wiki-serve-page (page content)
  (let ((handled t))
    (cond
     ((string-match "\\`wiki\\?\\(.+\\)" page)
      (emacs-wiki-render-page (match-string 1 page)))

     ((string-match "\\`editwiki\\?\\(.+\\)" page)
      (let ((page-name (match-string 1 page)))
        (if (not (emacs-wiki-editable-p page-name))
            (emacs-wiki-http-reject "Editing not allowed"
                                    "Editing this Wiki page is not allowed")
          (emacs-wiki-with-temp-buffer
            (emacs-wiki-setup-edit-page page-name)
            ;; this is required because of the : in the name
            (emacs-wiki-http-send-buffer
             (concat "Edit Wiki Page: " page-name))))))

     ((string-match "\\`searchwiki\\?get" page)
      (emacs-wiki-with-temp-buffer
        (insert "<verbatim>" emacs-wiki-http-search-form "</verbatim>")
        (emacs-wiki-http-send-buffer "Search Wiki Pages")))

     ((string-match "\\`searchwiki\\?q=\\(.+\\)" page)
      (let ((compilation-scroll-output nil)
            (term (match-string 1 page)))
        (unintern 'start-process)
        (require 'compile)
        (with-current-buffer (emacs-wiki-grep term)
          (emacs-wiki-wikify-search-results term)
          (emacs-wiki-http-send-buffer "Search Results")
          (kill-buffer (current-buffer)))))

     ((string-match "\\`changewiki\\?post" page)
      (emacs-wiki-http-changewiki content))

     ((string-match "\\`diffwiki\\?\\(.+\\)" page)
      ;; jww (2001-04-20): This code doesn't fully work yet.
      (emacs-wiki-find-file (match-string 1 page))
      (require 'vc)
      (require 'vc-hooks)
      (let ((curr-ver (vc-workfile-version buffer-file-name)))
        (vc-version-diff buffer-file-name
                         curr-ver (vc-previous-version curr-ver))
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (when (re-search-forward "^diff" nil t)
            (forward-line)
            (delete-region (point-min) (point)))
          (insert "<verbatim><pre>")
          (emacs-wiki-escape-html-specials)
          (goto-char (point-max))
          (if (re-search-backward "^Process.*killed" nil t)
              (delete-region (point) (point-max)))
          (insert "</verbatim></pre>")
          (emacs-wiki-http-send-buffer "Diff Results"))))

     (t
      (setq handled nil)))
    handled))

(defun emacs-wiki-serve (page &optional content)
  "Serve the given PAGE from this emacs-wiki server."
  ;; index.html is really a reference to the main Wiki page
  (if (string= page "index.html")
      (setq page (concat "wiki?" emacs-wiki-default-page)))

  ;; handle the actual request
  (let ((vc-follow-symlinks t)
        (emacs-wiki-report-threshhold nil)
        (emacs-wiki-serving-p t)
        httpd-vars project)
    (save-excursion
      ;; process any CGI variables, if cgi.el is available
      (if (string-match "\\`\\([^&]+\\)&" page)
          (setq httpd-vars
                (and (fboundp 'cgi-decode)
                     (cgi-decode (substring page (match-end 0))))
                page (match-string 1 page)))
      (setq project (httpd-var "project"))
      (if project
          (with-emacs-wiki-project project
            (emacs-wiki-serve-page page content))
        (emacs-wiki-serve-page page content)))))

(if (featurep 'httpd)
    (httpd-add-handler "\\`\\(index\\.html\\|.*wiki\\(\\?\\|\\'\\)\\)"
                       'emacs-wiki-serve))

(defun emacs-wiki-editable-p (name)
  "Return non-nil if NAME is a page that may be publically edited.
If the page does not exist, the page will be created if: mode flags
are not being checked, and it is a page listed in
`emacs-wiki-editable-pages', or the first directory in
`emacs-wiki-directories' is writable.  In either case, the new page
will be created in the first directory in `emacs-wiki-directories'."
  (if (and name emacs-wiki-http-support-editing)
      (if emacs-wiki-use-mode-flags
          (let ((filename
                 (file-truename
                  (or (emacs-wiki-page-file name t)
                      (expand-file-name name (car emacs-wiki-directories))))))
            (if (file-exists-p filename)
                (eq ?w (aref (nth 8 (file-attributes filename)) 8))
              (eq ?w (aref (nth 8 (file-attributes
                                   (file-name-directory filename))) 8))))
        (let ((editable-pages emacs-wiki-editable-pages) editable)
          (while editable-pages
            (if (string-match (car editable-pages) name)
                (setq editable t editable-pages nil)
              (setq editable-pages (cdr editable-pages))))
          editable))))
(provide 'emacs-wiki-httpd)

;;; emacs-wiki-httpd.el ends here
