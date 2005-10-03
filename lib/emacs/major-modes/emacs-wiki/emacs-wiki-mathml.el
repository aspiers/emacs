;;; emacs-wiki-mathml.el --- Provide MathML support for emacs-wiki

;; Copyright (C) 2004 Li Daobing
;; Copyright (C) 2004 Michael Olson

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-menu.el
;; Keywords: emacs-wiki mathml hypermedia
;; Author: Li Daobing (lidaobing AT gmail DOT com)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Provide MathML support for emacs-wiki
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;_* Commentary

;;;_ + Startup

;; 1. Get a copy of itex2MML and install it to `/usr/bin' or
;;    `/usr/local/bin'.
;;
;;    You can get a copy from
;;    `http://pear.math.pitt.edu/mathzilla/itex2mml.tar.gz'.
;;
;; 2. Copy `itex2MML.py' to `/usr/bin' or `/usr/local/bin', if you
;;    do not have this file, create it and do a `chmod a+x itex2MML.py'.
;;    Its content is the following.
;;
;; #!/usr/bin/env python
;; """A wrap for itex2MML
;;
;; Delete the extra blank line.
;;
;; You can use it as itex2MML.
;;
;; For example:
;;
;; echo '$a_b$' | itex2MML.py
;; """
;;
;; import sys
;; import os
;;
;; def main():
;;     fin, fo = os.popen2('itex2MML')
;;     fin.write(sys.stdin.read())
;;     fin.close()
;;     for line in fo:
;;         line = line.strip()
;;         if line:
;;             print line
;;
;; if __name__ == '__main__':
;;     main()
;;
;; 3. Put the `emacs-wiki-math.el' into your `load-path'.
;;
;; 4. Add the following to your .emacs file.
;;
;;    (require 'emacs-wiki-mathml)

(require 'emacs-wiki)

(defun emacs-wiki-publishing-xml-encoding ()
  "Set encoding of this file to `emacs-wiki-publishing-xml-encoding'."
  (if (stringp emacs-wiki-meta-content-coding)
      emacs-wiki-meta-content-coding
    (emacs-wiki-transform-content-type
     (or buffer-file-coding-system
         emacs-wiki-coding-default))))

;;; TODO: Rewrite this for Muse.

;; (setq emacs-wiki-publishing-header
;;       "<?xml version=\"1.0\" encoding=\"<lisp>(emacs-wiki-publishing-xml-encoding)</lisp>\"?>
;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN\"
;; \"http://www.w3.org/TR/MathML2/dtd/xhtml-math11-f.dtd\">
;; <html xmlns=\"http://www.w3.org/1999/xhtml\"
;; xmlns:xlink=\"http://www.w3.org/1999/xlink\">
;;   <head>
;;     <title><lisp>(emacs-wiki-page-title)</lisp></title>
;;     <meta name=\"generator\" content=\"emacs-wiki.el\" />
;;     <link rev=\"made\" href=\"<lisp>emacs-wiki-maintainer</lisp>\" />
;;     <link rel=\"home\" href=\"<lisp>(emacs-wiki-published-name
;;                                      emacs-wiki-default-page)</lisp>\" />
;;     <link rel=\"index\" href=\"<lisp>(emacs-wiki-published-name
;;                                       emacs-wiki-index-page)</lisp>\" />
;;     <lisp>emacs-wiki-style-sheet</lisp>
;;   </head>
;;   <body>
;;     <h1><lisp>(emacs-wiki-page-title)</lisp></h1>
;;     <!-- Page published by Emacs Wiki begins here -->\n")

;; (setq emacs-wiki-publishing-footer "
;;     <!-- Page published by Emacs Wiki ends here -->
;;     <div class=\"navfoot\">
;;       <hr />
;;       <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
;;         <tr>
;;           <td align=\"left\" style=\"width: 33%;\">
;;             <lisp>
;;               (if buffer-file-name
;;                   (concat
;;                    \"<span class=\\\"footdate\\\">Updated: \"
;;                    (format-time-string emacs-wiki-footer-date-format
;;                     (nth 5 (file-attributes buffer-file-name)))
;;                    (and emacs-wiki-serving-p
;;                         (emacs-wiki-editable-p (emacs-wiki-page-name))
;;                         (concat
;;                          \" / \"
;;                          (emacs-wiki-link-href
;;                           (concat \"editwiki?\" (emacs-wiki-page-name))
;;                           \"Edit\")))
;;                    \"</span>\"))
;;             </lisp>
;;           </td>
;;           <td align=\"center\" style=\"width: 34%;\">
;;             <span class=\"foothome\">
;;               <lisp>
;;                 (concat
;;                  (and (emacs-wiki-page-file emacs-wiki-default-page t)
;;                       (not (emacs-wiki-private-p emacs-wiki-default-page))
;;                       (concat
;;                        (emacs-wiki-link-href emacs-wiki-default-page \"Home\")
;;                        \" / \"))
;;                  (emacs-wiki-link-href emacs-wiki-index-page \"Index\")
;;                  (and (emacs-wiki-page-file \"ChangeLog\" t)
;;                       (not (emacs-wiki-private-p \"ChangeLog\"))
;;                       (concat
;;                        \" / \"
;;                        (emacs-wiki-link-href \"ChangeLog\" \"Changes\"))))
;;               </lisp>
;;             </span>
;;           </td>
;;           <td align=\"right\" style=\"width: 33%;\">
;;             <lisp>
;;               (if emacs-wiki-serving-p
;;                   (concat
;;                    \"<span class=\\\"footfeed\\\">\"
;;                    (emacs-wiki-link-href \"searchwiki?get\" \"Search\")
;;                    (and buffer-file-name
;;                         (concat
;;                          \" / \"
;;                          (emacs-wiki-link-href
;;                           (concat \"searchwiki?q=\" (emacs-wiki-page-name))
;;                           \"Referrers\")))
;;                    \"</span>\"))
;;             </lisp>
;;           </td>
;;         </tr>
;;       </table>
;;     </div>
;;   </body>
;; </html>
;; ")

;; (setq emacs-wiki-publishing-file-suffix ".xml")

(add-to-list 'emacs-wiki-markup-tags
             '("math" t t t emacs-wiki-math-tag))

(defcustom emacs-wiki-latex-to-mathml
  (if (or (featurep 'executable)
          (load "executable" t t))
      (executable-find "itex2MML.py"))
  "Program to use to convert Latex text to MathML."
  :type 'string
  :group 'emacs-wiki-publish)

(defun emacs-wiki-math-tag (beg end attrs highlight-p)
  (if highlight-p
      (goto-char end)
    (if emacs-wiki-latex-to-mathml
        (emacs-wiki-command-tag
         beg end (list (cons "file" emacs-wiki-latex-to-mathml))))
      (emacs-wiki-escape-html-specials end)
      (goto-char end)
      (add-text-properties
       beg (point) '(rear-nonsticky (read-only) read-only t))))

(provide 'emacs-wiki-mathml)
;;; emacs-wiki-mathml.el ends here
