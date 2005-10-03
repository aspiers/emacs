;;; planner-authz.el --- restrict portions of published pages using Mason

;; Copyright 2004 by Andrew J. Korty <ajk@iu.edu>

;; Emacs Lisp Archive Entry
;; Filename: planner-authz.el
;; Version: $Revision$
;; Keywords: hypermedia
;; Author: Andrew J. Korty <ajk@iu.edu>
;; Maintainer: Andrew J. Korty <ajk@iu.edu>
;; Description: Control access to portions of published planner pages
;; URL:
;; Compatibility: Emacs21

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This library lets you publish your planner pages while controlling
;; access to certain portions of them to users you specify.  When you
;; load this library, you gain access to two additional markup
;; directives to use in your planner pages.  The <authz> tag lets you
;; restrict access to arbitrary content as follows:

;;   Here is a sentence everyone should see.  This sentence also
;;   contains no sensitive data whatsoever.  <authz users="ajk">This
;;   sentence, however, talks about my predilection for that French
;;   vanilla instant coffee that comes in the little tin, and I'm
;;   embarrassed for anyone else to know about that.</authz> And
;;   here's some more perfectly innocuous content.

;; You can use <authz> tags to mark up entire paragraphs, tasks,
;; notes, and anything else.  The tags are replaced with Mason code

;; The #authz directive restricts access to an entire page.  A Mason
;; call is added to this page to generate a 403 error when someone not
;; listed tries to access it.  Any notes or tasks on a
;; #authz-protected page are also wrapped in Mason code on linked
;; pages.

;; * Startup

;; Add the following to your .emacs file to cause
;; M-x emacs-wiki-publish to automatically use planner-authz features.

;;   (require 'planner-authz)

;; * Customization

;; All user-serviceable options can be customized with
;; M-x customize-group RET planner-authz RET.

;; * Defaults

;; The following customization options let you set default access
;; lists for pages that don't have explicit settings:

;; planner-authz-project-default

;;   Default access list for project pages (not day pages).  If a
;;   given project page doesn't contain a #authz tag, it will receive
;;   the access list defined here.  If this variable is nil, all users
;;   will be allowed to view the page.  No corresponding variable is
;;   provided for day pages because it doesn't seem like you'd ever
;;   want to control access based on what day it was.  (But I will
;;   accept patches. :) Notes and tasks referencing pages without
;;   #authz tags will also be restricted to the users listed here.

;; planner-authz-day-note-default

;;   Default access list for notes on day pages not associated with
;;   any project.  There is way to set a default for notes on project
;;   pages for the reason above; they would only be associated with
;;   date pages anyway.

;; planner-authz-day-task-default

;;   Same as above but for tasks.

;;; Todo

;; - Make more specific tags override less specific ones, rather than
;;   more restrictive overriding less restrictive
;; - Support something other than Mason

;;; Code

;; $Id$

(require 'planner)

;; Customization options

;;; Code:
(defgroup planner-authz nil
  "A planner.el extension for restricting portions of your
published pages to specified users."
  :group 'planner
  :prefix "planner-authz")

(defcustom planner-authz-appt-alt nil
  "If non-nil, show `planner-appt' appointments to users not
authorized to see them, but replace the text of the appointment with
the contents of this variable.  If nil, don't show any part of an
appointment to an unauthorized user.

For example, if this variable is set to \"Private appointment\" and
some hypothetical user is not authorized for the SecretStuff page, an
appointment that was entered as

 #A1  _ @10:00 12:00 Secret meeting (SecretStuff)

would appear to our unauthorized user as

 #A1  _ @10:00 12:00 Private appointment"
  :group 'planner
  :type '(choice (string :tag "Replacement text")
                 (const :tag "Disable" nil)))

(defcustom planner-authz-appt-regexp
  (if (require 'planner-appt nil t)
      (concat "\\(?:[@!][ \t]*\\)?" planner-appt-time-regexp "\\(?:[ \t]+"
              planner-appt-time-regexp "\\)?[ \t]+"))
  "Regexp that matches a `planner-appt' start and end time specification.")

(defcustom planner-authz-mason-component-contents
  "<%once>
sub authz {
        my $r_user = $r ? $r->connection->user
                        : $ENV{REMOTE_USER} or return 0;
        foreach (@_) { return 1 if $r_user eq $_ }
        return 0;
}
</%once>
<%method content>
<%args>
$alt    => undef
@users
</%args>
% if (authz @users) {
<% $m->content %>\\
% } elsif ($alt) {
<% $alt %>\\
% }
</%method>
<%method page>
<%args>@users</%args>
<%perl>
unless (authz @users) {
        $m->clear_buffer;
        $m->abort(404);
}
</%perl>
</%method>
"
  "Mason code to be stored in a component.
The component's name is determined from
`planner-authz-mason-component-name'."
  :group 'planner-authz
  :type 'string)

(defcustom planner-authz-mason-component-name "authz.mas"
  "Name of Mason component that restricts content."
  :group 'planner-authz
  :type 'string)

(defcustom planner-authz-day-note-default nil
  "Default list of users for restricting non-project notes on day pages."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-day-task-default nil
  "Default list of users for restricting non-project tasks on day pages."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-project-default nil
  "Default list of users for restricting project pages if #authz is nil."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-sections-rule-list nil
  "List of sections and their access rule.

Each rule is a sublist of the form:

    (SECTION-NAME PREDICTION USER-LIST)

For sections matching SECTION-NAME, if the PREDICTION is t or a
function return t, that section will be accessable for users in
USER-LIST only.

The following example will make the \"Timeclock\" section and
\"Accomplishments\" section on day pages only accessable by user1 and
user2, while on plan pages obey the \"parent\" rule.

    ((\"Timeclock\" planner-authz-day-p
				       (\"user1\" \"user2\"))
    (\"Accomplishments\" planner-authz-day-p
				     (\"user1\" \"user2\")))"
  :group 'planner-authz
  :type '(repeat (regexp (choice boolean function))
		 (repeat string)))

(defcustom planner-authz-publishing-markup-first
  '(["^#authz\\s-+\\(.+\\)\n+" 0 ""]
    ["^\\([*]\\)+\\s-+\\(.+\\)" 0 planner-authz-markup-section]
    ["^#\\([A-C]\\)\\([0-9]*\\)\\s-*\\([_oX>CP]\\)\\s-*\\(.+\\)" 0
     planner-authz-markup-task]
    ["^\\.#\\([0-9]+\\)" 0 planner-authz-markup-note])
  "List of emacs-wiki publishing rules to apply under `planner-authz'.
This list of rules will be applied before any planner or
emacs-wiki rules."
  :group 'planner-authz
  :type '(repeat
          (vector :tag "Markup rule"
                  (choice regexp symbol)
                  integer
                  (choice string function symbol))))

(defcustom planner-authz-publishing-markup-last
  '(
    ;; Change : to | (the | would cause table markup).

    ["<&:" 0 "<&|"]

    ;; Move Masonry to outside of list item tags.

    ["\\(<li>\\)\\(<&[^&]*&>\\)\\(.*\\)\\(</&>\\)\\(\\s-*\\)\\(</li>\\)" 0
     "\\2\\1\\3\\5\\6\\4"]

    ;; Remove <p> tags from inserted Masonry.

    ["<p>\\s-*\\(</?&[^&]*>\\)" 0 "\\1"]
    ["\\(</?&[^&]*>\\)\\s-*</p>" 0 "\\1"]
    )
  "List of emacs-wiki publishing rules to apply under `planner-authz'.
This list of rules will be applied after any planner or
emacs-wiki rules."
  :group 'planner-authz
  :type '(repeat
          (vector :tag "Markup rule"
                  (choice regexp symbol)
                  integer
                  (choice string function symbol))))

;; Non-customizable variables

(defvar planner-authz-initialized-p nil
  "Non-nil if planner-authz has run its initialization code.")

(defvar planner-authz-pages nil
  "Alist of planner pages and users authorized to view them.
The list of users is separated by spaces.  This variable is
internal to planner-authz; do not set it manually.")
(defvar planner-authz-pages-to-republish nil
  "Queue of planner pages to republish when finished with current round.
Used to markup planner day pages that wouldn't ordinarily get
republished because they haven't explicitly changed.  This
variable is internal to planner-authz; do not set it manually.")

(defvar planner-authz-mason-page-tag
  "<& authz.mas:page, 'users', [qw(%s)] &>"
  "Mason component call inserted in published page to restrict entire page.
Don't be tempted to use the => Perl operator; the = could cause
emacs-wiki to insert <code> tags.")

(defvar planner-authz-mason-content-tag-begin
  "<&: authz.mas:content, 'users', [qw(%s)] &>"
  "Mason component call that restricts content to a list of users.
The %s is replaced by a space-separated list of these users.  If this
variable contains \"<&:\", it will be replaced by \"<&|\" at the end
of the publishing process to avoid being marked up as a table by
emacs-wiki.  Don't be tempted to use the => Perl operator; the = could
cause emacs-wiki to insert <code> tags.")
(defvar planner-authz-mason-content-tag-alt-begin
  "<&: authz.mas:content, 'users', [qw(%s)], 'alt', '%s' &>"
  "Mason component call that restricts content to a list of users and
shows alternate content to other users.  The first %s is replaced by a
space-separated list of these users.  The second %s is replaced by
alternate text to show to users who are not authorized.  If this
variable contains \"<&:\", it will be replaced by \"<&|\" at the end
of the publishing process to avoid being marked up as a table by
emacs-wiki.  Don't be tempted to use the => Perl operator; the = could
cause emacs-wiki to insert <code> tags.")
(defvar planner-authz-mason-content-tag-end "</&>"
  "Mason tag to insert at the end of content to be restricted.")

(defvar planner-authz-rule '("authz" t t nil planner-authz-tag)
  "Rule to add to `emacs-wiki-dangerous-tags' for <authz> tag support.")

(defvar planner-authz-version "$Revision$"
  "Version of of planner-authz.el.")

;; Cleanup function to run in buffer after markup

(setq emacs-wiki-after-markup-hook
      (delq 'planner-authz-after-markup emacs-wiki-after-markup-hook)
      planner-custom-variables
      (assq-delete-all
       'emacs-wiki-after-markup-hook planner-custom-variables))
(add-to-list
 'planner-custom-variables
 `(emacs-wiki-after-markup-hook
   . ,(cons 'planner-authz-after-markup emacs-wiki-after-markup-hook)))

;; Run after all files are published: republish files that still need
;; authz markup and generate Mason component.  If
;; emacs-wiki-publish-index appears, replace it with our own index
;; publisher.

(let ((match
       (memq 'emacs-wiki-publish-index
             emacs-wiki-after-wiki-publish-hook)))
  (if match
      (setcar match 'planner-authz-publish-index)))
(setq emacs-wiki-after-wiki-publish-hook
      (delq 'planner-authz-after-wiki-publish
            emacs-wiki-after-wiki-publish-hook)
      planner-custom-variables
      (assq-delete-all 'emacs-wiki-after-wiki-publish-hook
                       planner-custom-variables))
(add-to-list
 'planner-custom-variables
 `(emacs-wiki-after-wiki-publish-hook
   . ,(cons 'planner-authz-after-wiki-publish
            emacs-wiki-after-wiki-publish-hook)))

;; Add our <authz> tag

(setq emacs-wiki-dangerous-tags
      (delete planner-authz-rule
              emacs-wiki-dangerous-tags)
      planner-custom-variables
      (assq-delete-all 'emacs-wiki-dangerous-tags
                       planner-custom-variables))
(add-to-list
 'planner-custom-variables
 `(emacs-wiki-dangerous-tags
   . ,(cons planner-authz-rule emacs-wiki-dangerous-tags)))

;; Replace emacs-wiki-publish-function to hook the beginning of the
;; publishing process

(setq planner-custom-variables
      (assq-delete-all 'emacs-wiki-publish-function
                       planner-custom-variables))
(add-to-list
 'planner-custom-variables
 '(emacs-wiki-publish-function . planner-authz-publish-current))

;; Make all our tweaks take effect

(planner-option-customized 'planner-custom-variables planner-custom-variables)

;; Add our own initialization function to planner-mode-hook

(add-hook 'planner-mode-hook 'planner-authz-initialize t)

;;; Functions

(defun planner-authz-after-markup ()
  "Function to run in a buffer after it has been marked up.
Currently used to remove the page from the queue of pages to
republish and to enforce default access controls for project
pages."
  (let ((page (emacs-wiki-page-name)))
    (when page
      (delete page planner-authz-pages-to-republish)
      (let ((users (planner-authz-users)))
        (when users
          (goto-char (point-min))
          (insert (format planner-authz-mason-page-tag users) "\n"))))))

(defun planner-authz-after-wiki-publish ()
  "Function to run in buffer after all pages have been published.
Currently used to republish pages that reference restricted pages
and to generate Mason code."
  (emacs-wiki-publish-files planner-authz-pages-to-republish t)
  (planner-authz-generate-mason-component))

(defun planner-authz-day-p (&optional page)
  "Return non-nil if the current page or PAGE is a day page."
  (save-match-data
    (string-match planner-date-regexp
                  (or page (emacs-wiki-page-name)))))

(defun planner-authz-default (page)
  "Return a space-separated string of users associated with PAGE."
  (and planner-authz-project-default
       (not (planner-authz-day-p page)) ; not on day pages
       (mapconcat 'identity planner-authz-project-default " ")))

(defun planner-authz-file-alist (users)
  "Generate a list of planner files that USERS have access to."
  (let ((pages (planner-file-alist))
        result)
    (while pages
      (let (not-found-p)
        (with-temp-buffer
          (insert-file-contents-literally (cdar pages))
          (when (re-search-forward "^#authz\\s-+\\(.+\\)\n+" nil t)
            (let ((users-iter users)
                  (authz (split-string (match-string 1))))
              (while (and users-iter (not not-found-p))
                (unless (member (car users-iter) authz)
                  (setq not-found-p t))
                (setq users-iter (cdr users-iter)))))
          (unless not-found-p
            (setq result (append (list (car pages)) result))))
        (setq pages (cdr pages))))
    result))

(defun planner-authz-generate-mason-component ()
  "Generate the Mason component restricting content.
The component's name is taken from
`planner-authz-mason-component-name' and initialized with the
contents of `planner-authz-mason-component-contents'.  The
component restricts access to users specified by <authz> and
#authz tags."
  (with-temp-buffer
    (insert planner-authz-mason-component-contents)
    (let ((backup-inhibited t))
      (write-file
       (concat (file-name-as-directory planner-publishing-directory)
               planner-authz-mason-component-name)))))

;;;###autoload
(defun planner-authz-initialize ()
  "Function to be run from `planner-mode-hook'.  Useful for
tweaking emacs-wiki variables that planner overrides."

  (unless planner-authz-initialized-p   ; run this code only once

    ;; Add our markup rules, preserving those of planner and emacs-wiki

    (setq planner-custom-variables
          (assq-delete-all 'emacs-wiki-publishing-markup
                           planner-custom-variables))
    (add-to-list
     'planner-custom-variables
     `(emacs-wiki-publishing-markup
       . ,(append
           planner-authz-publishing-markup-first
           emacs-wiki-publishing-markup
           planner-authz-publishing-markup-last)))

    ;; Make our tweak take effect

    (planner-option-customized 'planner-custom-variables
                               planner-custom-variables))

  (setq planner-authz-initialized-p t))

(defun planner-authz-initial-directive-preprocess ()
  "Build `planner-authz-pages' from all pages containing #authz.
Pages that are subsequently published get removed from this list;
all others are forcibly republished."
  (setq planner-authz-pages nil
        planner-authz-pages-to-republish nil)
  (let ((pages (planner-file-alist)))
    (while pages
      (with-temp-buffer
        (insert-file-contents-literally (cdar pages))
        (while (re-search-forward "^#authz\\s-+\\(.+\\)\n+" nil t)
          (push `(,(caar pages) . ,(match-string 1)) planner-authz-pages)))
      (setq pages (cdr pages)))))

(defun planner-authz-markup-note ()
  "Restrict notes linked to a restricted page.
If this page is restricted and the note is linked to another
page, remember to republish that page later and restrict the note
as it appears there."
  (let* ((link (save-match-data
                 (planner-note-link (planner-current-note-info))))
         (linked-page (if link (emacs-wiki-wiki-base link)))
         (linked-users
          (if linked-page
              (planner-authz-users linked-page)
            (and planner-authz-day-note-default
                 (planner-authz-day-p)
                 (mapconcat 'identity
                            planner-authz-day-note-default " ")))))

    ;; "Inherit" from planner-markup-note, which inserts text rather
    ;; than returning a string

    (planner-markup-note)

    ;; If this note is linked to another page, republish that page
    ;; later to restrict the note as it appears there, providing that
    ;; page has an authz restriction

    (if linked-page
        (planner-authz-republish-page-maybe linked-page))

    ;; If the linked page has an authz restriction, restrict this note

    (when linked-users
      (goto-char (match-beginning 0))
      (planner-authz-surround-text
       linked-users
       (lambda ()
         (save-match-data
           (if (re-search-forward
                "^\\(?:\\.#[0-9][0-9]*\\|\\*\\*?\\)\\s-+" nil 0 2)
               (goto-char (match-beginning 0)))))
       t))
    nil))

(defun planner-authz-markup-task ()
  "Restrict tasks linked to restricted pages.
If this page is restricted and the task is linked to another
page, remember to republish that page later and restrict the task
as it appears there."
  (let* ((begin (match-end 0))
         (link (save-match-data
                 (planner-task-link (planner-current-task-info))))
         (linked-page (if link (emacs-wiki-wiki-base link)))
         (linked-users
          (if linked-page
              (planner-authz-users linked-page)
            (and planner-authz-day-task-default
                 (planner-authz-day-p)
                 (mapconcat 'identity
                            planner-authz-day-task-default " ")))))

    ;; "Inherit" from planner-markup-task, which inserts text rather
    ;; than returning a string

    (planner-markup-task)

    ;; If this task is linked to another page, republish that page
    ;; later to restrict the task as it appears there, providing that
    ;; page has an authz restriction

    (if linked-page
        (planner-authz-republish-page-maybe linked-page))

    ;; If the linked page has an authz restriction, restrict this task

    (when linked-users
      (let* ((end (point))
             (start-tag
              (if (and planner-authz-appt-alt
                       planner-authz-appt-regexp
                       (save-match-data
                         (goto-char begin)
                         (re-search-forward planner-authz-appt-regexp end t)))
                  (format planner-authz-mason-content-tag-alt-begin
                          linked-users planner-authz-appt-alt)
                (goto-char (+ 2 begin)) ; skip past "- "
                (format planner-authz-mason-content-tag-begin linked-users))))
        (emacs-wiki-surround-text
         start-tag
         planner-authz-mason-content-tag-end
         (lambda () (goto-char (+ end (length start-tag)))))))
    ""))

(defun planner-authz-markup-section-predict (rule)
  "Check if the prediction is satisfied."
  (let ((predict (elt rule 1)))
    (if (functionp predict)
	(funcall predict)
      predict)))

(defun planner-authz-markup-section ()
  "Restrict section according to `planner-authz-sections-rule-list'."
  (let ((begin (planner-line-beginning-position))
	(rule-list planner-authz-sections-rule-list)
	section-name
	section-level
	next-section-regexp)
    (goto-char begin)
    (save-match-data
      (re-search-forward "^\\([*]\\)+\\s-+\\(.+\\)" nil t)
      (setq section-level (length (match-string 1)))
      (setq section-name (match-string 2)))
    (let ((rule (catch 'done
                  (while rule-list
                    (if (string-match (caar rule-list) section-name)
                        (throw 'done (car rule-list))
                      (setq rule-list (cdr rule-list))))
                  nil)))
      (if (and rule
	       (planner-authz-markup-section-predict rule))
	  (progn
	    (goto-char begin)
	    (planner-authz-surround-text
	     (mapconcat 'identity (elt rule 2) " ")
	     (lambda ()
	       (save-match-data
		 (let ((found nil))
		   (re-search-forward "^\\([*]\\)+\\s-+\\(.+\\)" nil t)
		   (while (and (not found)
			       (re-search-forward
				"^\\([*]\\)+\\s-+\\(.+\\)" nil t))
		     (if (<= (length (match-string 1))
			     section-level)
			 (setq found t)))
		   (if found
		     (goto-char (planner-line-beginning-position))
		   (goto-char (point-max))))))
	     t))))))
  
(defun planner-authz-publish-current (file output-path)
  "Preprocess #authz directives and then publish FILE to OUTPUT-PATH."
  (planner-authz-initial-directive-preprocess)
  (emacs-wiki-publish-current file output-path))

(defun planner-authz-publish-index ()
  "Publish an index for the planner marked up with Mason code.
Only those links to pages which the remote user is authorized to
access will be shown."
  (interactive)
  (with-current-buffer (emacs-wiki-generate-index t t t)
    (message "Marking up index...")
    (goto-char (point-min))
    (while (re-search-forward "^- \\(.*\\)" nil t)
      (let* ((match (emacs-wiki-wiki-base (match-string 1)))
             (users (if match (planner-authz-users match))))
        (if users
            (replace-match
             (concat
              (format planner-authz-mason-content-tag-begin users)
              match planner-authz-mason-content-tag-end)
             t nil nil 1))))
    (emacs-wiki-replace-markup emacs-wiki-index-page)
    (let ((backup-inhibited t))
      (write-file (emacs-wiki-published-file emacs-wiki-index-page)))
    (kill-buffer (current-buffer))))

(defun planner-authz-republish-page-maybe (linked-page)
  "Remember LINKED-PAGE to be republished later.
The page will be republished if and only if the current page is
restricted."
  (if (planner-authz-users)
      (add-to-list 'planner-authz-pages-to-republish
                   (emacs-wiki-page-file linked-page))))

(defun planner-authz-surround-text (users move-func &optional block-p)
  "Insert Mason code for <authz> and #authz.
Restrict text to USERS between point and the position moved to by
MOVE-FUNC.  If BLOCK-P is non-nil, set it off as a paragraph,
else insert it inline."
  (unless users (setq users ""))
  (emacs-wiki-surround-text
   (concat
    (format planner-authz-mason-content-tag-begin users)
    (if block-p "\n" ""))
   (concat
    planner-authz-mason-content-tag-end
    (if block-p "\n" ""))
   move-func))

(defun planner-authz-tag (beg end attrs)
  "Insert Mason code to restrict access to users specified in <authz>.
The region from BEG to END is protected.  ATTRS should be an alist
of tag attributes including \"users\"."
  (let ((users (cdr (assoc "users" attrs))))
    (planner-authz-surround-text users (lambda () (goto-char end)))))

(defun planner-authz-users (&optional page)
  "Return a list of acceptable users for PAGE.
The list of users is returned as space-separated string, based on
a #authz directive appearing in the page.  If PAGE contains no
#authz directive and is a project page (it doesn't match
`planner-date-regexp'), return `planner-authz-project-default' as
a space-separated string.

If PAGE is nil, return a list of users associated with the
current page."
  (unless page (setq page (emacs-wiki-page-name)))
  (or (cdr (assoc page planner-authz-pages))
      (planner-authz-default page)))

(provide 'planner-authz)

;;; planner-authz.el ends here
