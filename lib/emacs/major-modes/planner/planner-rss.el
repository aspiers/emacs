;;; planner-rss.el --- RSS export for the Emacs Planner (planner.el)

;; Copyright (C) 2003 Sandra Jean Chua <sacha@free.net.ph>

;; Emacs Lisp Archive Entry
;; Filename: planner-rss.el
;; Version: 2005.08.20-17.59-stable
;; Keywords: hypermedia
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Export planner entries as an RSS feed
;; URL: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/planner-rss.el
;; ChangeLog: http://sacha.free.net.ph/notebook/emacs/emacs-wiki/ChangeLog
;; Compatibility: Emacs20, Emacs21

;;; Commentary:
;;
;; If you use `remember-to-planner' to keep daily notes, you can
;; automatically publish remembered notes as an RSS feed by adding the
;; following code to your .emacs:
;;
;;   (add-to-list 'remember-planner-append-hook 'planner-rss-add-note t)
;;
;; You can also invoke `planner-rss-add-note' on any note you would like added.

;;; Code:

(require 'emacs-wiki)
(require 'planner)

(defgroup planner-rss nil
  "Planner options for RSS feeds."
  :prefix "planner-rss-"
  :group 'planner)

(defcustom planner-rss-base-url
  ""
  "Base URL for blog entries. Should include trailing /.
Example: http://sacha.free.net.ph/notebook/wiki/"
  :type 'string
  :group 'planner-rss)

;; On my system, this is set to
;;'(("."
;;   "/home/sacha/public_html/notebook/wiki/blog.rdf"
;;   "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel>
;;<title>sachachua's blog</title>
;;<link>http://sacha.free.net.ph/notebook/wiki/today.php</link>
;;<description>Random notes</description>
;;</channel></rss>\n"))
(defcustom planner-rss-category-feeds
  nil
  "List of (CONDITION FILENAME INITIAL-CONTENTS).

If CONDITION is a regexp, all entries that match the regexp in
either title or body will be included in FILENAME. If CONDITION
is a function with one argument, it will be called with the
marked-up text, and a non-nil return value means include this
entry in FILENAME.

If INITIAL-CONTENTS is non-nil, it is used to initialize the file if
the file is not found or is corrupted.

Example:
'((\".\"
   \"/home/sacha/public_html/notebook/wiki/blog.rdf\"
   \"<?xml version=\\\"1.0\\\"?><rss version=\\\"2.0\\\"><channel>
<title>sachachua's blog</title>
<link>http://sacha.free.net.ph/notebook/wiki/today.php</link>
<description>Random notes</description>
</channel></rss>\n\"))"
  :type '(repeat (group (choice regexp function) file string))
  :group 'planner-rss)

(defcustom planner-rss-feed-limits nil
  "A list of (REGEX SIZE-LIMIT ITEM-LIMIT).

REGEX is a regular expression that matches the filename.
SIZE-LIMIT, if non-nil, is the upper limit in characters.
ITEM-LIMIT, if non-nil, is the upper limit in items. If the feed
exceeds the stated limits, older items are deleted."
  :type '(alist :key-type regexp
                :value-type (group (choice
                                    :tag "Characters: "
                                    (const :tag "No limit" nil)
                                    (integer))
                              (choice
                               :tag "Size: "
                               (const :tag "No limit" nil)
                               (integer))))
  :group 'planner-rss)

;; Determined from planner-rss-category-feeds.
;; You don't need to set this.
(defvar planner-rss-file-name nil "Filename of current RSS feed.")
(defvar planner-rss-initial-contents nil "Initial contents.")

(defun planner-rss-add-item (&optional title link description
                                       pubdate categories)
  "Add an item to the top of the items list in `planner-rss-file-name'.
It will have TITLE, LINK, DESCRIPTION, PUBDATE and CATEGORIES.
`planner-rss-initialize' is called if necessary."
  (save-excursion
    (save-window-excursion
      (find-file planner-rss-file-name)
      (goto-char (point-min))
      (unless (re-search-forward "<item>\\|</channel>" nil t)
        (erase-buffer)
        (insert planner-rss-initial-contents)
        (re-search-backward "</channel>"))
      (goto-char (match-beginning 0))
      (insert "<item>")
      (when title (insert "<title>" title "</title>"))
      (when link (insert "<link>" link "</link>"))
      (when description (insert "<description>" description "</description>"))
      (when pubdate (insert "<pubDate>" pubdate "</pubDate>"))
      (when categories
        (insert (mapconcat (lambda (item) (concat "<category>" item "</category>"))
                           categories
                           "\n")))
      (insert "</item>")
      (planner-rss-limit)
      (save-buffer))))

(defun planner-rss-strip-tags (string)
  "Remove all tags from STRING."
  (planner-replace-regexp-in-string "<[^>]+>" "" string))

;;;###autoload
(defun planner-rss-add-note (&optional feed)
  "Export the current note using `planner-rss-add-item'.
If FEED is non-nil, add the note to the specified feed only.
Call with the interactive prefix in order to be prompted for FEED."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Feed: "))))
  (save-window-excursion
    (save-excursion
      (save-restriction
        (when (planner-narrow-to-note)
          (let ((info (planner-current-note-info t))
                (page (emacs-wiki-published-name (emacs-wiki-page-name)))
                (text (buffer-substring-no-properties (point-min) (point-max)))
                description title pubdate link categories)
            ;; Get the title
            (with-temp-buffer
              (insert (planner-format-note info "" nil nil nil ""))
              (goto-char (point-min))
              (let ((emacs-wiki-project planner-project))
                (emacs-wiki-change-project planner-project))
              (let ((emacs-wiki-publishing-header "")
                    (emacs-wiki-publishing-footer ""))
                (mapcar
                 (lambda (item)
                   (setq emacs-wiki-publishing-markup
                         (remove item
                                 emacs-wiki-publishing-markup)))
                 (list
                  '["\\`\n*" 0 "<p>\n"]
                  '["\\([^> \t\n]\\)\\s-*\\'" 0 "\\1\n</p>"]
                  '["\n\\([[:blank:]]*\n\\)+" 0 "\n\n</p>\n\n<p>\n"]
                  '["\\([^>[:space:]]\\)\\s-*\\'" 0 "\\1\n</p>"]))
                (emacs-wiki-replace-markup)
                (setq title
                      (planner-rss-strip-tags
                       (buffer-substring-no-properties (point-min)
                                                       (point-max))))))
            (with-temp-buffer
              (insert (or title ""))
              (goto-char (point-min))
              (emacs-wiki-escape-html-specials)
              (setq title (buffer-string)))
            ;; Figure out the timestamp
            ;; FIXME: Use the real time
            (setq pubdate (format-time-string "%a, %d %b %Y %H:%M:00 %z"))
            ;; Figure out the permalink
            (setq link (concat planner-rss-base-url page "#" (elt info 1)))
            ;; Figure out the category
            (when (planner-note-plan info)
              (setq categories
                    (if (and (featurep 'planner-multi)
                             (string-match (regexp-quote planner-multi-separator)
                                           (planner-note-link-text info)))
                        (mapcar 'emacs-wiki-wiki-base (planner-multi-note-link-as-list info))
                      (list (emacs-wiki-wiki-base (planner-note-plan info))))))
            ;; Get the description
            (with-temp-buffer
              (insert (or (elt info 5) ""))
              (goto-char (point-min))
              (emacs-wiki-change-project planner-project)
              (let ((emacs-wiki-publishing-header "")
                    (emacs-wiki-publishing-footer ""))
                (emacs-wiki-replace-markup)
                (setq description (buffer-substring-no-properties
                                   (point-min)
                                   (point-max)))))
            (with-temp-buffer
              (insert (or description ""))
              (goto-char (point-min))
              (emacs-wiki-escape-html-specials)
              (setq description (buffer-string)))
            (if feed
                (let ((planner-rss-file-name feed))
                  (planner-rss-add-item title link description pubdate
                                        categories))
              (let (seen)
                (mapcar (lambda (entry)
                          (let ((condition (elt entry 0))
                                (planner-rss-file-name (elt entry 1))
                                (planner-rss-initial-contents (elt entry 2)))
                            (when (cond
                                   ((stringp condition)
                                    (string-match condition text))
                                   ((functionp condition)
                                    (funcall condition text))
                                   (t condition))
                              (unless (member planner-rss-file-name seen)
                                (add-to-list 'seen planner-rss-file-name)
                                (planner-rss-add-item
                                 title link description pubdate
                                 categories)))))
                        planner-rss-category-feeds)))))))))

(defun planner-rss-limit ()
  "Apply limits specified in `planner-rss-feed-limits'."
  (let ((filename (expand-file-name (planner-current-file))))
    (mapcar
     (lambda (item)
       (when (string-match (elt item 0) filename)
         (planner-rss-limit-size (elt item 1))
         (planner-rss-limit-items (elt item 2))))
     planner-rss-feed-limits)))

(defun planner-rss-limit-size (limit)
  "Delete RSS items that cause this file to go over LIMIT characters."
  (when limit
    (widen)
    (goto-char limit)
    (unless (eobp)
      (re-search-backward "<item>" nil t)
      (let ((start (match-beginning 0)))
        (re-search-forward "</channel>" nil t)
        (delete-region start (match-beginning 0))))))

(defun planner-rss-limit-items (limit)
  "Delete RSS items past the LIMIT-th item."
  (when limit
    (widen)
    (goto-char (point-min))
    (while (and (> limit -1) (re-search-forward "<item>" nil t))
      (setq limit (1- limit)))
    (when (= limit -1)
      (let ((start (match-beginning 0)))
        (re-search-forward "</channel>" nil t)
        (delete-region start (match-beginning 0))))))

(provide 'planner-rss)

;;; planner-rss.el ends here
