;;; emacs-wiki-pgg.el --- PGG interface for Emacs Wiki

;; Copyright (C) 2004 Anirudh Sasikumar

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-pgg.el
;; Keywords: hypermedia encryption
;; Author: Anirudh Sasikumar (anirudhsasikumar AT gmail DOT com)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain sections of encrypted text via PGG in emacs-wiki
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

;;;_* Commentary

;; This file is the part of the Emacs Wiki project that allows you to
;; interface with PGG and maintain sections of encrypted/unencrypted text
;; in Emacs Wiki mode.

;;;_ + Startup

;; To support PGG encryption within Emacs Wiki, you will need to put this in
;; your .emacs file:
;; 
;;   (require 'emacs-wiki-pgg)
;;
;; Make sure to set  pgg-gpg-user-id  to your user id. Eg:
;;
;; (setq pgg-gpg-user-id "Your user id")

;;;_ + Usage

;; There are two interfaces to PGG.
;;
;; gpg tag
;;
;;  Enclose text that you want to encrypt/decrypt interactively in
;;  Emacs Wiki mode using M-x emacs-wiki-encrypt-gpg and
;;  M-x emacs-wiki-decrypt-gpg bound to C-c C-S-e and C-c C-S-d
;;  respectively.
;;
;; Calling both of these functions interactively with a C-u prefix
;; encrypts/decrypts the entire buffer. But C-u M-x
;; emacs-wiki-encrypt-gpg renders the wiki file unpublishable.
;;
;; gpge tag
;;
;;  Enclose text that you want unencrypted within the Wiki source file
;;  and encrypted in the published html file.
;;
;; The text within both gpg tags are preformatted via the pre tag.

;;;_ + Contributors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki PGG Interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emacs-wiki)

(require 'pgg)
(require 'pgg-gpg)

;;; Customizable options

(defgroup emacs-wiki-pgg nil
  "Options controlling the behavior of Emacs Wiki PGG Interface."
  :group 'emacs-wiki)

(defcustom emacs-wiki-pgg-interface 'pgg-encrypt-show
  "Interfacing function to invoke pgg with.

`pgg-encrypt-show' asks for recipients via the minibuffer.

`pgg-encrypt-sign-self' encrypts with recipient as self and signs
if emacs-wiki-pgg-sign is non-nil."
  :type 'function
  :group 'emacs-wiki-pgg)

(defcustom emacs-wiki-pgg-sign nil
  "Signs the message after encrypting if emacs-wiki-pgg-interface
is `pgg-encrypt-sign-self'."
  :type 'boolean
  :group 'emacs-wiki-pgg)

;;; PGG Functions
(defun pgg-encrypt-sign-self (&optional start end)
  "Encrypt the current buffer for self.
Signs the message if `emacs-wiki-pgg-sign' is non-nil.

If optional arguments START and END are specified, only encrypt
within the region."
  (interactive "*")
  (let* ((rcpts (list pgg-gpg-user-id))
         (start (or start (point-min)))
         (end (or end (point-max)))
         (status (pgg-encrypt-region start end rcpts
                                     emacs-wiki-pgg-sign)))
      (pgg-display-output-buffer start end status)
    status))


(defun pgg-encrypt-show (&optional start end)
  "Encrypt and sign the current buffer for self.
If optional arguments START and END are specified, only encrypt
within the region."
  (interactive "*")
  (let* ((start (or start (point-min)))
         (end (or end (point-max))))
    (push-mark start t t)
    (goto-char end)
    (call-interactively 'pgg-encrypt-region)
    ))

(defun pgg-decrypt-show (&optional start end)
  "Decrypt the current buffer.
If optional arguments START and END are specified, only decrypt
within the region."
  (interactive "*")
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (status (pgg-decrypt-region start end)))
      (pgg-display-output-buffer start end status)
    status))

;;; Interactive Emacs Wiki mode Functions

(defun emacs-wiki-encrypt-gpg (&optional entirewiki)
  "Find all gpg tags and encrypt their contents.
When called with \\[universal-argument], encrypt entire buffer."
  (interactive "P")
  (if entirewiki
      (funcall emacs-wiki-pgg-interface)
  (let (tagbegin tagend)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "<gpg>" nil t)
        (setq tagbegin (match-end 0))
        (re-search-forward "</gpg>" nil t)
        (setq tagend (match-beginning 0))
        (funcall emacs-wiki-pgg-interface tagbegin tagend))))))

(defun emacs-wiki-decrypt-gpg (&optional entirewiki)
  "Find all gpg tags and decrypt their contents.
When called with \\[universal-argument], decrypt entire buffer."
  (interactive "P")
  (if entirewiki
      (pgg-decrypt-show)
  (let (tagbegin tagend)
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "<gpg>" nil t)
        (setq tagbegin (match-end 0))
        (if (re-search-forward "</gpg>" nil t)
            (progn
              (setq tagend (match-beginning 0))
              (pgg-decrypt-show tagbegin tagend))
          (message "No closing gpg tags found")))))))

;;; Key Bindings

(define-key emacs-wiki-mode-map
  [(control ?c) (control ?E)]
  'emacs-wiki-encrypt-gpg)

(define-key emacs-wiki-mode-map
  [(control ?c) (control ?D)]
  'emacs-wiki-decrypt-gpg)

;;; Tag handler Functions

;; gpg tag: For interactive encryption/decryption

(defun emacs-wiki-gpg-tag (beg end highlight-p)
  "Enables interactive encryption/decryption via PGG in emacs-wiki.
Whitespace is preserved."
  (if highlight-p
      (progn
        (emacs-wiki-multiline-maybe beg end)
        (goto-char end))
    (insert "<pre class=\"example\">")
    (when (< (point) end)
      (goto-char end))
    (insert "</pre>")
    (add-text-properties beg end
                         '(rear-nonsticky
                           (read-only)
                           read-only t))))

(push '("gpg" t nil t emacs-wiki-gpg-tag) emacs-wiki-markup-tags)

;; gpge tag: For publishing encrypted text from unencrypted source

(defun emacs-wiki-gpg-encrypt-tag (beg end highlight-p)
  "Automatically encrypts text via PGG on publish.
Whitespace is preserved."
  (if highlight-p
      (progn
        (emacs-wiki-multiline-maybe beg end)
        (goto-char end))
    (insert "<pre class=\"example\">")
    (funcall emacs-wiki-pgg-interface (point) end)
    (when (< (point) end)
      (goto-char end))
    (insert "</pre>")
    (add-text-properties beg (point)
                         '(rear-nonsticky
                           (read-only)
                           read-only t))))

(push '("gpge" t nil t emacs-wiki-gpg-encrypt-tag)
      emacs-wiki-markup-tags)

(provide 'emacs-wiki-pgg)

;;; emacs-wiki-pgg.el ends here
