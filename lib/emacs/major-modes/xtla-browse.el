;;; xtla-browse.el --- Arch archives/library browser

;; Copyright (C) 2004 by Stefan Reichoer

;; Author: Masatake YAMATO <jet@gyve.org>

;; This is a part of xtla.
;; xtla is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xtla is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 1. Load xtla-browse.el
;; 2. Do M-x xtla-browse.el

;;; Code:

(require 'tree-widget)
(require 'xtla)

;;
;; Internal buffer local variables
;;
(defvar tla-browse-archive-widget-list nil)
(defvar tla-browse-archive-root-widget nil)

;;
;; My-id
;;
(defun tla-browse-my-id-widget-new (my-id)
  `(push-button
    :notify tla-browse-my-id-widget-set
    :format "My-id: %[%v%]\n"
    :help-echo "Click here to change my-id"
    ,my-id))

(defun tla-browse-my-id-widget-set (self changed event)
  (let ((new-id (tla-my-id t)))
    (widget-value-set changed new-id)
    (widget-setup)))

;;
;; Root node widget for archives and libraries
;;
(define-widget 'tla-browse-root-widget 'tree-widget
  "Widget for a root node."
  :tla-type 'archives-root
  :open t
  :has-children t)

(defun tla-browse-root-widget-new (label expander)
  `(tla-browse-root-widget
    :dynargs ,expander
    :tag ,label))

(defun tla-browse-archives-root-widget-expand (root)
  (or (widget-get root :args)
      (let ((archives (progn
			(unless tla-archive-tree
			  (tla-archives-build-archive-tree))
			tla-archive-tree))
	    (default-archive (tla-my-default-archive)))
	(setq tla-browse-archive-widget-list
	      (mapcar
	       (lambda (archive)
		 (tla-browse-archive-widget-new (car archive)
						(cadr archive)
						(equal
						 default-archive
						 (car archive))
						(equal
						 default-archive
						 (car archive))))
	       archives)))))

;;
;; Archive
;;
(define-widget 'tla-browse-archive-widget 'tree-widget
  "Widget for an archive."
  :open nil
  :dynargs 'xtla-browse-archive-widget-expand
  :has-children t)

(defun tla-browse-archive-widget-new (archive location defaultp &optional open-p)
  `(tla-browse-archive-widget
    :open ,open-p
    :tla-type archive
    :tla-archive ,archive
    :tla-archive-location ,location
    :tla-defaultp ,defaultp
    :get-default-widget (lambda (widget) (nth 3 (widget-get widget :node)))
    :keep (:tla-type :tla-archive :tla-archive-location :tla-defaultp)
    :node (group
	   :value ,(list defaultp archive location)
	   (toggle :on "<A>"
		   :off " a "
		   :format "%[%v%]"
		   ;; TODO
		   ;; :action tla-browse-archive-widget-action
		   :help-echo (lambda (w)
				(if (widget-value w)
				    "Default Archive"
				  "Archive. Click here to make this default archive."))
		   :value ,defaultp)
	   (link :button-prefix ""
		 :button-suffix ""
		 :button-face default
		 :format " %[%v%] => "
		 :keymap ,(let ((map (copy-keymap widget-keymap)))
			    (define-key map "a" '(lambda () (interactive) 
						   (message "TEST: HERE I AM %s"
							    (widget-get (widget-at (point))
									:tla-archive))
						   ))
			    map)
		 ,archive)
	   (item ,location)
	   )))

;; This function is still broken.
(defun tla-browse-archive-widget-action (widget &optional event)
  (unless (widget-value widget)
    ;;
    (mapcar (lambda (other)
	      (let ((rbutton (widget-apply other :get-default-widget)))
		;;(widget-put (widget-apply other :get-default-widget) :value nil)
		(widget-value-set rbutton nil)
		(widget-apply widget :notify widget event)))
	    tla-browse-archive-widget-list)
    ;;
    (widget-value-set widget t))
  (widget-apply widget :notify widget event)
  (run-hook-with-args 'widget-edit-functions widget))

(defun xtla-browse-archive-widget-expand (archive)
  (or (widget-get archive :args)
      (let* ((archive-name (widget-get archive :tla-archive))
	     (categories (cddr (tla-archive-tree-get-archive archive-name)))
	     (open-p nil ;(widget-get archive :tla-defaultp)
		     ))
	(unless categories
	  (tla-categories-build-archive-tree archive-name)
	  (setq categories
		(cddr (tla-archive-tree-get-archive archive-name))))
	(mapcar
	 (lambda (category)
	   (tla-browse-category-widget-new archive-name
					   (car category)
					   open-p))
	 categories))))

;;
;; Abstract widget for category, branch
;;
(define-widget 'tla-browse-element-widget 'tree-widget
  "Abstract widget for category, branch and version."
  :open nil
  ;; ---
  :dynargs 'tla-browse-element-widget-expand
  :keep '(:tla-type :tla-archive :tla-category :tla-branch :tla-version)
  :node '(link :value "a")
  :value "a")



(defun tla-browse-element-widget-new (type
				      prefix
				      archive
				      &optional
				      category
				      branch)
  `(tla-browse-element-widget
    :has-children t
    :tla-type ,type
    :tla-archive ,archive
    :tla-category ,category
    :tla-branch ,branch
    :tla-version nil
    :node (group
	   :value ,(list prefix (cond
				 (branch branch)
				 (category category)))
	   (link :button-prefix ""
		 :button-suffix ""
		 :button-face bold
		 :format " %[%v%]"
		 ,prefix)
	   (link :button-prefix ""
		 :button-suffix ""
		 :button-face default
		 :format " %[%v%]\n"
		 ,(cond
		   (branch branch)
		   (category category))))))

(defun tla-browse-element-widget-expand (widget)
  (or (widget-get widget :args)
      (let* ((archive (widget-get widget  :tla-archive))
	     (category (widget-get widget :tla-category))
	     (branch (widget-get widget   :tla-branch))
	     args get-func build-func widget-func
	     elements open-p)
	(cond
	 (branch
	  (setq args (list archive category branch)
		get-func 'tla-archive-tree-get-branch
		build-func 'tla-versions-build-archive-tree
		widget-func 'tla-browse-version-widget-new))
	 (category
	  (setq args (list archive category)
		get-func 'tla-archive-tree-get-category
		build-func 'tla-branches-build-archive-tree
		widget-func 'tla-browse-branch-widget-new)))
	(setq elements (cdr (apply get-func args)))
	(unless elements
	  (apply build-func args)
	  (setq elements (cdr (apply get-func args))))
	(mapcar
	 (lambda (elt)
	   (apply widget-func
		  (reverse (cons (car elt) (reverse args)))))
	 elements))))



;;
;; Category
;;
(defun tla-browse-category-widget-new (archive category &optional open-p)
  (let ((widget (tla-browse-element-widget-new
		 'category "c" archive category)))
    (widget-put widget :open open-p)
    widget))

;;
;; Branch
;;
(defun tla-browse-branch-widget-new (archive category branch &optional open-p)
  (let ((widget (tla-browse-element-widget-new
		 'branch "b" archive category branch)))
    (widget-put widget :open open-p)
    widget))

;;
;; Version
;;
(defun tla-browse-version-widget-new (archive category branch version &optional open-p)
  `(group
    :tla-type version
    :tla-archive ,archive
    :tla-category ,category
    :tla-branch ,branch
    :tla-version ,version
    :value ,(list "v" version)
    (link :button-prefix ""
	  :button-suffix ""
	  :button-face bold
	  :format "%[ %v%]"
	  :action tla-browse-version-widget-show-revisions
	  "v")
    (link :button-prefix ""
	  :button-suffix ""
	  :button-face default
	  :format "%[ %v%]\n"
	  :action tla-browse-version-widget-show-revisions
	  ,version)))

(defun tla-browse-version-widget-show-revisions (widget &optional event)
  "This function must return nil."
  (setq widget (widget-get widget :parent))
  (save-excursion
    (tla-revisions
     (widget-get widget :tla-archive)
     (widget-get widget :tla-category)
     (widget-get widget :tla-branch)
     (widget-get widget :tla-version))))

;;
;; Revision
;;

;;
;; Revision-library
;;
(defun tla-browse-my-revision-library-new (my-revision-library)
  `(push-button
    :notify tla-browse-my-revision-library-set
    :format "Library Location: %[%v%]\n"
    :help-echo "Click here to change my-revision-library"
    ,my-revision-library))

(defun tla-browse-my-revision-library-set (self changed event)
  (let ((new-rev-lib (tla-my-revision-library t)))
    (widget-value-set changed new-rev-lib)
    (widget-setup)))


;;
;; Browser main
;;
;;;###autoload
(defun tla-browse ()
  (interactive)
  (switch-to-buffer (format "*tla-browse*"))

  (kill-all-local-variables)
  (set (make-local-variable 'tla-browse-archive-widget-list) nil)
  (set (make-local-variable 'tla-browse-archive-root-widget) nil)

  (let ((inhibit-read-only t))
    (erase-buffer))
  ;; remove-overlays is not portable enough.
  (mapc #'delete-overlay (overlays-in
			  (point-min) (point-max)))

  (let ((rev-lib (tla-my-revision-library)))
    (widget-create
     (tla-browse-my-id-widget-new (tla-my-id)))
    (setq tla-browse-archive-root-widget
	  (widget-create
	   (tla-browse-root-widget-new
	    "Archives"
	    'tla-browse-archives-root-widget-expand)))
    (widget-insert "\n")

    (when (and rev-lib (not (string= "" rev-lib )))
      (widget-create
       (tla-browse-my-revision-library-new rev-lib))
      (widget-create
	   (tla-browse-root-widget-new
	    "Revision Library"
	    nil))
      ))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(provide 'xtla-browse)

;; Local Variables:
;; arch-tag: 5e947e90-82df-4f49-9325-719a3f27732e
;; End:
