;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     align.el -- emacs customization file     ;
;   Copyright (C) 1992-1998 Michael Marucheck  ;
;             maruchck@blarg.net               ;
;                                              ;
; Permission to use, copy, and distribute this ;
; software is given, provided that this header ;
; is included intact, and provided that you do ;
; so in accordance with the GNU GENERAL PUBLIC ;
; LICENSE.  Violators shall all be prosecuted! ;
;       (-:  Have a nice day.  :-)             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mjm-align-skip-indent (skip-char)
  "Goes to first non-space char on line"
  (beginning-of-line)
  (while (or (= (following-char) ? )
             (= (following-char) ?\t))
    (forward-char 1))

  (if skip-char
      (while (and (not (eolp))
                  (/= (following-char) skip-char))
        (forward-char 1)))
  )

(defun mjm-align-offset-list (search-char eol-sep)
  "Get list of spans between search-char on a line"
  (beginning-of-line)
  (let ((last-point (point))
        (offset-list nil))

    ;; skip indentation
    (mjm-align-skip-indent nil)

    (while (not (eolp))
      (if (= (following-char) search-char)
          (progn
            (forward-char 1)
            (if (= (following-char) ? )
                (forward-char 1))
            (setq offset-list (cons (- (point) last-point) offset-list))
            (setq last-point (point))
            )
        (forward-char 1)
        )
      )

    (if (and eol-sep (/= (preceding-char) search-char))
        (setq offset-list (cons (- (point) last-point) offset-list))
      )
    (nreverse offset-list)
    ))

(defun mjm-max-two-lists (left right)
  "Get the max of two offset lists, element by element"
  (cond
   ((and left right) (cons
                      (max (car left) (car right))
                      (mjm-max-two-lists (cdr left) (cdr right))))
   (left)
   (right)))

(defun mjm-align-by-list (offset-list search-char align-at-char)
  "Indent a line according to offset-list, aligning by search-char.
If align-at-char is non-nil, the separators will be aligned.  Otherwise
the text will be aligned and the separators will follow the end of the
span of text."
  (mjm-align-skip-indent nil)
  (let ((offset 0))
    (while offset-list
      (setq offset (+ offset (car offset-list)))
      ;; (message "current offset is %d" offset) (sit-for 1)
      (setq offset-list (cdr offset-list))
      (while (and (not (eolp))
                  (/= (following-char) search-char))
        ;; (message "searching for [%c]" search-char) (sit-for 1)
        (forward-char 1))

      (if (= (following-char) search-char)
          (if align-at-char
              ;; If we are aligning the search-char too, subtract one extra
              ;; column for the char itself, and perhaps one more for an
              ;; extra space that we added in the offset calculations.
              (let ((extra 1))
                (forward-char 1)
                (if (= (following-char) ? )
                    (setq extra (+ extra 1))
                  )
                (forward-char -1)
                (or (eolp)
                    (indent-to (- offset extra))
                    )
                (forward-char 1))
            ;; If we are not aligning the search-char, we skip past the
            ;; search-char and possibly past one space before indenting
            ;; the text beyond
            (forward-char 1)
            (if (= (following-char) ? )
                (forward-char 1))
            (if (not (eolp))
                (indent-to offset))))
      )))
            
(defun mjm-align-one-space (skip-func skip-arg)
  "Convert all spaces and tabs outside of indentation into a single space."
  (interactive)
  (goto-char (point-min))

  (while (< (point) (point-max))

    ;; move past newline from previous iteration
    (forward-char 1)

    ;; skip indentation, if any
    (apply skip-func skip-arg nil)

    ;; process rest of line, if any
    (while (and (not (eolp))
                (< (point) (point-max))
                )
      ;; just move forward if we are not just after a tab or space
      (if (and (/= (preceding-char) ? )
               (/= (preceding-char) ?\t))
          (forward-char 1)

        ;; otherwise make sure exactly one space is before cursor
        (if (= (preceding-char) ?\t)
            (progn
              (delete-char -1)
              (if (= (following-char) ? )
                  (forward-char 1)
                (insert ? )
                )))

        ;; delete all space after cursor
        (while (or (= (following-char) ? )
                   (= (following-char) ?\t))
          (delete-char 1))

        ;; move to next character
        (if (not (eolp))
            (forward-char 1)

          ;; make sure that there are no spaces at end of lines
          (if (= (preceding-char) ? )
              (delete-char -1))
          )
        ))
    ))

(defun mjm-align-no-space (skip-func skip-arg)
  "Get rid of all spaces and tabs outside of indentation"
  (goto-char (point-min))

  (while (< (point) (point-max))

    ;; if at the end of a line from prev iteration, move past newline
    (forward-char 1)

    ;; skip indentation, if any
    (apply skip-func skip-arg nil)

    ;; process rest of line, if any
    (while (and (not (eolp))
                (< (point) (point-max))
                )
      (while (or (= (following-char) ? )
                 (= (following-char) ?\t))
        (delete-char 1))

      (if (not (eolp))
          (forward-char 1))
      )
    )
  )

(defun mjm-align-condense-space (search-char zero-space after-sep)
  "condense horizontal space within region"
  (let ((skip-arg (and after-sep search-char)))
    (if (or (not zero-space) (= search-char ? ))
        (mjm-align-one-space 'mjm-align-skip-indent skip-arg)
      (mjm-align-no-space 'mjm-align-skip-indent skip-arg)
      )))

(defun mjm-align-region
  (beg end search-char kill-all-space align-at-char eol-sep after-sep)
  "Aligns successive lines in a region by some character."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (max-list)

        ;; We use spaces instead of tabs
        (if (= search-char ?\t)
            (setq search-char ? ))

        ;; Condense all horizontal space
        (mjm-align-condense-space search-char kill-all-space after-sep)

        ;; Create list of relative offsets
        (goto-char (point-min))
        (beginning-of-line)
        (while (< (point) (point-max))
          (setq max-list (mjm-max-two-lists
                          max-list
                          (mjm-align-offset-list search-char eol-sep)))
          (forward-line 1))

        ;; Indent according to the offsets
        (goto-char (point-min))
        (beginning-of-line)
        (while (< (point) (point-max))
          (mjm-align-by-list max-list search-char align-at-char)
          (forward-line 1))
        (message "Aligned with separator %c" search-char)
        max-list))))

;; user interface to alignment
(defconst mjm-align-defaults
  '(
    (?\= . ((alignchar . t)  (killspace . nil)(eolsep . nil)(aftersep . nil)))
    (?\. . ((alignchar . t)  (killspace . nil)(eolsep . nil)(aftersep . nil)))
    (?\, . ((alignchar . nil)(killspace . nil)(eolsep . t)  (aftersep . nil)))
    (?\  . ((alignchar . nil)(killspace . nil)(eolsep . nil)(aftersep . nil)))
    (?\/ . ((alignchar . t)  (killspace . nil)(eolsep . nil)(aftersep . t)))
    )
  "Alignment options for some characters."
  )

(defun mjm-align (beg end search-char nodefault)
  "Interactively aligns successive lines in a region by some character.
This will ask you for a character to align by.  After that, there are
a few options for aligning to be specified.

killspace: describes whether to kill all spaces before starting alignment
alignchar: tells whether to line up the separators
eolsep:    tells whether to consider the end of the line as a separator.
aftersep:  tells whether to wait until after the first separator to start
           deleting spaces.  Normally only spaces in the indentation are
           preserved.

To try to make it a bit easier on the user of these, defaults have been
set for several characters, and you can add your own (or change the existing
ones) by prepending your defaults to mjm-align-defaults.  Any defaults not
set there for a given character will result in asking the user for that
option.

Also note that by giving a prefix arg, you can get mjm-align to ask you
for every option--this allows you to override the defaults.

This version does not allow aligning by a set of characters, but you can
often acheive the right results by applying this a couple times.  For
example, you can make:

   abcd = efghijklmn; // opqrstuvwxyz
   abcdefghijk = lmnopq; // rstuvwxyz

into:

   abcd        = efghijklmn; // opqrstuvwxyz
   abcdefghijk = lmnopq;     // rstuvwxyz

by first aligning by '=' and then by '/'.
"
  (interactive "r\ncSeparator: \nP")

  ;; Look up the defaults list for this char
  (let* ((defaults (or nodefault (cdr (assoc search-char mjm-align-defaults))))

         ;; Pick out default alignment value or ask user
         (align-cell (assoc 'alignchar defaults))
         (align-char-p (if align-cell
                           (cdr align-cell)
                         (y-or-n-p (format "Align [%c]'s " search-char))))

         ;; Pick out default spacing value or ask user
         (space-cell (assoc 'killspace defaults))
         (kill-space-p (if space-cell
                           (cdr space-cell)
                         (y-or-n-p "Kill all extra spaces ")))

         (eol-cell (assoc 'eolsep defaults))
         (eol-sep-p (if eol-cell
                        (cdr eol-cell)
                      (y-or-n-p "End of line counts as separator ")))

         (after-cell (assoc 'aftersep defaults))
         (after-sep-p (if after-cell
                          (cdr after-cell)
                        (y-or-n-p "Don't condense spaces before first sep ")))
         )

    ;; Do the alignment according to all the options
    (mjm-align-region
     beg end search-char kill-space-p align-char-p eol-sep-p after-sep-p
     )
    ))
