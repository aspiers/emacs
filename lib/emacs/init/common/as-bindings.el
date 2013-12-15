(load-library "as-loaddefs")
(require 'as-progress)
;; For faster compilation
(eval-when-compile (require 'cl))

(eval-and-compile (as-loading-started))

;; Why is this a separate file?

;; The following bindings are grouped by their location in the
;; "Universal Keymap", so that I can coordinate bindings globally and
;; prevent conflicts.  Otherwise I would have preferred to group the
;; code logically, e.g. a section for each mode, and have it all
;; within the relevant files in ~/.emacs.d/init.d.
;;
;; Update: I think the above approach is flawed, since it's easy to
;; figure out what bindings are available without that grouping (using
;; <prefix> C-h), and grouping logically is easier to maintain.
;; Having said that, a clear strategy on use of keymaps is useful.

;; Ben uses (define-key global-map ...) instead of (global-set-key ...)
;; The latter is a wrapper around the former, so is a bit safer.

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(as-progress "key bindings...done")

(provide 'as-bindings)

