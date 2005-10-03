;;; Test cases for emacs-wiki
;;; Sacha Chua (sacha@free.net.ph)
;; Version: 3.0.0

;; Just load this and complain loudly if there are any errors.
;; It only tests a handful of functions right now.

(require 'emacs-wiki)

(defun emacs-wiki-test/emacs-wiki-link-escape ()
  (let ((test-cases '(("Test" . "Test")
                      ("[Foo" . "%5BFoo")
                      ("Bar]" . "Bar%5D")
                      ("[B[a]z]" . "%5BB%5Ba%5Dz%5D")
                      ("Baz#" . "Baz#"))))
    (mapc (lambda (item)
            (unless (equal (emacs-wiki-link-escape (car item))
                           (cdr item))
              (error "Test case escape %S failed" item))
            (unless (equal (emacs-wiki-link-unescape (cdr item))
                           (car item))
              (error "Test case unescape %S failed" item)))
          test-cases))
  (let ((test-cases '(("Test" . "Test")
                      ("[Foo" . "%5BFoo")
                      ("Bar]" . "Bar%5D")
                      ("[B[a]z]" . "%5BB%5Ba%5Dz%5D")
                      ("#Baz#" . "%23Baz%23"))))
    (mapc (lambda (item)
            (unless (equal (emacs-wiki-link-escape (car item) t)
                           (cdr item))
              (error "Test case escape further %S failed" item))
            (unless (equal (emacs-wiki-link-unescape (cdr item) t)
                           (car item))
              (error "Test case unescape further %S failed" item)))
          test-cases)))
(emacs-wiki-test/emacs-wiki-link-escape)

(defun emacs-wiki-test/emacs-wiki-wiki-link-target ()
  (interactive)
  (let ((test-cases
         '(("[[SomePage][%5B%5BSomePage%5D%5D]]" "SomePage")
           ("[[http://sacha.free.net.ph?foo=1][%5B%5BSomePage%5D%5D]]" "http://sacha.free.net.ph?foo=1")
           ("[[http://sacha.free.net.ph%5Dfoo=1][%5B%5BSomePage%5D%5D]]" "http://sacha.free.net.ph]foo=1")
           ("[[http://sacha.free.net.ph%5Dfoo=1]]" "http://sacha.free.net.ph]foo=1")
           ("[[http://sacha.free.net.ph?foo=1]]" "http://sacha.free.net.ph?foo=1")
           ("SomePage" "SomePage"))))
    (mapc (lambda (item)
            (let ((output (emacs-wiki-wiki-link-target (elt item 0))))
              (unless (equal output (elt item 1))
                (error "Test case wiki-link-target %S failed output %S" item output))))
          test-cases)))
(emacs-wiki-test/emacs-wiki-wiki-link-target)

(defun emacs-wiki-test/emacs-wiki-make-link ()
  (interactive)
  (let ((test-cases
         '(("[[SomePage][%5B%5BSomePage%5D%5D]]" nil "[[SomePage][%5B%5BSomePage%5D%5D]]")
           ("SomePage" nil "SomePage") ;; WikiName page
           ("SomePageXXX" nil "[[SomePageXXX]]") ;; Like a WikiName, isn't
           ("http://sacha.free.net.ph" nil "http://sacha.free.net.ph") ;; URL
           ("[[http://sacha.free.net.ph]]" nil "http://sacha.free.net.ph") ;; URL
           ("[[http://sacha.free.net.ph][http://sacha.free.net.ph]]" nil "http://sacha.free.net.ph") ;; URL
           ("xxx" nil "[[xxx]]") ;; Non-wiki-name
           ("SomePage" "SomePage" "SomePage") ;; WikiName, same page
           ;; emacs-wiki-make-link is really smart!
           ("[[SomePage][SomePage]]" nil "SomePage")
           ("[[SomePage#2][SomePage]]" nil "[[SomePage#note2][SomePage]]")
           ("[[somepage][SomePage]]" nil "[[somepage][SomePage]]")
           ("[[somepage][somepage]]" nil "[[somepage]]")
           ("SomePage" "AnotherPage" "[[SomePage][AnotherPage]]") ;; WikiName, different page
           ("SomePage#23" "SomePage" "[[SomePage#note23][SomePage]]") ;; WikiName page
           ("SomePageXXX" "Foo" "[[SomePageXXX][Foo]]")
           ("[[FooBar][An old link]]" nil "[[FooBar][An old link]]")
           ("[[FooBar][An old link%5D]]" nil "[[FooBar][An old link%5D]]")
           ("[[FooBar][An old link]]" "A new link" "[[FooBar][A new link]]")
           ("[[FooBar][An old link]]" "[A new link]" "[[FooBar][%5BA new link%5D]]")
           ("Something" "[Foo]" "[[Something][%5BFoo%5D]]") ;; Escape
           ("[Something]" "[Foo]" "[[%5BSomething%5D][%5BFoo%5D]]")))) ;; Escape
    (mapc (lambda (item)
            (let ((output (emacs-wiki-make-link (elt item 0)
                                                 (elt item 1))))
              (unless (equal output (elt item 2))
                (error "Test case make-link %S failed output %S" item output))))
          test-cases)))
(emacs-wiki-test/emacs-wiki-make-link)

(defun emacs-wiki-test (test-name test-cases test-function)
  (while test-cases
    (let* ((test-case (car test-cases))
           (expected (car test-case))
           (output (apply test-function (cdr test-case))))
      (unless (equal output expected)
        (error "%s: expected %s output %s" test-name expected output)))
    (setq test-cases (cdr test-cases))))

;; (let ((emacs-wiki-bare-digits-anchor-prefix "note"))
;;   (emacs-wiki-test
;;    "emacs-wiki-wiki-link-target"
;;    '(("FooBar" "FooBar")
;;      ("FooBar" "[[FooBar]]")
;;      ("FooBar" "[[FooBar][Baz]]")
;;      ("FooBar#A1" "[[FooBar#A1]]")
;;      ("FooBar#note1" "[[FooBar#1]]")
;;      ("FooBar#note1A" "[[FooBar#1A]]"))
;;    'emacs-wiki-wiki-link-target))

;; Future: Macros, if I can figure them out.

;; (defmacro emacs-wiki-test/define-test (test-name test-cases lambda-expression)
;;   (let ((function-name (make-symbol (concat "emacs-wiki-test/" test-name)))
;;         (test-cases-var (make-symbol "test-cases"))
;;         (number-var (make-symbol "number"))
;;         (counter-var (make-symbol "counter")))
;;     `(progn
;;        (defun ,function-name (&optional ,number-var)
;;          (interactive (list (when current-prefix-arg (read-string "Test case number: "))))
;;          (when (stringp ,number-var)
;;            (setq ,number-var (string-to-number ,number-var)))
;;          (let ((,test-cases-var ,test-cases)
;;                (,counter-var 0))
;;            (when ,number-var
;;              (setq ,counter-var ,number-var)
;;              (setcdr ,test-cases-var nil))
;;            (while (and ,test-cases-var
;;                        (apply ,lambda-expression ,number-var
;;                               (car ,test-cases-var)))
;;              (setq ,counter-var (1+ ,counter-var))
;;              (setq ,test-cases-var (cdr ,test-cases-var)))))
;;        (,function-name))))

;; (emacs-wiki-test/define-test
;;  "emacs-wiki-wiki-link-target"
;;  '(("[[SomePage][%5B%5BSomePage%5D%5D]]" "SomePage")
;;    ("[[http://sacha.free.net.ph?foo=1][%5B%5BSomePage%5D%5D]]" "http://sacha.free.net.ph?foo=1")
;;    ("[[http://sacha.free.net.ph%5Dfoo=1][%5B%5BSomePage%5D%5D]]" "http://sacha.free.net.ph]foo=1")
;;    ("[[http://sacha.free.net.ph%5Dfoo=1]]" "http://sacha.free.net.ph]foo=1")
;;    ("[[http://sacha.free.net.ph?foo=1]]" "http://sacha.free.net.ph?foo=1")
;;    ("SomePage" "SomePage"))
;;  (lambda (test-case orig expected-result)
;;    (let ((output (emacs-wiki-wiki-link-target orig)))
;;      (unless (string= output expected-result)
;;        (error "Failed test case %S output %S expected %S"
;;               test-case output expected-result)))))


;;;_* Local emacs vars.
;; Local variables:
;; change-log-default-name: "../ChangeLog"
;; End:
;;; emacs-wiki-test.el ends here
