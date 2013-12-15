(autoload 'key-chord-mode "key-chord")
(autoload 'key-chord-define-global "key-chord")
(key-chord-mode 1)
(key-chord-define-global "zf" 'iy-go-to-char)

(global-set-key [(control N)] 'next-logical-line)
(global-set-key [(control P)] 'previous-logical-line)

(autoload 'ace-jump-mode "ace-jump-mode" nil t)
(global-set-key [(control ?0)] 'ace-jump-mode)
(autoload 'idomenu "idomenu" nil t)
(global-set-key [(control ?1)] 'idomenu)
(global-set-key [(control meta !)]        'idomenu)


(global-set-key [(meta g)]      'goto-line)          ;; was set-face
(global-set-key [(meta E)]                'mark-end-of-sentence)
(global-set-key [(control E)]             'bn-end-of-line-but-one)


(autoload 'bn-end-of-line-but-one "as-editing" "bn-end-of-line-but-one" t)


;; emacs < 22 doesn't have x-clipboard-yank
(if (boundp 'x-clipboard-yank)
    (global-set-key [(shift insert)] 'x-clipboard-yank)
  (global-set-key [(shift insert)] 'clipboard-yank))

(global-set-key [(meta i)]     'indent-relative)    ;; was tab-to-tab-stop


(provide 'as-point-motion)
