;; If you're using color-theme.el, this background should match the
;; background of your chosen theme
;; (setq as-default-background
;;    "seashell1"
;;    "wheat"
;;    "blanched almond"
;;    "antique white"
;;    "gray90"
;;    )

;; (setq frame-background-mode
;;       (cond ((and (= emacs-major-version 21) window-system) 'light)
;;             (t 'dark)))

;; (and (eq frame-background-mode 'light)
;;      (custom-set-variables
;;       `(vc-annotate-background ,as-default-background)))

;; (and (>= emacs-major-version 21)
;;      (progn
;;        (custom-set-faces
;;         `(default
;;            ((((class color) (background light))
;;              (:background ,as-default-background))))
;;         `(fringe
;;           ((((class color) (background light))
;;             (:background ,as-default-background))))
;;         `(mmm-default-submode-face
;;           ((((class color) (background light))
;;             (:background ,as-default-background)))))
;;        ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list
   (quote
    ("~/share/info" "/share/usr/info" "/usr/local/info" "/usr/share/info")))
 '(LilyPond-command-alist
   (quote
    (quote
     (("LilyPond" "lilypond %s" "%s" "%l" "View")
      ("2PS" "lilypond -f ps %s" "%s" "%p" "ViewPS")
      ("Book" "lilypond-book %x" "%x" "%l" "LaTeX")
      ("LaTeX" "latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI")
      ("View" "pdf %f")
      ("ViewPDF" "pdf %f")
      ("ViewPS" "gv --watch %p")
      ("Midi" "")
      ("MidiAll" "")))))
 '(LilyPond-pdf-command "okular")
 '(adaptive-fill-regexp "[ 	]\\{,10\\}\\([-!|#%;>*·•‣⁃◦]+[ 	]*\\)*")
 '(align-dq-string-modes
   (quote
    (vhdl-mode emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode c++-mode c-mode java-mode python-mode)))
 '(align-exclude-rules-list
   (quote
    ((exc-dq-string
      (regexp . "\"\\([^\"
]+\\)\"")
      (repeat . t)
      (modes . align-dq-string-modes))
     (exc-sq-string
      (regexp . "'\\([^'
]+\\)'")
      (repeat . t)
      (modes . align-sq-string-modes))
     (exc-open-comment
      (regexp lambda
              (end reverse)
              (funcall
               (if reverse
                   (quote re-search-backward)
                 (quote re-search-forward))
               (concat "[^ 	
\\\\]"
                       (regexp-quote comment-start)
                       "\\(.+\\)$")
               end t))
      (modes . align-open-comment-modes))
     (exc-c-comment
      (regexp . "/\\*\\(.+\\)\\*/")
      (repeat . t)
      (modes . align-c++-modes))
     (exc-c-func-params
      (regexp . "(\\([^)
]+\\))")
      (repeat . t)
      (modes . align-c++-modes))
     (exc-c-macro
      (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$")
      (group . 2)
      (modes . align-c++-modes))
     (exc-perl-comment
      (regexp . "^\\s-*#.*$")
      (modes . align-perl-modes)))))
 '(align-rules-list
   (quote
    ((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes quote
             (org-mode)))
     (lisp-second-arg
      (regexp . "\\(^\\s-+[^( 	
]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)")
      (group . 3)
      (modes . align-lisp-modes)
      (run-if lambda nil current-prefix-arg))
     (lisp-alist-dot
      (regexp . "\\(\\s-*\\)\\.\\(\\s-*\\)")
      (group 1 2)
      (modes . align-lisp-modes))
     (open-comment
      (regexp lambda
              (end reverse)
              (funcall
               (if reverse
                   (quote re-search-backward)
                 (quote re-search-forward))
               (concat "[^ 	
\\\\]"
                       (regexp-quote comment-start)
                       "\\(.+\\)$")
               end t))
      (modes . align-open-comment-modes))
     (c-macro-definition
      (regexp . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)")
      (modes . align-c++-modes))
     (c-variable-declaration
      (regexp . "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|=[^=
].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?\\s-*[;,]\\|)\\s-*$\\)")
      (group . 1)
      (modes . align-c++-modes)
      (justify . t)
      (valid lambda nil
             (not
              (or
               (save-excursion
                 (goto-char
                  (match-beginning 1))
                 (backward-word 1)
                 (looking-at "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
               (if
                   (and
                    (boundp
                     (quote font-lock-mode))
                    font-lock-mode)
                   (eq
                    (get-text-property
                     (point)
                     (quote face))
                    (quote font-lock-comment-face))
                 (eq
                  (caar
                   (c-guess-basic-syntax))
                  (quote c)))))))
     (c-assignment
      (regexp . "[^-=!^&*+<>/| 	
]\\(\\s-*[-=!^&*+<>/|]*\\)=\\(\\s-*\\)\\([^= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-c++-modes)
      (justify . t)
      (tab-stop))
     (perl-assignment
      (regexp . "[^=!^&*-+<>/| 	
]\\(\\s-*\\)=[~>]?\\(\\s-*\\)\\([^>= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-perl-modes)
      (tab-stop))
     (python-assignment
      (regexp . "[^=!<> 	
]\\(\\s-*\\)=\\(\\s-*\\)\\([^>= 	
]\\|$\\)")
      (group 1 2)
      (modes . align-python-modes)
      (tab-stop))
     (make-assignment
      (regexp . "^\\s-*\\w+\\(\\s-*\\):?=\\(\\s-*\\)\\([^	
 \\\\]\\|$\\)")
      (group 1 2)
      (modes quote
             (makefile-mode))
      (tab-stop))
     (c-comma-delimiter
      (regexp . ",\\(\\s-*\\)[^/ 	
]")
      (repeat . t)
      (modes . align-c++-modes)
      (run-if lambda nil current-prefix-arg))
     (basic-comma-delimiter
      (regexp . ",\\(\\s-*\\)[^# 	
]")
      (repeat . t)
      (modes append align-perl-modes
             (quote
              (python-mode)))
      (run-if lambda nil current-prefix-arg))
     (c++-comment
      (regexp . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$")
      (modes . align-c++-modes)
      (column . comment-column)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-beginning 1))
               (not
                (bolp)))))
     (c-chain-logic
      (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
      (modes . align-c++-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(/[*/]\\|$\\)"))))
     (perl-chain-logic
      (regexp . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
      (modes . align-perl-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(#\\|$\\)"))))
     (python-chain-logic
      (regexp . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)")
      (modes . align-python-modes)
      (valid lambda nil
             (save-excursion
               (goto-char
                (match-end 2))
               (looking-at "\\s-*\\(#\\|$\\|\\\\\\)"))))
     (c-macro-line-continuation
      (regexp . "\\(\\s-*\\)\\\\$")
      (modes . align-c++-modes)
      (column . c-backslash-column))
     (basic-line-continuation
      (regexp . "\\(\\s-*\\)\\\\$")
      (modes append align-python-modes
             (quote
              (makefile-mode))))
     (tex-record-separator
      (regexp lambda
              (end reverse)
              (align-match-tex-pattern "&" end reverse))
      (group 1 2)
      (modes . align-tex-modes)
      (repeat . t))
     (tex-tabbing-separator
      (regexp lambda
              (end reverse)
              (align-match-tex-pattern "\\\\[=>]" end reverse))
      (group 1 2)
      (modes . align-tex-modes)
      (repeat . t)
      (run-if lambda nil
              (eq major-mode
                  (quote latex-mode))))
     (tex-record-break
      (regexp . "\\(\\s-*\\)\\\\\\\\")
      (modes . align-tex-modes))
     (text-column
      (regexp . "\\(^\\|\\S-\\)\\([ 	]+\\)\\(\\S-\\|$\\)")
      (group . 2)
      (modes . align-text-modes)
      (repeat . t)
      (run-if lambda nil
              (and current-prefix-arg
                   (not
                    (eq
                     (quote -)
                     current-prefix-arg)))))
     (text-dollar-figure
      (regexp . "\\$?\\(\\s-+[0-9]+\\)\\.")
      (modes . align-text-modes)
      (justify . t)
      (run-if lambda nil
              (eq
               (quote -)
               current-prefix-arg)))
     (css-declaration
      (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;")
      (group 1)
      (modes quote
             (css-mode html-mode))))))
 '(align-sq-string-modes (quote (python-mode)))
 '(allout-auto-activation t)
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-revert-check-vc-info nil)
 '(auto-revert-interval 10)
 '(auto-save-interval 120)
 '(backup-directory-alist (quote (("." . ".emacs.backup"))))
 '(blink-cursor-delay 0.0)
 '(blink-cursor-interval 0.3)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "url-handler")
 '(bs-alternative-configuration "cvs")
 '(bs-attributes-list
   (quote
    (("" 1 1 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Buffer" bs--get-name-length 10 left bs--get-name)
     ("" 1 1 left " ")
     ("Size" 5 8 right bs--get-size-string)
     ("" 1 1 left " ")
     ("Mode" 5 12 right bs--get-mode-name)
     ("" 2 2 left "  ")
     ("File" 12 12 left bs--get-file-name)
     ("" 2 2 left "  "))))
 '(bs-configurations
   (quote
    (("all" nil nil nil nil nil)
     ("files" nil nil ".~[0-9.]+~$" bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("files-and-scratch" "^\\*scratch\\*$" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
     ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last)
     ("cvs" "\\*cvs\\*$" nil "" nil bs--sort-by-name))))
 '(bs-default-sort-name "by nothing")
 '(bs-max-window-height 24)
 '(bs-maximal-buffer-name-column 22)
 '(bs-minimal-buffer-name-column 5)
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(case-fold-search t)
 '(coffee-tab-width 4)
 '(color-theme-is-cumulative t)
 '(color-theme-legal-frame-parameters "\\(color\\|mode\\)$")
 '(column-number-mode t)
 '(comment-empty-lines (quote (quote eol)))
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".hg/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(cperl-auto-newline nil)
 '(cperl-auto-newline-after-colon t)
 '(cperl-autoindent-on-semi t)
 '(cperl-close-paren-offset -2)
 '(cperl-continued-statement-offset 0)
 '(cperl-electric-parens-string "{}()")
 '(cperl-font-lock t)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face (quote (quote default)))
 '(cperl-lineup-step 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-under-as-char nil)
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "8f392c0db8c92563397d55464320b53505ece8e6066e8d30575952663787e02f" "1dd84fe8349c012731e10e39c79627841744c1d38556f22fb970de5335a44c90" "056d98129e2f4f3200b16acfdaf2cf87a9442b2ac1ed4196a109edb8f082d7da" "56cb796e4712309ede12a316d99b0a2a8c3b48a8c63bf62c257861ca103cc4d1" "e41c402084437f037d1075fe825ad9d331abe36c4c45104f01c0d14c071b46ac" "3a963acc5629c10113409c0ea5c04a8e1c2159c628bf8b0830cbd154014efe3f" default)))
 '(cvs-buffer-switch-alist (quote ("diff" "status" "log")))
 '(cvs-buffer-switch-list (quote ("diff" "status" "log")))
 '(cvs-find-file-and-jump t)
 '(cvs-invert-ignore-marks (quote ("diff")))
 '(cvs-parse-ignored-messages
   (quote
    ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" ".*New directory `.*' -- ignored.*$" ".*warning: directory CVS specified in argument.*$" ".*but CVS uses CVS for its own purposes; skipping CVS directory.*$")))
 '(cvs-reuse-cvs-buffer (quote subdir))
 '(cvs-use-fileinfo-caches t)
 '(debugger-batch-max-lines 1000)
 '(delete-old-versions t)
 '(delete-selection-mode nil)
 '(diff-switches "-u")
 '(dired-kept-versions 0)
 '(dired-listing-switches "-l")
 '(display-time-mode nil)
 '(dvc-tips-enabled nil)
 '(echo-keystrokes 0.01)
 '(ediff-custom-diff-options "-u")
 '(edit-server-port 9292)
 '(el-get-sources
   (quote
    ((:name magit :pkgname "aspiers/magit")
     (:name org2blog :pkgname "aspiers/org2blog")
     (:name smooth-scrolling :pkgname "DarwinAwardWinner/smooth-scrolling"))))
 '(eldoc-minor-mode-string "")
 '(eldoc-mode t t)
 '(enable-local-eval t)
 '(enable-local-variables :all)
 '(erc-autojoin-channels-alist
   (quote
    (("freenode.net" "#suse" "#orgmode" "#linux-ha" "#openstack" "#opensuse-buildservice" "#tmux")
     ("suse.de" "#suse" "#cloud" "#ha"))))
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-target))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-log-channels-directory (quote erc-generate-log-directory-name-network))
 '(erc-log-insert-log-on-open t)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols log match menu netsplit networks noncommands readonly ring stamp track truncate)))
 '(erc-networks-alist
   (quote
    ((4-irc "4-irc.com")
     (A5KNet "a5knet.com")
     (AbleNet "ablenet.org")
     (Accessirc "accessirc.net")
     (Acestar "acestar.org")
     (Action-IRC "action-irc.net")
     (AfterNET "afternet.org")
     (Alternativenet "altnet.org")
     (AmigaNet "amiganet.org")
     (AngelEyez "angeleyez.net")
     (Anothernet "another.net")
     (ArabChat "arabchat.org")
     (Ars "arstechnica.com")
     (AsiaTalk "asiatalk.org")
     (AstroLink "astrolink.org")
     (Asylumnet "asylumnet.org")
     (Austnet "austnet.org")
     (AwesomeChat "awesomechat.net")
     (Awesomechristians "awesomechristians.com")
     (Axenet "axenet.org")
     (Beyondirc "beyondirc.net")
     (BGIRC "bulgaria.org")
     (Blabbernet "blabber.net")
     (Blitzed "blitzed.org")
     (BrasIRC "brasirc.net")
     (BRASnet "brasnet.org")
     (BubbleNet "bubblenet.org")
     (CCnet "christian-chat.net")
     (Chat-Net "chat-net.org")
     (Chat-Solutions "chat-solutions.org")
     (Chatcafe "chatcafe.net")
     (Chatchannel "chatchannel.org")
     (ChatCircuit "chatcircuit.com")
     (Chatlink "chatlink.org")
     (Chatnet "chatnet.org")
     (ChatNut "chatnut.net")
     (Chatpinoy "chatpinoy.com")
     (ChatPR "chatpr.org")
     (Chatroom "chatroom.org")
     (Chatster "chatster.org")
     (ChatX "chatx.net")
     (China263 "263.net")
     (Cineplex1 "cineplex1.com")
     (CNN "cnn.com")
     (CobraNet "cobra.net")
     (Coolchat "coolchat.net")
     (Criten "criten.net")
     (Cyberchat "cyberchat.org")
     (CyGanet "cyga.net")
     (DALnet "dal.net")
     (Dark-Tou-Net "d-t-net.de")
     (Darkfire "darkfire.net")
     (DarkMyst "darkmyst.org")
     (Darkserv "darkserv.net")
     (Darksystem "darksystem.com")
     (Darktree "darktree.net")
     (DayNet "daynet.org")
     (Deepspace "deepspace.org")
     (Different "different.net")
     (Digarix "digarix.net")
     (Digatech "digatech.net")
     (Digital-Base "digital-base.net")
     (Digitalirc "digitalirc.net")
     (Discussioni "discussioni.org")
     (DorukNet "doruk.net.tr")
     (DWChat "dwchat.net")
     (Dynastynet "dynastynet.net")
     (EFnet nil)
     (EgyptianIRC "egyptianirc.net")
     (Eircnet "eircnet.org")
     (Eleethal "eleethal.com")
     (EntertheGame "enterthegame.com")
     (EpiKnet "epiknet.org")
     (EsperNet "esper.net")
     (Esprit "esprit.net")
     (euIRC "euirc.net")
     (Evilzinc "evilzinc.net")
     (ExodusIRC "exodusirc.net")
     (FDFnet "fdfnet.net")
     (FEFnet "fef.net")
     (Financialchat "financialchat.com")
     (Forestnet "forestnet.org")
     (ForeverChat "foreverchat.net")
     (Fraggers "fraggers.co.uk")
     (FreedomChat "freedomchat.net")
     (FreedomIRC "freedomirc.net")
     (freenode "freenode.net")
     (FunNet "funnet.org")
     (GalaxyNet "galaxynet.org")
     (Gamesnet "gamesnet.net")
     (GammaForce "gammaforce.org")
     (GIKInet "giki.edu.pk")
     (GizNet "giznet.org")
     (Globalchat "globalchat.org")
     (GlobIRC "globirc.net")
     (Goldchat "goldchat.nl")
     (Goodchatting "goodchatting.com")
     (GravityLords "gravitylords.net")
     (GRnet "irc.gr")
     (GulfChat "gulfchat.net")
     (HabberNet "habber.net")
     (HanIRC "hanirc.org")
     (Hellenicnet "mirc.gr")
     (IceNet "icenet.org.za")
     (ICQnet "icq.com")
     (iip "anon.iip")
     (Infatech "infatech.net")
     (Infinity "infinity-irc.org")
     (Infomatrix "infomatrix.net")
     (Inside3D "inside3d.net")
     (InterlinkChat "interlinkchat.net")
     (IRC-Chile "irc.cl")
     (IRC-Hispano "irc-hispano.org")
     (IRCchat "ircchat.tk")
     (IRCGate "ircgate.net")
     (IRCGeeks "ircgeeks.org")
     (IRChat "irchat.net")
     (IrcLordz "irclordz.com")
     (IrcMalta "ircmalta.org")
     (IRCnet nil)
     (IRCSoulZ "ircsoulz.net")
     (IRCSul "wnet.com.br")
     (IrcTalk "irctalk.net")
     (Irctoo "irctoo.net")
     (IRCtown "irc.irctown.net")
     (IRCworld "ircworld.org")
     (ircXtreme "ircXtreme.net")
     (Israelnet "israel.net")
     (K0wNet "k0w.net")
     (KDFSnet "kdfs.net")
     (Kemik "kemik.net")
     (Kewl\.Org "kewl.org")
     (Kickchat "kickchat.com")
     (KidsWorld "kidsworld.org")
     (Knightnet "knightnet.net")
     (Konfido\.Net "konfido.net")
     (Kreynet "krey.net")
     (Krono "krono.net")
     (Krushnet "krushnet.org")
     (LagNet "lagnet.org.za")
     (Librenet "librenet.net")
     (LinkNet "link-net.org")
     (LinuxChix "cats.meow.at\\|linuxchix.org")
     (Liquidized "liquidized.net")
     (M-IRC "m-sys.org")
     (MagicStar "magicstar.net")
     (Mavra "mavra.net")
     (MediaDriven "mediadriven.com")
     (mIRC-X "mircx.com")
     (Morat "morat.net")
     (MusicCity "musiccity.com")
     (Mysteria "mysteria.net")
     (Mysterychat "mysterychat.net")
     (Mystical "mystical.net")
     (Narancs "narancs.com")
     (Net-France "net-france.com")
     (Nevernet "nevernet.net")
     (Newnet "newnet.net")
     (Nexusirc "nexusirc.org")
     (NightStar "nightstar.net")
     (NitrousNet "nitrousnet.net")
     (Novernet "novernet.com")
     (Nullrouted "nullrouted.org")
     (NullusNet "nullus.net")
     (OFTC "oftc.net")
     (OpChat "opchat.org")
     (Openprojects "openprojects.net")
     (Othernet "othernet.org")
     (OtherSide "othersideirc.net")
     (Outsiderz "outsiderz.com")
     (OzOrg "oz.org")
     (Peacefulhaven "peacefulhaven.net")
     (PhazedIRC "phazedirc.net")
     (Philchat "philchat.net")
     (phrozN "phrozn.net")
     (PiNet "praetorians.org")
     (Pinoycentral "abs-cbn.com")
     (Planetarion "planetarion.com")
     (POLNet "ircnet.pl")
     (Psionics "psionics.net")
     (PTirc "ptirc.com.pt")
     (PTlink "ptlink.net")
     (PTnet "ptnet.org")
     (QChat "qchat.net")
     (QuakeNet "quakenet.org")
     (Realirc "realirc.org")
     (RealmNET "realmnet.com")
     (Rebelchat "rebelchat.org")
     (Red-Latina "red-latina.org")
     (RedLatona "redlatona.net")
     (Relicnet "relic.net")
     (Rezosup "rezosup.org")
     (Risanet "risanet.com")
     (Rubiks "rubiks.net")
     (Rusnet "nil")
     (Sandnet "sandnet.net")
     (Scunc "scunc.net")
     (SerbianCafe "serbiancafe.ws")
     (SexNet "sexnet.org")
     (ShadowFire "shadowfire.org")
     (ShadowWorld "shadowworld.net")
     (SkyNet "bronowski.pl")
     (SlashNET "slashnet.org")
     (SolarStone "solarstone.net")
     (Sorcery "sorcery.net")
     (SourceIRC "sourceirc.net")
     (SpaceTronix "spacetronix.net")
     (Spirit-Harmony "spirit-harmony.com")
     (StarChat "starchat.net")
     (StarEquinox "starequinox.net")
     (Starlink "starlink.net")
     (starlink-irc "starlink-irc.org")
     (StarWars-IRC "starwars-irc.net")
     (Stormdancing "stormdancing.net")
     (Superchat "superchat.org")
     (Sysopnet "sysopnet.org")
     (Telstra "telstra.com")
     (TR-net "dominet.com.tr")
     (Tri-net "tri-net.org")
     (TriLink "ft4u.net")
     (TurkishChat "turkishchat.org")
     (UberNinja "uberninja.net")
     (UICN "uicn.net")
     (UltraIRC "ultrairc.net")
     (UnderChat "underchat.it")
     (Undernet "undernet.org")
     (UnderZ "underz.org")
     (UniChat "irc.uni-chat.net")
     (UnionLatina "unionlatina.org")
     (Univers "univers.org")
     (UnixR "unixr.net")
     (Vidgamechat "vidgamechat.com")
     (VirtuaNet "virtuanet.org")
     (Vitamina "vitamina.ca")
     (Voila "voila.fr")
     (Wahou "wf-net.org")
     (Warpednet "warped.net")
     (Weaklinks "weaklinks.net")
     (Webnet "webchat.org")
     (WinChat "winchat.net")
     (WinIRC "winirc.org")
     (WorldIRC "worldirc.org")
     (WyldRyde "wyldryde.net")
     (XentoniX "xentonix.net")
     (Xevion "xevion.net")
     (XNet "xnet.org")
     (XWorld "xworld.org")
     (ZAnetNet "zanet.net")
     (ZAnetOrg "zanet.org.za")
     (ZiRC "zirc.org")
     (ZUHnet "zuh.net")
     (Zurna "zurna.net")
     (SUSE "suse.de"))))
 '(erc-nick "aspiers")
 '(erc-server-alist
   (quote
    (("4-irc: Random server" 4-irc "4-irc.com" 6667)
     ("A5KNet: Random server" A5KNet "irc.a5knet.com"
      ((6660 6669)))
     ("AbleNet: Random server" AbleNet "irc.ablenet.org" 6667)
     ("Accessirc: Random server" Accessirc "irc.accessirc.net" 6667)
     ("Acestar: Random server" Acestar "irc.acestar.org" 6667)
     ("Action-IRC: Random server" Action-IRC "irc.action-irc.net"
      ((6660 6669)))
     ("AfterNET: Random server" AfterNET "irc.afternet.org" 6667)
     ("Alternativenet: Random server" Alternativenet "irc.altnet.org" 6667)
     ("AmigaNet: Random server" AmigaNet "irc.amiganet.org" 6667)
     ("AngelEyez: Random server" AngelEyez "irc.angeleyez.net"
      ((6666 7000)))
     ("AnotherNet: Random server" Anothernet "irc.another.net"
      (6667 7000))
     ("ArabChat: Random server" ArabChat "irc.arabchat.org"
      ((6660 6667)))
     ("Ars-OpenIRC: Random server" Ars "irc.arstechnica.com" 6667)
     ("AsiaTalk: Random server" AsiaTalk "irc.asiatalk.org"
      ((6667 6669)
       7000))
     ("AstroLink: Random server" AstroLink "irc.astrolink.org"
      ((6660 6667)))
     ("Asylumnet: Random server" Asylumnet "irc.asylum-net.org"
      ((6661 6669)
       7000 7777))
     ("Austnet: Random AU server" Austnet "au.austnet.org" 6667)
     ("Austnet: Random NZ server" Austnet "nz.austnet.org" 6667)
     ("Austnet: Random SG server" Austnet "sg.austnet.org" 6667)
     ("Austnet: Random US server" Austnet "us.austnet.org" 6667)
     ("AwesomeChat: Random server" AwesomeChat "irc.awesomechat.net"
      ((6661 6669)))
     ("Awesomechristians: Random server" Awesomechristians "irc.awesomechristians.com" 7000)
     ("Axenet: Random server" Axenet "irc.axenet.org"
      ((6660 6667)))
     ("BeyondIRC: Random server" Beyondirc "irc.beyondirc.net"
      ((6660 6669)))
     ("BGIRC: Random server" BGIRC "irc.bulgaria.org"
      ((6666 6669)
       7000))
     ("Blabbernet: Random server" Blabbernet "irc.blabber.net"
      (6667 7000))
     ("Blitzed: Random server" Blitzed "irc.blitzed.org"
      (6667 7000))
     ("Brasirc: Random server" Brasirc "irc.brasirc.net"
      ((6666 6667)))
     ("Brasirc: BR, PA, Belem" Brasirc "irc.libnet.com.br"
      ((6666 6668)
       7777 8002))
     ("BRASnet: Random European server" BRASnet "eu.brasnet.org"
      ((6665 6669)))
     ("BRASnet: Random US server" BRASnet "us.brasnet.org"
      ((6665 6669)))
     ("BubbleNet: Random server" BubbleNet "irc.bubblenet.org"
      ((6667 6669)))
     ("CCnet: Random server" CCnet "irc.cchat.net"
      (6667 7000))
     ("CCnet: US, TX, Dallas" CCnet "irc2.cchat.net"
      (6667 7000))
     ("Chat-Net: Random server" Chat-Net "irc.chat-net.org" 6667)
     ("Chat-Solutions: Random server" Chat-Solutions "irc.chat-solutions.org" 6667)
     ("Chatcafe: Random server" Chatcafe "irc.chatcafe.net" 6667)
     ("Chatchannel: Random server" Chatchannel "irc.chatchannel.org"
      ((6666 6669)
       7000))
     ("ChatCircuit: Random server" ChatCircuit "irc.chatcircuit.com" 6668)
     ("Chatlink: Random server" Chatlink "irc.chatlink.org" 6667)
     ("Chatnet: Random AU server" Chatnet "au.chatnet.org" 6667)
     ("Chatnet: Random EU server" Chatnet "eu.chatnet.org" 6667)
     ("Chatnet: Random US server" Chatnet "us.chatnet.org" 6667)
     ("ChatNut: Random server" ChatNut "irc.chatnut.net"
      (6667 7000))
     ("Chatpinoy: Random server" Chatpinoy "irc.chatpinoy.com" 6667)
     ("ChatPR: Random server" ChatPR "irc.chatpr.org" 6667)
     ("Chatroom: Random server" Chatroom "irc.chatroom.org" 6667)
     ("Chatster: Random server" Chatster "irc.chatster.org" 6667)
     ("ChatX: Random server" ChatX "irc.chatx.net" 6667)
     ("China263: Random server" China263 "irc.263.net" 6667)
     ("Cineplex1: Random server" Cineplex1 "irc.cineplex1.com"
      ((6666 6668)))
     ("CNN: CNN News discussions" CNN "chat.cnn.com"
      ((6667 6669)
       7000))
     ("CobraNet: Random server" CobraNet "irc.cobra.net" 6667)
     ("Coolchat: Random server" Coolchat "irc.coolchat.net" 6667)
     ("Criten: Random server" Criten "irc.criten.net" 6667)
     ("Cyberchat: Random server" Cyberchat "irc.cyberchat.org"
      (6667 6668))
     ("CyGanet: Random server" CyGanet "irc.cyga.net" 6667)
     ("DALnet: AS, MY, Coins" DALnet "coins.dal.net"
      ((6663 6668)
       7000))
     ("DALnet: CA, ON, Sodre" DALnet "sodre.on.ca.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: EU, DE, Nexgo" DALnet "nexgo.de.eu.dal.net"
      ((6664 6669)
       7000))
     ("DALnet: EU, NO, Powertech" DALnet "powertech.no.eu.dal.net"
      ((6666 6667)
       7000))
     ("DALnet: EU, SE, Borg" DALnet "borg.se.eu.dal.net"
      (6667 7000))
     ("DALnet: EU, SE, Ced" DALnet "ced.se.eu.dal.net"
      (6667 7000))
     ("DALnet: US, GA, Astro" DALnet "astro.ga.us.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: US, GA, Dragons" DALnet "dragons.ga.us.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: US, GA, Elysium" DALnet "elysium.ga.us.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: US, MA, Twisted" DALnet "twisted.ma.us.dal.net"
      ((6660 6669)
       7001 7002))
     ("DALnet: US, MO, Global" DALnet "global.mo.us.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: US, NJ, Liberty" DALnet "liberty.nj.us.dal.net"
      ((6662 6669)
       7000))
     ("DALnet: US, VA, Wombat" DALnet "wombat.va.us.dal.net"
      ((6661 6669)
       7000))
     ("DALnet: Random EU server" DALnet "irc.eu.dal.net" 6667)
     ("DALnet: Random US server" DALnet "irc.dal.net"
      ((6660 6667)))
     ("Dark-Tou-Net: Random server" Dark-Tou-Net "irc.d-t-net.de" 6667)
     ("Darkfire: Random server" Darkfire "irc.darkfire.net"
      (6667 7000 8000))
     ("DarkMyst: Random server" DarkMyst "irc.darkmyst.org" 6667)
     ("Darkserv: Random server" Darkserv "irc.darkserv.net" 6667)
     ("Darksystem: Random server" Darksystem "irc.darksystem.com" 6667)
     ("Darktree: Random server" Darktree "irc.darktree.net" 6667)
     ("DayNet: Random server" DayNet "irc.daynet.org" 6667)
     ("Deepspace: Disability network" Deepspace "irc.deepspace.org" 6667)
     ("Different: Random server" Different "irc.different.net" 6667)
     ("Digarix: Random server" Digarix "irc.digarix.net" 6667)
     ("Digatech: Random server" Digatech "irc.digatech.net" 6667)
     ("Digital-Base: Random server" Digital-Base "irc.digital-base.net"
      ((6660 7000)))
     ("Digitalirc: Random server" Digitalirc "irc.digitalirc.net" 6667)
     ("Discussioni: Random server" Discussioni "irc.discussioni.org"
      ((6666 6669)))
     ("DorukNet: TR, Istanbul" DorukNet "irc.doruk.net.tr"
      ((6660 6669)
       7000 8888))
     ("Dreamcast: Random server" Dreamcast "irc0.dreamcast.com" 6667)
     ("DWChat: Random server" DWChat "irc.dwchat.net" 6667)
     ("Dynastynet: Random server" Dynastynet "irc.dynastynet.net" 6667)
     ("EFnet: CA, AB, Edmonton (arcti)" EFnet "irc.arcti.ca" 6667)
     ("EFnet: CA, AB, Edmonton (mpls)" EFnet "irc.mpls.ca"
      ((6660 6669)))
     ("EFnet: CA, ON, Toronto" EFnet "irc2.magic.ca" 6667)
     ("EFnet: CA, QB, Montreal" EFnet "irc.qeast.net" 6667)
     ("EFnet: EU, DK, Aarhus" EFnet "irc.inet.tele.dk" 6667)
     ("EFnet: EU, FI, Helsinki" EFnet "efnet.cs.hut.fi" 6667)
     ("EFnet: EU, FR, Paris" EFnet "irc.isdnet.fr"
      ((6667 6669)))
     ("EFnet: EU, NL, Amsterdam" EFnet "efnet.vuurwerk.nl" 6667)
     ("EFnet: EU, NO, Homelien" EFnet "irc.homelien.no"
      (5190
       (6666 6667)
       (7000 7001)))
     ("EFnet: EU, NO, Oslo" EFnet "irc.daxnet.no"
      ((6666 7000)))
     ("EFnet: EU, PL, Warszawa" EFnet "irc.efnet.pl" 6667)
     ("EFnet: EU, RU, Moscow" EFnet "irc.rt.ru"
      ((6661 6669)))
     ("EFnet: EU, SE, Dalarna" EFnet "irc.du.se"
      ((6666 6669)))
     ("EFnet: EU, SE, Gothenberg" EFnet "irc.hemmet.chalmers.se"
      ((6666 7000)))
     ("EFnet: EU, SE, Sweden" EFnet "irc.light.se" 6667)
     ("EFnet: EU, UK, London (carrier)" EFnet "irc.carrier1.net.uk"
      ((6666 6669)))
     ("EFnet: EU, UK, London (demon)" EFnet "efnet.demon.co.uk"
      ((6665 6669)))
     ("EFnet: ME, IL, Inter" EFnet "irc.inter.net.il"
      ((6665 6669)))
     ("EFnet: US, AZ, Phoenix" EFnet "irc.easynews.com"
      (6660
       (6665 6667)
       7000))
     ("EFnet: US, CA, San Jose" EFnet "irc.concentric.net"
      ((6665 6668)))
     ("EFnet: US, CA, San Luis Obispo" EFnet "irc.prison.net"
      ((6666 6667)))
     ("EFnet: US, GA, Atlanta" EFnet "irc.mindspring.com"
      ((6660 6669)))
     ("EFnet: US, MI, Ann Arbor" EFnet "irc.umich.edu" 6667)
     ("EFnet: US, MN, Twin Cities" EFnet "irc.umn.edu"
      ((6665 6669)))
     ("EFnet: US, NY, Mineola" EFnet "irc.lightning.net"
      ((6665 7000)))
     ("EFnet: US, NY, New York (east)" EFnet "irc.east.gblx.net" 6667)
     ("EFnet: US, NY, New York (flamed)" EFnet "irc.flamed.net"
      ((6665 6669)))
     ("EFnet: US, TX, Houston" EFnet "ircd.lagged.org"
      ((6660 6669)))
     ("EFnet: US, VA, Ashburn" EFnet "irc.secsup.uu.net"
      ((6665 6669)
       8080))
     ("EFnet: Random AU server" EFnet "au.rr.efnet.net" 6667)
     ("EFnet: Random CA server" EFnet "ca.rr.efnet.net" 6667)
     ("EFnet: Random EU server" EFnet "eu.rr.efnet.net" 6667)
     ("EFnet: Random US server" EFnet "us.rr.efnet.net" 6667)
     ("EgyptianIRC: Random server" EgyptianIRC "irc.egyptianirc.net"
      ((6667 6669)))
     ("Eircnet: Random server" Eircnet "irc.eircnet.org"
      ((6660 6669)
       7000))
     ("Eleethal: Random server" Eleethal "irc.eleethal.com"
      ((6660 6669)
       7000))
     ("EntertheGame: Random server" EntertheGame "irc.enterthegame.com"
      ((6667 6669)))
     ("EpiKnet: Random server" EpiKnet "irc.epiknet.org"
      ((6660 6669)
       7000 7001))
     ("EsperNet: Random server" EsperNet "irc.esper.net"
      (5555
       (6667 6669)))
     ("Esprit: Random server" Esprit "irc.esprit.net" 6667)
     ("euIRC: Random server" euIRC "irc.euirc.net"
      ((6665 6669)))
     ("Evilzinc: Random server" Evilzinc "irc.evilzinc.net"
      ((6660 6669)
       7000 8000))
     ("ExodusIRC: Random server" ExodusIRC "irc.exodusirc.net"
      ((6660 6669)))
     ("FDFnet: Random server" FDFnet "irc.fdfnet.net"
      ((6666 6668)
       9999))
     ("FEFnet: Random server" FEFnet "irc.fef.net" 6667)
     ("Financialchat: Random server" Financialchat "irc.financialchat.com"
      ((6667 6669)
       7000))
     ("Forestnet: Random server" Forestnet "irc.forestnet.org"
      (6667 7000))
     ("ForeverChat: Random server" ForeverChat "irc.foreverchat.net"
      ((6660 6669)
       7000))
     ("Fraggers: Random server" Fraggers "irc.fraggers.co.uk"
      ((6661 6669)
       (7000 7001)))
     ("FreedomChat: Random server" FreedomChat "chat.freedomchat.net" 6667)
     ("FreedomIRC: Random server" FreedomIRC "irc.freedomirc.net" 6667)
     ("Freenode: Random server" freenode "irc.freenode.net" 6667)
     ("Freenode: Random EU server" freenode "irc.eu.freenode.net" 6667)
     ("Freenode: Random US server" freenode "irc.us.freenode.net" 6667)
     ("FunNet: Random server" FunNet "irc.funnet.org" 6667)
     ("Galaxynet: Random server" GalaxyNet "irc.galaxynet.org"
      ((6662 6668)
       7000))
     ("Galaxynet: AU, NZ, Auckland" GalaxyNet "auckland.nz.galaxynet.org"
      ((6661 6669)))
     ("Galaxynet: EU, BE, Online" GalaxyNet "online.be.galaxynet.org"
      ((6661 6669)))
     ("Galaxynet: US, FL, Florida" GalaxyNet "gymnet.us.galaxynet.org"
      ((6661 6669)))
     ("Gamesnet: Random east US server" Gamesnet "east.gamesnet.net" 6667)
     ("Gamesnet: Random west US server" Gamesnet "west.gamesnet.net" 6667)
     ("GammaForce: Random server" GammaForce "irc.gammaforce.org"
      ((6660 6669)
       7000))
     ("GIKInet: Random server" GIKInet "irc.giki.edu.pk" 6667)
     ("GizNet: Random server" GizNet "irc.giznet.org"
      ((6666 6669)
       7000))
     ("Globalchat: Random server" Globalchat "irc.globalchat.org" 6667)
     ("GlobIRC: Random server" GlobIRC "irc.globirc.net"
      ((6666 6668)
       9999))
     ("Goldchat: Random server" Goldchat "irc.goldchat.nl"
      ((6660 6669)
       7000))
     ("Goodchatting: Random server" Goodchatting "irc.goodchatting.com"
      ((6661 6669)
       7000))
     ("GravityLords: Random server" GravityLords "irc.gravitylords.net" 6667)
     ("Grnet: Random EU server" GRnet "gr.irc.gr"
      (6667 7000))
     ("Grnet: Random server" GRnet "srv.irc.gr"
      (6667 7000))
     ("Grnet: Random US server" GRnet "us.irc.gr"
      (6667 7000))
     ("GulfChat: Random server" GulfChat "irc.gulfchat.net"
      ((6660 6669)))
     ("HabberNet: Random server" HabberNet "irc.habber.net" 6667)
     ("HanIRC: Random server" HanIRC "irc.hanirc.org" 6667)
     ("Hellenicnet: Random server" Hellenicnet "irc.mirc.gr"
      (6667 7000))
     ("IceNet: Random server" IceNet "irc.icenet.org.za" 6667)
     ("ICQnet: Random server" ICQnet "irc.icq.com" 6667)
     ("Infatech: Random server" Infatech "irc.infatech.net"
      ((6660 6669)))
     ("Infinity: Random server" Infinity "irc.infinity-irc.org" 6667)
     ("Infomatrix: Random server" Infomatrix "irc.infomatrix.net" 6667)
     ("Inside3D: Random server" Inside3D "irc.inside3d.net"
      ((6661 6669)))
     ("InterlinkChat: Random server" InterlinkChat "irc.interlinkchat.net"
      ((6660 6669)
       7000))
     ("IRC-Chile: Random server" IRC-Chile "irc.cl" 6667)
     ("IRC-Hispano: Random server" IRC-Hispano "irc.irc-hispano.org" 6667)
     ("IRCchat: Random server" IRCchat "irc.ircchat.tk" 6667)
     ("IRCGate: Random server" IRCGate "irc.ircgate.net"
      ((6667 6669)))
     ("IRCGeeks: Random server" IRCGeeks "irc.ircgeeks.org"
      ((6660 6669)))
     ("IRChat: Random server" IRChat "irc.irchat.net"
      ((6660 6669)))
     ("IrcLordz: Random server" IrcLordz "irc.irclordz.com" 6667)
     ("IrcMalta: Random server" IrcMalta "irc.ircmalta.org"
      ((6660 6667)))
     ("IRCnet: EU, FR, Random" IRCnet "irc.fr.ircnet.net" 6667)
     ("IRCnet: EU, IT, Random" IRCnet "irc.ircd.it"
      ((6665 6669)))
     ("IRCnet: AS, IL, Haifa" IRCnet "ircnet.netvision.net.il"
      ((6661 6668)))
     ("IRCnet: AS, JP, Tokyo" IRCnet "irc.tokyo.wide.ad.jp" 6667)
     ("IRCnet: AS, TW, Seed" IRCnet "irc.seed.net.tw" 6667)
     ("IRCnet: EU, AT, Linz" IRCnet "linz.irc.at"
      ((6666 6668)))
     ("IRCnet: EU, AT, Wien" IRCnet "vienna.irc.at"
      ((6666 6669)))
     ("IRCnet: EU, BE, Brussels" IRCnet "irc.belnet.be" 6667)
     ("IRCnet: EU, BE, Zaventem" IRCnet "ircnet.wanadoo.be"
      ((6661 6669)))
     ("IRCnet: EU, CZ, Prague" IRCnet "irc.felk.cvut.cz" 6667)
     ("IRCnet: EU, DE, Berlin" IRCnet "irc.fu-berlin.de"
      ((6665 6669)))
     ("IRCnet: EU, DE, Dusseldorf" IRCnet "irc.freenet.de"
      ((6665 6669)))
     ("IRCnet: EU, DE, Stuttgart" IRCnet "irc.belwue.de"
      ((6665 6669)))
     ("IRCnet: EU, DK, Copenhagen" IRCnet "irc.ircnet.dk" 6667)
     ("IRCnet: EU, EE, Tallinn" IRCnet "irc.estpak.ee"
      ((6666 6668)))
     ("IRCnet: EU, FI, Helsinki" IRCnet "irc.cs.hut.fi" 6667)
     ("IRCnet: EU, GR, Thessaloniki" IRCnet "irc.ee.auth.gr"
      ((6666 6669)))
     ("IRCnet: EU, HU, Budapest" IRCnet "irc.elte.hu" 6667)
     ("IRCnet: EU, IS, Reykjavik (ircnet)" IRCnet "irc.ircnet.is"
      ((6661 6669)))
     ("IRCnet: EU, IS, Reykjavik (simnet)" IRCnet "irc.simnet.is"
      ((6661 6669)))
     ("IRCnet: EU, IT, Rome" IRCnet "irc.tin.it"
      ((6665 6669)))
     ("IRCnet: EU, NL, Amsterdam (nlnet)" IRCnet "irc.nl.uu.net"
      ((6660 6669)))
     ("IRCnet: EU, NL, Amsterdam (xs4all)" IRCnet "irc.xs4all.nl"
      ((6660 6669)))
     ("IRCnet: EU, NL, Enschede" IRCnet "irc.snt.utwente.nl"
      ((6660 6669)))
     ("IRCnet: EU, NL, Nijmegen" IRCnet "irc.sci.kun.nl"
      ((6660 6669)))
     ("IRCnet: EU, NO, Oslo" IRCnet "irc.ifi.uio.no" 6667)
     ("IRCnet: EU, NO, Trondheim" IRCnet "irc.pvv.ntnu.no" 6667)
     ("IRCnet: EU, PL, Lublin" IRCnet "lublin.irc.pl"
      ((6666 6668)))
     ("IRCnet: EU, PL, Warsaw" IRCnet "warszawa.irc.pl"
      ((6666 6668)))
     ("IRCnet: EU, RU, Moscow" IRCnet "irc.msu.ru" 6667)
     ("IRCnet: EU, SE, Lulea" IRCnet "irc.ludd.luth.se"
      ((6661 6669)))
     ("IRCnet: EU, UK, London (Demon)" IRCnet "ircnet.demon.co.uk"
      ((6665 6669)))
     ("IRCnet: EU, UK, London (Easynet)" IRCnet "ircnet.easynet.co.uk"
      ((6666 6669)))
     ("IRCnet: US, NY, New York" IRCnet "irc.stealth.net"
      ((6660 6669)))
     ("IRCnet: Random AU server" IRCnet "au.ircnet.org" 6667)
     ("IRCnet: Random EU server" IRCnet "eu.ircnet.org"
      ((6665 6668)))
     ("IRCnet: Random US server" IRCnet "us.ircnet.org"
      ((6665 6668)))
     ("IRCSoulZ: Random server" IRCSoulZ "irc.ircsoulz.net" 6667)
     ("IRCSul: BR, PR, Maringa" IRCSul "irc.wnet.com.br" 6667)
     ("IrcTalk: Random server" IrcTalk "irc.irctalk.net"
      ((6660 6669)))
     ("Irctoo: Random server" Irctoo "irc.irctoo.net" 6667)
     ("IRCtown: Random server" IRCtown "irc.irctown.net"
      ((6666 6669)
       7000))
     ("IRCworld: Random server" IRCworld "irc.ircworld.org" 6667)
     ("ircXtreme: Random server" ircXtreme "irc.ircXtreme.net"
      ((6660 6669)))
     ("Israelnet: Random server" Israelnet "irc.israel.net" 6667)
     ("K0wNet: Random server" K0wNet "irc.k0w.net"
      ((6660 6669)))
     ("KDFSnet: Random server" KDFSnet "irc.kdfs.net"
      ((6667 6669)))
     ("Kemik: Random server" Kemik "irc.kemik.net" 6667)
     ("Kewl.Org: Random server" Kewl\.Org "irc.kewl.org"
      (6667 7000))
     ("Kickchat: Random server" Kickchat "irc.kickchat.com"
      ((6660 6669)
       7000))
     ("Kidsworld: Random server" KidsWorld "irc.kidsworld.org"
      ((6666 6669)))
     ("Knightnet: AF, ZA, Durban" Knightnet "orc.dbn.za.knightnet.net"
      (6667 5555))
     ("Knightnet: US, CA, Goldengate" Knightnet "goldengate.ca.us.knightnet.net"
      (6667 5555))
     ("Konfido.Net: Random server" Konfido\.Net "irc.konfido.net" 6667)
     ("KreyNet: Random server" Kreynet "irc.krey.net" 6667)
     ("Krono: Random server" Krono "irc.krono.net"
      ((6660 6669)
       7000))
     ("Krushnet: Random server" Krushnet "irc.krushnet.org" 6667)
     ("LagNet: Random server" LagNet "irc.lagnet.org.za" 6667)
     ("LagNet: AF, ZA, Cape Town" LagNet "reaper.lagnet.org.za" 6667)
     ("LagNet: AF, ZA, Johannesburg" LagNet "mystery.lagnet.org.za" 6667)
     ("Librenet: Random server" Librenet "irc.librenet.net" 6667)
     ("LinkNet: Random server" LinkNet "irc.link-net.org"
      ((6667 6669)))
     ("LinuxChix: Random server" LinuxChix "irc.linuxchix.org" 6667)
     ("Liquidized: Random server" Liquidized "irc.liquidized.net"
      (6667 7000))
     ("M-IRC: Random server" M-IRC "irc.m-sys.org"
      ((6667 6669)))
     ("MagicStar: Random server" MagicStar "irc.magicstar.net" 6667)
     ("Mavra: Random server" Mavra "irc.mavra.net" 6667)
     ("MediaDriven: Random server" MediaDriven "irc.mediadriven.com"
      ((6667 6669)))
     ("mIRC-X: Random server" mIRC-X "irc.mircx.com"
      (6667 7000))
     ("Morat: Random server" Morat "irc.morat.net" 6667)
     ("MusicCity: Random server" MusicCity "chat.musiccity.com" 6667)
     ("Mysteria: Random server" Mysteria "irc.mysteria.net"
      (6667 7000))
     ("Mysterychat: Random server" Mysterychat "irc.mysterychat.net"
      ((6667 6669)))
     ("Mystical: Random server" Mystical "irc.mystical.net"
      (6667 7000))
     ("Narancs: Random server" Narancs "irc.narancs.com"
      ((6667 6669)
       7000))
     ("Net-France: Random server" Net-France "irc.net-france.com" 6667)
     ("Nevernet: Random server" Nevernet "irc.nevernet.net" 6667)
     ("Newnet: Random server" Newnet "irc.newnet.net"
      ((6665 6667)))
     ("Nexusirc: Random server" Nexusirc "irc.nexusirc.org" 6667)
     ("Nightstar: Random server" NightStar "irc.nightstar.net"
      ((6665 6669)))
     ("NitrousNet: Random server" NitrousNet "irc.nitrousnet.net" 6667)
     ("Novernet: Random server" Novernet "irc.novernet.com"
      ((6665 6669)
       7000))
     ("Nullrouted: Random server" Nullrouted "irc.nullrouted.org"
      ((6666 6669)
       7000))
     ("NullusNet: Random server" NullusNet "irc.nullus.net" 6667)
     ("OFTC: Random server" OFTC "irc.oftc.net"
      ((6667 6670)
       7000))
     ("OpChat: Random server" OpChat "irc.opchat.org"
      ((6667 6669)))
     ("Othernet: Random server" Othernet "irc.othernet.org" 6667)
     ("Othernet: US, FL, Miami" Othernet "miami.fl.us.othernet.org" 6667)
     ("Othernet: US, MO, StLouis" Othernet "stlouis.mo.us.othernet.org" 6667)
     ("Otherside: Random server" OtherSide "irc.othersideirc.net" 6667)
     ("Outsiderz: Random server" Outsiderz "irc.outsiderz.com" 6667)
     ("OzOrg: AU, Perth" OzOrg "iinet.perth.oz.org" 6667)
     ("Peacefulhaven: Random server" Peacefulhaven "irc.peacefulhaven.net"
      ((6660 6669)
       7000))
     ("PhazedIRC: Random server" PhazedIRC "irc.phazedirc.net" 6667)
     ("Philchat: Random server" Philchat "irc.philchat.net" 6667)
     ("phrozN: Random server" phrozN "irc.phrozn.net" 6667)
     ("PiNet: Random server" PiNet "irc.praetorians.org"
      ((6665 6669)))
     ("Pinoycentral: Random server" Pinoycentral "chat.abs-cbn.com" 6667)
     ("Planetarion: Random server" Planetarion "irc.planetarion.com" 6667)
     ("POLNet: Random server" POLNet "irc.ircnet.pl" 6667)
     ("Psionics: CA, PQ, Montreal" Psionics "chat.psionics.net"
      ((6660 6669)))
     ("PTirc: Random server" PTirc "irc.ptirc.com.pt" 6667)
     ("PTlink: Random server" PTlink "irc.ptlink.net" 6667)
     ("PTnet: Random server" PTnet "irc.ptnet.org" 6667)
     ("QChat: Random server" QChat "irc.qchat.net" 6667)
     ("QuakeNet: Random German server" QuakeNet "de.quakenet.org"
      ((6667 6669)))
     ("QuakeNet: Random server" QuakeNet "irc.quakenet.eu.org"
      ((6667 6669)))
     ("QuakeNet: Random Swedish server" QuakeNet "se.quakenet.org"
      ((6667 6669)))
     ("QuakeNet: Random UK server" QuakeNet "uk.quakenet.org"
      ((6667 6669)))
     ("QuakeNet: Random US server" QuakeNet "us.quakenet.org"
      ((6667 6669)))
     ("Realirc: Random server" Realirc "irc.realirc.org" 6667)
     ("RealmNET: Random server" RealmNET "irc.realmnet.com" 6667)
     ("Rebelchat: Random server" Rebelchat "irc.rebelchat.org" 6667)
     ("Red-Latina: Random server" Red-Latina "irc.red-latina.org" 6667)
     ("RedLatona: Random server" RedLatona "irc.redlatona.net"
      (6667 6668))
     ("Relicnet: Random server" Relicnet "irc.relic.net" 6667)
     ("Rezosup: Random server" Rezosup "irc.rezosup.org" 6667)
     ("Risanet: Random server" Risanet "irc.risanet.com"
      ((6667 6669)))
     ("Rizon: Random server" Rizon "irc.rizon.net"
      (6633
       (6660 6669)
       6697 7000 8080 9999))
     ("Rubiks: Random server" Rubiks "irc.rubiks.net" 6667)
     ("Rusnet: EU, RU, Tomsk" Rusnet "irc.tsk.ru"
      ((6667 6669)
       (7770 7775)))
     ("Rusnet: EU, RU, Vladivostok" Rusnet "irc.vladivostok.ru"
      ((6667 6669)
       (7770 7775)))
     ("Rusnet: EU, UA, Kiev" Rusnet "irc.kar.net"
      ((6667 6669)
       (7770 7775)))
     ("Sandnet: Random server" Sandnet "irc.sandnet.net"
      ((6660 6669)
       7000))
     ("Scunc: Random server" Scunc "irc.scunc.net" 6667)
     ("SerbianCafe: Random server" SerbianCafe "irc.serbiancafe.ws"
      ((6665 6669)))
     ("SexNet: Random server" SexNet "irc.sexnet.org" 6667)
     ("ShadowFire: Random server" ShadowFire "irc.shadowfire.org" 6667)
     ("ShadowWorld: Random server" ShadowWorld "irc.shadowworld.net" 6667)
     ("SkyNet: Random server" SkyNet "irc.bronowski.pl"
      ((6666 6668)))
     ("Slashnet: Random server" Slashnet "irc.slashnet.org" 6667)
     ("SolarStone: Random server" SolarStone "irc.solarstone.net"
      ((6660 6669)))
     ("Sorcerynet: Random server" Sorcery "irc.sorcery.net"
      (6667 7000 9000))
     ("Sorcerynet: EU, SE, Karlskrona" Sorcery "nexus.sorcery.net"
      (6667 7000 9000))
     ("Sorcerynet: US, CA, Palo Alto" Sorcery "kechara.sorcery.net"
      (6667 7000 9000))
     ("SourceIRC: Random server" SourceIRC "irc.sourceirc.net"
      ((6667 6669)
       7000))
     ("SpaceTronix: Random server" SpaceTronix "irc.spacetronix.net"
      ((6660 6669)
       7000))
     ("Spirit-Harmony: Random server" Spirit-Harmony "irc.spirit-harmony.com"
      ((6661 6669)))
     ("StarChat: Random server" StarChat "irc.starchat.net"
      ((6667 6669)
       7000))
     ("StarEquinox: Random server" StarEquinox "irc.starequinox.net"
      ((6660 6669)))
     ("StarLink: Random server" Starlink "irc.starlink.net"
      ((6660 6669)))
     ("StarLink-irc: Random server" starlink-irc "irc.starlink-irc.org" 6667)
     ("StarWars-IRC: Random server" StarWars-IRC "irc.starwars-irc.net"
      ((6663 6667)))
     ("Stormdancing: Random server" Stormdancing "irc.stormdancing.net"
      ((6664 6669)
       7000 9000))
     ("Superchat: Random server" Superchat "irc.superchat.org"
      ((6660 6668)))
     ("SUSE" SUSE "irc.suse.de" 6667)
     ("Sysopnet: Random server" Sysopnet "irc.sysopnet.org"
      ((6666 6668)))
     ("Telstra: Random server" Telstra "irc.telstra.com"
      ((6667 6669)))
     ("TR-net: EU, TR, Ankara" TR-net "irc.dominet.com.tr" 6667)
     ("TR-net: EU, Tr, Istanbul" TR-net "irc.teklan.com.tr" 6667)
     ("Tri-net: Random server" Tri-net "irc.tri-net.org" 6667)
     ("TriLink: Random server" TriLink "irc.ft4u.net" 6667)
     ("TurkishChat: Random server" TurkishChat "irc.turkishchat.org"
      ((6660 6669)
       7000))
     ("UberNinja: Random server" UberNinja "irc.uberninja.net"
      ((6667 6669)))
     ("UICN: Random server" UICN "irc.uicn.net" 6667)
     ("UltraIRC: Random server" UltraIRC "irc.ultrairc.net" 6667)
     ("UnderChat: Random server" UnderChat "irc.underchat.it"
      ((6660 6669)
       7000))
     ("Undernet: CA, ON, Toronto" Undernet "toronto.on.ca.undernet.org"
      ((6661 6669)))
     ("Undernet: CA, QC, Montreal" Undernet "montreal.qu.ca.undernet.org"
      ((6660 6669)))
     ("Undernet: EU, AT, Graz" Undernet "graz.at.eu.undernet.org"
      ((6661 6669)))
     ("Undernet: EU, BE, Antwerp" Undernet "flanders.be.eu.undernet.org"
      ((6660 6669)))
     ("Undernet: EU, BE, Brussels" Undernet "brussels.be.eu.undernet.org" 6667)
     ("Undernet: EU, CH, Geneva" Undernet "geneva.ch.eu.undernet.org"
      ((6660 6669)
       7777 8000))
     ("Undernet: EU, FR, Caen" Undernet "caen.fr.eu.undernet.org"
      ((6666 6669)))
     ("Undernet: EU, NL, Diemen" Undernet "diemen.nl.eu.undernet.org"
      ((6660 6669)))
     ("Undernet: EU, NL, Haarlem" Undernet "haarlem.nl.eu.undernet.org"
      ((6660 6669)))
     ("Undernet: EU, NO, Oslo" Undernet "oslo.no.eu.undernet.org"
      ((6660 6669)))
     ("Undernet: EU, SE, Stockholm" Undernet "stockholm.se.eu.undernet.org"
      ((6666 6669)))
     ("Undernet: EU, UK, Surrey" Undernet "surrey.uk.eu.undernet.org"
      ((6660 6669)))
     ("Undernet: US, AZ, Mesa" Undernet "mesa.az.us.undernet.org"
      ((6665 6667)))
     ("Undernet: US, CA, San Diego" Undernet "sandiego.ca.us.undernet.org"
      ((6660 6670)))
     ("Undernet: US, DC, Washington" Undernet "washington.dc.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, KS, Manhattan" Undernet "manhattan.ks.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, NV, Las Vegas" Undernet "lasvegas.nv.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, TX, Austin" Undernet "austin.tx.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, UT, Saltlake" Undernet "saltlake.ut.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, VA, Arlington" Undernet "arlington.va.us.undernet.org"
      ((6660 6669)))
     ("Undernet: US, VA, McLean" Undernet "mclean.va.us.undernet.org"
      ((6666 6669)))
     ("Undernet: Random EU server" Undernet "eu.undernet.org" 6667)
     ("Undernet: Random US server" Undernet "us.undernet.org" 6667)
     ("UnderZ: Random server" UnderZ "irc.underz.org"
      ((6667 6668)))
     ("UniChat: Random server" UniChat "irc.uni-chat.net" 6667)
     ("UnionLatina: Random server" UnionLatina "irc.unionlatina.org" 6667)
     ("Univers: Random server" Univers "irc.univers.org"
      ((6665 6669)))
     ("UnixR: Random server" UnixR "irc.unixr.net"
      ((6667 6669)))
     ("Vidgamechat: Random server" Vidgamechat "irc.vidgamechat.com" 6667)
     ("VirtuaNet: Random server" VirtuaNet "irc.virtuanet.org"
      ((6660 6669)
       7000))
     ("Vitamina: Random server" Vitamina "irc.vitamina.ca" 6667)
     ("Voila: Random server" Voila "irc.voila.fr" 6667)
     ("Wahou: Random server" Wahou "irc.wahou.org"
      ((6665 6669)))
     ("Warpednet: Random server" Warpednet "irc.warped.net" 6667)
     ("Weaklinks: Random server" Weaklinks "irc.weaklinks.net"
      ((6667 6669)))
     ("Webnet: Random server" Webnet "irc.webchat.org"
      ((6667 6669)
       7000))
     ("Webnet: US, CA, Santa Clara" Webnet "webmaster.ca.us.webchat.org"
      ((6661 6669)))
     ("WinChat: Random server" WinChat "irc.winchat.net"
      ((6661 6669)))
     ("WinIRC: Random server" WinIRC "irc.winirc.org"
      ((6667 6669)
       4400))
     ("WorldIRC: Random server" WorldIRC "irc.worldirc.org"
      ((6660 6667)))
     ("WyldRyde: Random server" WyldRyde "irc.wyldryde.net"
      ((6666 6669)))
     ("XentoniX: Random server" XentoniX "irc.xentonix.net"
      ((6661 6669)))
     ("Xevion: Random server" Xevion "irc.xevion.net"
      (6667 7000))
     ("XNet: Random server" XNet "irc.xnet.org" 6667)
     ("XWorld: Random server" XWorld "irc.xworld.org" 6667)
     ("ZAnet Net: Random server" ZAnetNet "irc.zanet.net" 6667)
     ("ZAnet Org: UK, London" ZAnetOrg "mystic.zanet.org.za" 6667)
     ("ZiRC: Random server" ZiRC "irc.zirc.org"
      ((6660 6669)))
     ("ZUHnet: Random server" ZUHnet "irc.zuh.net" 6667)
     ("Zurna: Random server" Zurna "irc.zurna.net" 6667))))
 '(erc-truncate-buffer-on-save t)
 '(erc-user-full-name "Adam Spiers")
 '(fast-lock-cache-directories (quote ("~/.emacs-flc")))
 '(fast-lock-minimum-size 4096)
 '(fci-rule-color "gray32")
 '(fci-rule-column 80)
 '(flx-ido-mode t)
 '(flx-ido-threshhold 60000)
 '(folding-mode-prefix-key "")
 '(gc-cons-threshold 200000)
 '(gdb-many-windows t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)))
 '(git-commit-without-user-email nil)
 '(git-gutter:lighter "")
 '(git-rebase-auto-advance t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-msf-abbrev-mode t)
 '(global-whitespace-mode t)
 '(gnus-asynchronous t)
 '(gnus-cache-active-file "~/.gnus/cache/active")
 '(gnus-cache-directory "~/.gnus/cache/")
 '(gnus-expert-user t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-group-line-format "%M%S%p%P%3y: %(%g%)%l
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode)))
 '(gnus-group-sort-function (quote gnus-group-sort-by-rank))
 '(gnus-novice-user nil)
 '(gnus-secondary-servers (quote ("news.linuxprinting.org")))
 '(gnus-startup-hook
   (quote
    ((lambda nil
       (if
           (<=
            (frame-width)
            80)
           (gnus-add-configuration
            (quote
             (article
              (vertical 1.0
                        (horizontal 0.25
                                    (summary 0.75 point)
                                    (tree 1.0))
                        (article 1.0))))))))))
 '(gnus-summary-exit-hook (quote (gnus-summary-bubble-group)))
 '(gnus-summary-line-format "%U%R%z%4L:%([%1{%-15,15n%}]%) %2t %3{%B%}%2{%s%}
")
 '(gnus-suppress-duplicates t)
 '(gnus-use-trees t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
    ("C-x 4" "C-x r" "C-x v" "C-x 8" "C-x p" "C-c" "C-c i" "C-c g" "C-c m" "C-c t" "C-c C-v" "C-c w" "C-c j" "C-S-SPC" "M-o" "zp" "pz" "<key-chord>"
     (dired-mode "%" "*" "C-t"))))
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(ido-auto-merge-delay-time 3)
 '(ido-auto-merge-work-directories-length 0)
 '(ido-case-fold nil)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-everywhere t)
 '(ido-hacks-mode t)
 '(ido-max-directory-size 100000)
 '(ido-max-prompt-path 0.8)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(ido-vertical-mode t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(iswitchb-case nil)
 '(iswitchb-default-method (quote samewindow))
 '(jit-lock-stealth-nice 0.1)
 '(jit-lock-stealth-time 1)
 '(kept-old-versions 0)
 '(kill-whole-line t)
 '(lazy-lock-defer-on-scrolling t)
 '(lazy-lock-defer-time 0.1)
 '(lazy-lock-minimum-size 4096)
 '(lazy-lock-stealth-time 15)
 '(lazy-lock-stealth-verbose t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
 '(magit-log-auto-more t)
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-push-always-verify t)
 '(magit-remote-ref-format (quote remote-slash-branch))
 '(magit-repo-dirs
   (quote
    ("/home/adam/.GIT" "/home/adam/SUSE/git" "/home/adam/SUSE/cloud/git" "/home/adam/SUSE/cloud/chef/git" "/home/adam/SUSE/cloud/OpenStack/git")))
 '(magit-rewrite-inclusive t)
 '(magit-stage-all-confirm nil)
 '(magit-status-buffer-switch-function (quote switch-to-buffer))
 '(magit-status-headers-hook
   (quote
    (magit-insert-diff-filter-header magit-insert-repo-header magit-insert-remote-header magit-insert-head-header magit-insert-upstream-header magit-insert-tags-header)))
 '(magit-unstage-all-confirm nil)
 '(mail-envelope-from (quote header))
 '(mail-self-blind t)
 '(make-backup-file-name-function (quote as-make-backup-file-name))
 '(mark-even-if-inactive t)
 '(message-default-news-headers
   "From: Adam Spiers <usenet@adamspiers.org>
Reply-To: Adam Spiers <usenet@adamspiers.org>
")
 '(message-log-max 1000)
 '(message-sendmail-f-is-evil t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-yank-at-point t)
 '(msf-abbrev-indent-after-expansion t)
 '(muse-colors-autogen-headings (quote outline))
 '(muse-mode-auto-p nil)
 '(muse-wiki-allow-nonexistent-wikiword t)
 '(muse-wiki-wikiword-regexp "\\<\\([A-Z]+[a-z]+[A-Z]+[a-zA-Z0-9]*+\\)")
 '(mwheel-follow-mouse t)
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-custom-commands
   (quote
    (("b" "bandhand TODOs" alltodo ""
      ((org-agenda-files
        (quote
         ("~/eventbook/design.org")))))
     ("d" "daily review"
      ((tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs"
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))))
     ("pd" "personal daily review"
      ((tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs+CATEGORY=\"personal\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "personal"))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))
       (org-agenda-prefix-format " %?-12t% s")))
     ("wd" "SUSE daily review"
      ((tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #A TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#A\\]"
                       (quote scheduled))))))
       (tags-todo "officehrs+CATEGORY=\"SUSE\""
                  ((org-agenda-overriding-header "Unscheduled [#AB] TODOs within office hours")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#[AB]\\]"
                       (quote scheduled))))))
       (agenda ""
               ((org-agenda-ndays 3)
                (org-agenda-skip-function
                 (as-org-agenda-skip-select-category-function "SUSE"))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (lambda nil
          (and nil
               (org-agenda-skip-entry-if
                (quote deadline)
                (quote scheduled)))))
       (org-agenda-prefix-format " %?-12t% s")))
     ("7" "weekly review"
      ((todo "CHASE"
             ((org-agenda-overriding-header "Items to CHASE")))
       (todo "WAITING"
             ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck "" nil)
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t))
      nil)
     ("p7" "personal weekly review"
      ((tags-todo "+CATEGORY=\"personal\"/CHASE"
                  ((org-agenda-overriding-header "Items to CHASE")))
       (tags-todo "+CATEGORY=\"personal\"/WAITING"
                  ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck personal projects:")
               (org-stuck-projects
                (quote
                 ("+CATEGORY=\"personal\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil "")))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "+CATEGORY=\"personal\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-skip-function
        (as-org-agenda-skip-select-category-function "personal"))
       (org-agenda-prefix-format " %?-12t% s"))
      nil)
     ("w7" "SUSE weekly review"
      ((tags-todo "+CATEGORY=\"SUSE\"/CHASE"
                  ((org-agenda-overriding-header "Items to CHASE")))
       (tags-todo "+CATEGORY=\"SUSE\"/WAITING"
                  ((org-agenda-overriding-header "Items still WAITING on somebody")))
       (stuck ""
              ((org-agenda-overriding-header "Stuck work projects")
               (org-stuck-projects
                (quote
                 ("+CATEGORY=\"SUSE\"/PROJECT"
                  ("TODO" "NEXT" "NEXTACTION" "STARTED")
                  nil "")))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #B TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#B\\]"
                       (quote scheduled))))))
       (tags-todo "+CATEGORY=\"SUSE\"/NEXT|STARTED"
                  ((org-agenda-overriding-header "Unscheduled #C TODOs")
                   (org-agenda-skip-function
                    (lambda nil
                      (org-agenda-skip-entry-if
                       (quote notregexp)
                       "\\=.*\\[#C\\]"
                       (quote scheduled)))))))
      ((org-agenda-compact-blocks t)
       (org-agenda-prefix-format " %?-12t% s"))
      nil)
     ("w" . "work TODOs")
     ("p" . "personal TODOs")
     ("@" . "TODOs by context")
     ("t" . "TODOs by time constraint")
     ("s" . "TODOs by ETC")
     ("#" . "TODOs by priority")
     ("P" "stuck projects" stuck "" nil)
     ("# " "missing priorities" tags-todo "/-PROJECT-SOMEDAY-MAYBE"
      ((org-agenda-overriding-header "TODOs missing priorities")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote regexp)
           "\\=.*\\[#[A-Z]\\]")))))
     ("s " "missing time estimates" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing time estimate")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote regexp)
           ":sub")))))
     ("@ " "missing contexts" tags-todo "/NEXT|STARTED"
      ((org-agenda-overriding-header "TODOs missing context")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote regexp)
           ":@[a-zA-Z]")))))
     ("#a" "priority #A tasks" tags ""
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))))
     ("#A" "priority #A NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #A TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))))
     ("#b" "priority #B tasks" tags ""
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#B\\]")))))
     ("#B" "priority #B NEXT actions" tags "/PROJECT|NEXT"
      ((org-agenda-overriding-header "priority #B TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#B\\]")))))
     ("#c" "priority #C tasks" tags ""
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#C\\]")))))
     ("#C" "priority #C NEXT actions" tags "/PROJECT|NEXT|STARTED"
      ((org-agenda-overriding-header "priority #C TODOs")
       (org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#C\\]")))))
     ("s1" "" tags "sub10" nil)
     ("s2" "" tags "sub120" nil)
     ("s3" "" tags "sub30" nil)
     ("s4" "" tags "sub4" nil)
     ("s6" "" tags "sub60" nil)
     ("sd" "" tags "subday" nil)
     ("tO" "within office hours" tags-todo "officehrs" nil)
     ("tS" "Saturday" tags-todo "Saturday" nil)
     ("@h" "at home" tags-todo "@home|@internet|@offline|@phone"
      ((org-agenda-overriding-header "at home")))
     ("@B" "in Bracknell office" tags-todo "@Bracknell" nil)
     ("@C" "in Canary Wharf" tags-todo "@CanaryWharf" nil)
     ("@L" "in London" tags-todo "@London" nil)
     ("@?" "elsewhere" tags-todo "-@Bracknell-@London-@CanaryWharf-@phone-@internet-@offline"
      ((org-agenda-overriding-header "elsewhere")))
     ("@i" "internet (online)" tags-todo "@internet" nil)
     ("@0" "offline (but at a computer)" tags-todo "@offline" nil)
     ("@p" "can make phone calls" tags-todo "@phone" nil)
     ("@." "current context"
      (lambda
        (a)
        (error "Not implemented yet"))
      "" nil)
     ("-" "easy" tags-todo "easy" nil)
     ("p-" "easy personal tasks" tags-todo "+easy+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("w-" "easy SUSE tasks" tags-todo "+easy+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("pa" "personal admin" tags-todo "+admin+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("po" "personal organisation" tags-todo "+admin+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pc" "personal computer" tags-todo "+computer+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pF" "personal F/OSS" tags-todo "+FOSS+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("p$" "personal finance" tags-todo "+finance+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pH" "personal homeimprovement" tags-todo "+homeimprovement+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pf" "personal fun" tags-todo "+fun+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pm" "personal music" tags-todo "+music+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pR" "personal OWRA" tags-todo "+OWRA"
      ((org-agenda-prefix-format "")))
     ("ps" "personal social" tags-todo "+social+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pt" "personal training" tags-todo "+training+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("pw" "personal welfare" tags-todo "+welfare+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("p*" "personal community" tags-todo "+community+CATEGORY=\"personal\""
      ((org-agenda-prefix-format "")))
     ("wa" "SUSE admin" tags-todo "+admin+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wo" "SUSE org" tags-todo "+org+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wc" "SUSE computer" tags-todo "+computer+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("wL" "SUSE learning" tags-todo "+learning+CATEGORY=\"SUSE\""
      ((org-agenda-prefix-format "")))
     ("c" "CHASE" todo "CHASE" nil)
     ("W" "WAITING" todo "WAITING" nil)
     ("A" "admin" tags-todo "admin" nil)
     ("z" "personal agenda" agenda "CATEGORY=\"personal\"" nil)
     ("o" "org" tags-todo "org" nil)
     ("pl" "personal log" agenda "DONE"
      ((org-agenda-files
        (quote
         ("~/org/TODO.org" "~/org/DONE.org" "~/eventbook/design.org")))
       (org-agenda-span
        (quote week))
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        (quote
         (closed clock)))
       (org-agenda-entry-types
        (quote
         (:timestamp :sexp)))
       (org-agenda-overriding-header "")))
     ("wl" "work log" agenda "DONE"
      ((org-agenda-files
        (quote
         ("~/SUSE/TODO.org" "~/SUSE/DONE.org")))
       (org-agenda-span
        (quote week))
       (org-agenda-start-on-weekday 1)
       (org-agenda-include-diary t)
       (org-agenda-overriding-header "")
       (org-agenda-start-with-log-mode
        (quote
         (closed clock)))
       (org-agenda-entry-types
        (quote
         (:timestamp :sexp)))
       (org-agenda-overriding-header "")
       (org-agenda-prefix-format "  - "))))))
 '(org-agenda-deadline-leaders (quote ("Deadline: " "In %3dd: ")))
 '(org-agenda-files (quote ("~/SUSE/TODO.org" "~/org/TODO.org")))
 '(org-agenda-fontify-priorities (quote ((65 (:bold t :weight bold)))))
 '(org-agenda-include-deadlines t)
 '(org-agenda-include-diary t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-9:c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-9:c")
     (tags . "  %-9:c"))))
 '(org-agenda-scheduled-leaders (quote ("Sched: " "Sched.%2dx: ")))
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown (quote not-today))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up priority-down category-keep effort-up)
     (todo priority-down category-keep effort-up)
     (tags priority-down category-keep effort-up)
     (search category-keep))))
 '(org-agenda-start-with-follow-mode nil)
 '(org-agenda-sticky t)
 '(org-agenda-time-grid
   (quote
    (nil "----------------"
         (800 1000 1200 1400 1600 1800 2000))))
 '(org-agenda-use-time-grid nil)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.6)))
 '(org-agenda-window-setup (quote current-window))
 '(org-archive-mark-done t)
 '(org-archive-save-context-info
   (quote
    (time file category todo priority itags olpath ltags)))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (lilypond . t)
     (ruby . t)
     (python . t)
     (shell . t))))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
 '(org-capture-templates
   (quote
    (("t" "Invisalign" entry
      (file+headline "~/org/notes/Invisalign.org" "journal")
      "*** %t
***** %?" :prepend t :immediate-finish t)
     ("o" "org mailing list item" entry
      (file+headline "~/org/TODO.org" "to list")
      "* NEXT %?" :prepend t)
     ("z" "property test" entry
      (file "~/org/TODO.org")
      "%^{Effort}p" :prepend t)
     ("i" "immediate personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?
  SCHEDULED: %T" :prepend t :jump-to-captured t :clock-in t)
     ("I" "immediate work NEXT" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?
  SCHEDULED: %T" :prepend t :jump-to-captured t :clock-in t)
     ("n" "personal NEXT" entry
      (file "~/org/TODO.org")
      "* NEXT %?" :prepend t)
     ("N" "work NEXT" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?" :prepend t)
     ("m" "NEXT from personal mail" entry
      (file "~/org/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t)
     ("M" "NEXT from work mail" entry
      (file "~/SUSE/TODO.org")
      "* NEXT %?%[~/.org-mairix-link]" :prepend t)
     ("a" "personal diary entry" entry
      (file "~/org/diary.org")
      "* %^t %?%[~/.org-mairix-link]" :prepend t)
     ("L" "work learning material" entry
      (file "~/SUSE/TODO.org")
      "* SOMEDAY %?%[~/.org-mairix-link]	:learning:" :prepend t)
     ("d" "personal task DONE" entry
      (file "~/org/DONE.org")
      "* DONE %?
  CLOSED: %U")
     ("D" "work task DONE" entry
      (file "~/SUSE/DONE.org")
      "* DONE %?
  CLOSED: %U")
     ("X" "nuisance phone call" entry
      (file "~/org/notes/NuisanceCalls.org")
      "* %T %?")
     ("w" "Wipfel learning" entry
      (file+headline "~/SUSE/TODO.org" "PROJECT rwipfel")
      "* SOMEDAY %[~/.org-mairix-link]" :prepend t)
     ("S" "PSO standup calls etc." entry
      (file+headline "~/SUSE/notes/PSO.org" "logs of stand-up calls and sprint planning")
      "*** %t
***** me
******* %?" :prepend t)
     ("p" "project" entry
      (file "~/org/TODO.org")
      "* PROJECT %^{project title}
*** why
    - %?
    - 
    - 
    - 
    - 
*** outcome
*** brainstorming
***** Who?
***** What?
***** When?
***** Where?
***** Why?
***** How?" :prepend t)
     ("P" "Procrastination" entry
      (file "~/org/notes/ProcrastinationLog.org")
      "* %T %^{activity}
  :PROPERTIES:
  :thoughts/feelings: %^{thoughts/feelings}
  :justification: %^{justification}
  :attempted solution: %^{attempted solution}
  :resultant thoughts/feelings: %^{resultant thoughts/feelings}
  :END:"))))
 '(org-catch-invisible-edits (quote smart))
 '(org-clock-idle-time 5)
 '(org-clock-in-switch-to-state (quote as-org-clock-in-switch-to-state))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-persist t)
 '(org-clock-persist-query-save t)
 '(org-clock-sound "~/lib/emacs/utils/org-clock-sound-quiet.wav")
 '(org-clone-delete-id t)
 '(org-columns-default-format
   "%TODO %PRIORITY %40ITEM(Task) %Effort(ETC){:} %CLOCKSUM(Taken){:} %TAGS(Tags)")
 '(org-combined-agenda-icalendar-file "~/SUSE/org.ics")
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-deadline-warning-days 3)
 '(org-default-extensions nil t)
 '(org-default-notes-file "~/org/TODO.org")
 '(org-directory "~/org")
 '(org-disputed-keys
   (quote
    (([(control shift right)]
      .
      [(control shift n)])
     ([(control shift left)]
      .
      [(control shift p)])
     ([(control 44)]
      .
      [(control 39)])
     ([(control tab)]
      .
      [(control meta tab)]))))
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "HIDE" "STATE")))
 '(org-email-link-description-format "mail %c: %.30s")
 '(org-emphasis-regexp-components (quote (" 	('\"" "- 	.,:?;'\")" " 	
,\"'" "." 5)) t)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-html-style
   "<style type=\"text/css\">
  html {
	font-family: Times, serif;
	font-size: 12pt;
  }
  .title { text-align: center; }
  .initialtext {
        text-align: center;
        font-size: 16pt;
  }
  .todo  { color: red; }
  .done { color: green; }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  .tag { background-color:lightblue; font-weight:normal }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	<!--border: 1pt solid #ADB9CC;-->
  }
</style>")
 '(org-extend-today-until 3)
 '(org-from-is-user-regexp
   "\\<\\(adam@spiers\\.net\\|Adam Spiers\\|@\\(adamspiers\\|tigerpig\\)\\.org\\|aspiers@\\(novell\\|suse\\)\\.com\\)\\>")
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00 16:00 0"))))
 '(org-goto-interface (quote outline))
 '(org-goto-max-level 7)
 '(org-hide-leading-stars t)
 '(org-icalendar-store-UID t)
 '(org-icalendar-timezone "Europe/London")
 '(org-link-abbrev-alist
   (quote
    (("bnc" . "https://bugzilla.novell.com/show_bug.cgi?id=")
     ("bug" . "https://bugzilla.novell.com/show_bug.cgi?id=")
     ("psorev" . "https://svn.innerweb.novell.com/viewsvn/pso-source?view=rev&revision="))))
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (gnus . gnus-other-frame)
     (file . find-file))))
 '(org-log-done (quote time))
 '(org-lowest-priority 69)
 '(org-mairix-augmented-links nil)
 '(org-mairix-display-hook (quote org-mairix-mutt-display-results))
 '(org-mairix-mutt-display-command "mairix-profile --view novell %search% &")
 '(org-mairix-open-command "mairix-profile novell %args% '%search%'")
 '(org-mairix-threaded-links nil)
 '(org-mobile-agendas (quote ("d" "pd" "wd" "7" "p7" "w7" "c" "W" "e")))
 '(org-mobile-directory "/scpx:adamspiers.org:org/")
 '(org-mobile-files (quote ("~/org/mobile")))
 '(org-mobile-inbox-for-pull "~/org/org-mobile-incoming.org")
 '(org-modules
   (quote
    (org-habit org-id org-info org-mouse org-protocol org-w3m org-man org-timer org-docview)))
 '(org-odd-levels-only t)
 '(org-priority-faces (quote ((65 :weight bold))))
 '(org-publish-project-alist
   (quote
    (("OWRA" :components
      ("OWRA-2008" "OWRA-2009" "OWRA-2010" "OWRA-2011" "OWRA-2012" "OWRA-2013"))
     ("OWRA-2008" :base-directory "~/OWRA/meetings/2008" :publishing-directory "~/OWRA/meetings/2008" :publishing-function org-html-publish-to-html)
     ("OWRA-2009" :base-directory "~/OWRA/meetings/2009" :publishing-directory "~/OWRA/meetings/2009" :publishing-function org-html-publish-to-html)
     ("OWRA-2010" :base-directory "~/OWRA/meetings/2010" :publishing-directory "~/OWRA/meetings/2010" :publishing-function org-html-publish-to-html)
     ("OWRA-2011" :base-directory "~/OWRA/meetings/2011" :publishing-directory "~/OWRA/meetings/2011" :publishing-function org-html-publish-to-html)
     ("OWRA-2012" :base-directory "~/OWRA/meetings/2012" :publishing-directory "~/OWRA/meetings/2012" :publishing-function org-html-publish-to-html)
     ("OWRA-2013" :base-directory "~/OWRA/meetings/2013" :publishing-directory "~/OWRA/meetings/2013" :publishing-function org-html-publish-to-html)
     ("OWRA-2014" :base-directory "~/OWRA/meetings/2014" :publishing-directory "~/OWRA/meetings/2014" :publishing-function org-html-publish-to-html)
     ("OWRA-2015" :base-directory "~/OWRA/meetings/2015" :publishing-directory "~/OWRA/meetings/2015" :publishing-function org-html-publish-to-html))))
 '(org-refile-targets (quote ((nil :maxlevel . 3))))
 '(org-refile-use-outline-path t)
 '(org-replace-disputed-keys t)
 '(org-return-follows-link t)
 '(org-reverse-note-order t)
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-stuck-projects
   (quote
    ("/PROJECT"
     ("TODO" "NEXT" "NEXTACTION" "STARTED")
     nil "")))
 '(org-subheading-todo-alist (quote (("PROJECT" . "NEXT") ("NEXT" . "NEXT"))))
 '(org-tags-column -78)
 '(org-tags-match-list-sublevels t)
 '(org-time-stamp-rounding-minutes (quote (15 5)))
 '(org-todo-interpretation (quote type))
 '(org-todo-keyword-faces
   (quote
    (("STARTED" :foreground "LimeGreen" :weight bold)
     ("ONGOING" :foreground "DarkOrange" :weight bold)
     ("CHASE" :background "orange red" :weight bold)
     ("WAITING" :foreground "#ffe000" :weight bold)
     ("PROJECT" :foreground "purple1" :weight bold)
     ("SOMEDAY" :foreground "gray70" :weight bold)
     ("MAYBE" :foreground "gray55" :weight bold)
     ("CANCELLED" :foreground "gray35" :strike-through t)
     ("BUG" :foreground "red" :weight bold)
     ("WORKAROUND" :foreground "dark magenta" :weight bold)
     ("USABILITY" :foreground "medium sea green" :weight bold)
     ("HOWTO" :foreground "slate blue" :weight bold)
     ("IGNORE" :foreground "slate grey" :strike-through t))))
 '(org-todo-keywords
   (quote
    ((sequence "NEXT(n)" "STARTED(>)" "|" "DONE(d)")
     (sequence "PROJECT(p)" "PROJDONE(P)")
     (sequence "ONGOING(o)" "WAITING(w@)" "CHASE(C@)" "|")
     (sequence "SOMEDAY(s)" "MAYBE(m)" "|" "CANCELLED(c@)"))))
 '(org-use-extra-keys t)
 '(org-use-fast-todo-selection t)
 '(org-use-property-inheritance (quote ("CRYPTKEY" "CATEGORY")))
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts (quote {}))
 '(org-yank-adjusted-subtrees t)
 '(org2blog/wp-blog-alist
   (quote
    (("blog.adamspiers.org" :url "http://blog.adamspiers.org/xmlrpc.php" :username "adam"))))
 '(org2blog/wp-image-thumbnails nil)
 '(org2blog/wp-sourcecode-default-params "")
 '(org2blog/wp-use-sourcecode-shortcode t)
 '(outline-auto-activation t)
 '(package-archives
   (quote
    (("ELPA" . "http://tromey.com/elpa/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("MELPA" . "http://melpa.milkbox.net/packages/")
     ("SC" . "http://joseito.republika.pl/sunrise-commander/"))))
 '(passive-voice nil)
 '(planner-use-day-pages t)
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-keymap-prefix "^Xp")
 '(projectile-switch-project-action (quote projectile-vc))
 '(projectile-use-git-grep t)
 '(ps-lpr-command "kprinter")
 '(ps-paper-type (quote a4))
 '(ps-print-color-p (quote black-white))
 '(quelpa-upgrade-p t)
 '(region-bindings-mode-disable-predicates (quote ((lambda nil buffer-read-only))))
 '(region-bindings-mode-enabled-modes
   (quote
    (c-mode shell-script-mode emacs-lisp-mode ruby-mode python-mode org-mode)))
 '(require-final-newline nil)
 '(rm-blacklist
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit" " Golden")))
 '(rm-excluded-modes
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit")))
 '(rst-toc-insert-number-separator ". ")
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(safe-local-variable-values
   (quote
    ((org-drawers quote
                  ("PROPERTIES" "HIDE"))
     (byte-compile-warnings redefine callargs free-vars unresolved obsolete noruntime)
     (auto-recompile))))
 '(save-abbrevs (quote silently))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 10)
 '(scroll-margin 20)
 '(scroll-preserve-screen-position t)
 '(search-upper-case t)
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "msmtp-personal")
 '(show-paren-delay 0)
 '(show-paren-mode t nil (paren))
 '(show-paren-ring-bell-on-mismatch nil)
 '(show-trailing-whitespace nil)
 '(smartparens-global-mode nil)
 '(sml/active-background-color "grey39")
 '(sml/active-foreground-color "black")
 '(sml/hidden-modes
   (quote
    (" hl-p" " WS" " ws" " Guide" " SP" " Flymake" " Projectile" " Projectile[lisp]" " Projectile[smart-mode-line]" " Projectile\\(\\[[^]]+\\]\\)?" " All" " Paredit")))
 '(sml/inactive-background-color "gray20")
 '(sml/line-number-format "%4l")
 '(sml/mode-width (quote full))
 '(sml/name-width (quote (20 . 45)))
 '(sml/outside-modified-char "M")
 '(sml/read-only-char "%")
 '(sml/replacer-regexp-list
   (quote
    (("^~/\\.STOW/emacs/\\.emacs\\.d/" ":ED:")
     ("^~/\\.GIT/adamspiers\\.org/emacs/\\.emacs\\.d/" ":ED:")
     ("^~/\\.STOW/emacs/lib/emacs/" ":LE:")
     ("~/lib/emacs/" ":LE:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^:ED:init.d/" ":EID:")
     ("^/sudo:.*:" ":SU:")
     ("^~/.GIT/adamspiers.org/" ":GG:")
     ("^~/.GIT/3rd-party/" ":G3:")
     ("^~/SUSE/cloud/crowbar/git/2.0/" ":CR2:")
     ("^~/SUSE/cloud/crowbar/git/1.6/" ":CRP:")
     (":CR2:barclamps/" ":C2B:")
     (":CRP:barclamps/" ":CPB:")
     ("^~/SUSE/IBS/" ":IBS:")
     ("^~/SUSE/OBS/" ":OBS:")
     ("^:IBS:Devel/Cloud/" ":DC:")
     ("^:DC:2.0/" ":DC2:")
     ("^:DC:3/" ":DC3:")
     ("^:DC2:Staging/" ":DC2S:")
     ("^:DC3:Staging/" ":DC3S:")
     ("^\\(.+:\\)crowbar-barclamp-" "\\1bc-"))))
 '(sml/show-battery nil)
 '(sml/show-client t)
 '(sml/vc-mode-show-backend t)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.emacs\\.backup\\)\\'")
 '(speedbar-tag-split-minimum-length 30)
 '(tidy-shell-command "htmltidy")
 '(tla-non-recursive-inventory nil)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tooltip-mode t)
 '(tramp-verbose 13)
 '(transient-mark-mode t)
 '(undo-tree-mode-lighter "")
 '(uniquify-after-kill-buffer-p nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "adam@spiers.net")
 '(vc-annotate-background "nil")
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(whitespace-empty-at-bob-regexp "^\\(\\(\\([ 	]*
\\)+\\)\\{2\\}\\)")
 '(whitespace-empty-at-eob-regexp "^\\(\\(\\([ 	]*
\\)+\\)\\{2\\}\\)")
 '(whitespace-line-column 80)
 '(whitespace-style
   (quote
    (face trailing space-before-tab newline empty tab-mark)))
 '(whitespace-trailing-regexp "[^>]\\([	  ]+\\)$")
 '(yas-snippet-dirs
   (quote
    ("~/lib/emacs/minor-modes/yasnippet/snippets" "~/lib/emacs/minor-modes/yasnippets-rails/rails-snippets")) nil (yasnippet)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 122 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(cursor ((t (:background "dark orange"))))
 '(custom-button ((t (:background "lightgrey" :foreground "gray20" :box (:line-width 2 :style released-button)))))
 '(custom-face-tag ((t (:weight bold :height 1.44 :family "helvetica"))))
 '(custom-group-tag ((t (:weight bold :height 1.6 :family "helvetica"))))
 '(custom-variable-tag ((t (:foreground "blue" :weight bold :height 1.4 :family "helvetica"))))
 '(cvs-msg-face ((t (:slant italic))) t)
 '(diff-added ((t (:inherit diff-changed :foreground "green3"))))
 '(diff-file-header ((t (:foreground "dark orange" :weight bold))))
 '(diff-header ((t (:foreground "dark orange"))))
 '(diff-refine-added ((t (:inherit diff-refine-change :background "#168816"))))
 '(diff-refine-change ((t (:background "grey30"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red3"))))
 '(erc-current-nick-face ((t (:background "green yellow" :weight bold))) t)
 '(erc-input-face ((t (:foreground "DarkOrange1" :weight bold))))
 '(erc-my-nick-face ((t (:background "plum1" :foreground "black"))) t)
 '(font-lock-preprocessor-face ((t (:foreground "medium turquoise"))))
 '(header-line ((t (:background "grey20" :foreground "VioletRed1" :box nil))))
 '(highlight ((t (:background "dark slate blue"))))
 '(info-title-1 ((t (:inherit info-title-2 :height 1.2 :foundry "Inconsolata"))))
 '(info-title-2 ((t (:inherit info-title-3 :height 1.2 :foundry "Inconsolata"))))
 '(italic ((t (:foreground "sky blue" :slant italic))))
 '(magit-branch ((t (:foreground "yellow"))))
 '(magit-header ((t (:inherit header-line))) t)
 '(magit-item-highlight ((t (:background "#2e2732"))))
 '(magit-section-title ((t (:foreground "VioletRed1" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "light blue"))))
 '(mode-line ((t (:background "grey45" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(org-agenda-column-dateline ((t nil)))
 '(org-agenda-date ((t (:inherit org-agenda-structure :background "grey20"))) t)
 '(org-column ((t (:background "#211E1E" :strike-through nil :underline nil :slant normal :weight normal :height 122 :family "Inconsolata"))))
 '(org-done ((t (:background "ForestGreen" :foreground "snow1" :weight bold))))
 '(org-hide ((nil (:foreground "black"))))
 '(rpm-spec-dir-face ((((class color) (background light)) (:foreground "olive drab"))) t)
 '(scroll-bar ((t (:background "grey45"))))
 '(sml/global ((t (:foreground "gray10"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "#f39400"))))
 '(sml/read-only ((t (:inherit sml/global :foreground "medium blue"))))
 '(sml/time ((t (:inherit sml/global))))
 '(whitespace-indentation ((t (:background "#abab00" :foreground "firebrick"))))
 '(whitespace-space-after-tab ((t (:background "#abab00" :foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:background "#451a00" :foreground "firebrick"))))
 '(whitespace-tab ((t (:foreground "dim gray"))))
 '(whitespace-trailing ((t (:background "#451a00" :foreground "yellow" :weight bold)))))
