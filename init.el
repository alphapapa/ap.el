;argh

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1E1C31" "#FF8080" "#95FFA4" "#FFE9AA" "#91DDFF" "#C991E1" "#AAFFE4" "#CBE3E7"])
 '(bufler-workspace-mode t)
 '(bufler-workspace-tabs-mode nil)
 '(burly-before-open-bookmark-hook '(tab-bar-new-tab))
 '(comp-deferred-compilation t t)
 '(completion-styles '(basic partial-completion emacs22 substring flex initials))
 '(completions-format 'vertical)
 '(custom-enabled-themes '(doom-solarized-dark))
 '(custom-safe-themes
   '("01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" default))
 '(electric-pair-mode t)
 '(fci-rule-color "#62686E")
 '(global-tab-line-mode t)
 '(inhibit-startup-screen t)
 '(ivy-mode t)
 '(ivy-sort-matches-functions-alist
   '((t . ivy--shorter-matches-first)
     (ivy-completion-in-region . ivy--shorter-matches-first)
     (ivy-switch-buffer . ivy-sort-function-buffer)
     (t . ivy--prefix-sort)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#ff665c")
 '(org-agenda-files '("~/org/inbox.org"))
 '(org-log-into-drawer t)
 '(org-superstar-remove-leading-stars t)
 '(org-todo-keywords
   '((sequence "TODO(t!)" "TODAY(a!)" "NEXT(n!)" "STARTED(s!)" "UNDERWAY(u!)" "WAITING(w@)" "SOMEDAY(o!)" "MAYBE(m!)" "|" "DONE(d@)" "CANCELED(c@)")
     (sequence "CHECK(k!)" "|" "DONE(d@)")
     (sequence "TO-READ(r!)" "READING(R!)" "|" "HAVE-READ(d@)")
     (sequence "TO-WATCH(!)" "WATCHING(!)" "SEEN(!)")
     (type "NOW(N!)" "|")
     (type "IMPORTANT(i!)" "|")
     (sequence "PROJECT(p!)" "|" "COMPLETED(c!)")))
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(org-web-tools org-sidebar org-super-agenda bufler org-ql burly buttercup doom-themes ivy-posframe stream forge imenu-list helm-bufler magit-todos dockerfile-mode unpackaged org-make-toc helm-org helm helm-core popup org-superstar org-now magit debbugs org-bullets spacemacs-theme highlight-function-calls scrollkeeper aggressive-indent prism general lispy magit-section pretty-hydra hydra lv f s dash-functional dash quelpa-use-package))
 '(pdf-view-midnight-colors (cons "#CBE3E7" "#1E1C31"))
 '(quelpa-update-melpa-p nil)
 '(rustic-ansi-faces
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(safe-local-variable-values
   '((eval when
	   (string-suffix-p ".txt" buffer-file-name)
	   (fundamental-mode)
	   (prism-mode))
     (org-auto-expand-nodes
      (("Daily")
       . body)
      (("Emacs")
       body 2))
     (org-use-property-inheritance . t)
     (magit-todos-exclude-globs "makem.sh" "Makefile")
     (magit-todos-update)))
 '(scroll-bar-mode nil)
 '(tab-bar-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bc275")
    (cons 40 "#a6c677")
    (cons 60 "#d1ca79")
    (cons 80 "#FCCE7B")
    (cons 100 "#f4b96e")
    (cons 120 "#eda461")
    (cons 140 "#e69055")
    (cons 160 "#db8981")
    (cons 180 "#d082ae")
    (cons 200 "#C57BDB")
    (cons 220 "#d874b0")
    (cons 240 "#eb6d86")
    (cons 260 "#ff665c")
    (cons 280 "#d15e59")
    (cons 300 "#a35758")
    (cons 320 "#754f56")
    (cons 340 "#62686E")
    (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :family "Fantasque Sans Mono"))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(italic ((t (:slant italic))))
 '(org-meta-line ((t (:inherit font-lock-comment-face))))
 '(tab-bar ((t (:inherit (header-line default) :weight bold))))
 '(tab-bar-tab ((t (:inherit (highlight tab-bar)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar))))
 '(tab-line ((t (:inherit (header-line default) :underline "#5d4d7a" :height 0.9))))
 '(tab-line-tab ((t (:inherit (highlight tab-line)))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab))))
 '(tab-line-tab-inactive ((t (:inherit tab-line)))))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; (native-compile-async "~/.emacs.d/elpa/" 3 t)

(use-package general)

(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :demand t)

(use-package unpackaged
  :quelpa
  (unpackaged :fetcher github :repo "alphapapa/unpackaged.el"))

(use-package bufler
  :quelpa
  (bufler :fetcher github :repo "alphapapa/bufler.el"
	  :files (:defaults (:exclude "helm-bufler.el")))
  
  :general
  ("C-x b" #'bufler-switch-buffer
   "C-x B" #'bufler-workspace-focus-buffer
   "C-x C-b" #'bufler)

  :custom
  (bufler-groups
   (bufler-defgroups
     (group (auto-workspace))
     (group (group-or "Elfeed"
                      (mode-match "*Elfeed*" (rx bos "elfeed-"))
                      (name-match "elfeed config" (rx bos "elfeed." (or "el" "org")))))
     (group
      (group-not "*Special*"
                 (group-or "*Special*"
                           (mode-match "Magit" (rx bos "magit-status"))
                           (mode-match "Org" (rx bos "org-"))
                           (auto-file)
                           (mode-match "Dired" (rx bos "dired-"))))
      (group
       (name-match "**Special**"
                   (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
      (group
       (group-or "*Help/Info*"
                 (mode-match "*Help*" (rx bos "help-"))
                 (mode-match "*Info*" (rx bos "info-")))
       (auto-mode))
      (group
       (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
       (auto-directory))
      (group
       (mode-match "*Helm*" (rx bos "helm-")))
      (auto-mode))
     (group
      (group-or "Org"
                (dir "~/org")
                (name-match "*Org QL*" (rx bos "*Org QL")))
      (group (name-match "*Org QL*" (rx bos "*Org QL")))
      (group (auto-indirect)
             (auto-file)))
     (group-or "Emacs"
               (dir "/usr/share/emacs")
               (dir "~/.emacs.d")
               (dir "~/src/emacs")
               (dir "~/src/emacs/emacs"))
     (group-or "Home"
               (dir '("~/.config" "~/.homesick/repos/main"))
               (dir "~/.bin"))
     (group
      (auto-parent-project))
     (auto-directory)
     (auto-mode)))
  (bufler-workspace-mode t)
  (bufler-workspace-tabs-mode t))

(use-package helm-bufler
  :quelpa
  (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
               :files ("helm-bufler.el"))

  :config
  ;; (cl-pushnew 'helm-bufler-source ap/helm-find-files-sources)
  )

(use-package burly
  :quelpa
  (burly :fetcher github :repo "alphapapa/burly.el"))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package highlight-function-calls
  :hook
  (emacs-lisp-mode . highlight-function-calls-mode))

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode))

(use-package prism
  :hook
  (emacs-lisp-mode . prism-mode)

  :config
  (defun ap/prism-spacemacs-dark ()
    (interactive)
    (prism-set-colors :colors '(font-lock-keyword-face
				font-lock-constant-face
				font-lock-string-face)
      :desaturations '(10 15 20 25 30 35)
      :lightens '(5 10 15 20 25 30)))
  (defun ap/prism-solarized-dark1 ()
    (interactive)
    (prism-set-colors
      :desaturations '(20 50 80)
      :lightens '(10 20 30)))

  (defun ap/prism-solarized-dark2 ()
    (interactive)
    (prism-set-colors
      :desaturations '(40 60 80)
      :lightens '(10 20 30))))

(use-package scrollkeeper
  :general
  ([remap scroll-down-command] #'scrollkeeper-up)
  ([remap scroll-up-command] #'scrollkeeper-down))

(use-package ivy
  :general
  (:keymaps 'ivy-minibuffer-map
	    "TAB" #'ivy-next-line)
  :custom
  (ivy-mode t))

(use-package org
  :general
  (:keymaps 'org-agenda-mode-map
	    "RET" #'ap/org-agenda-switch-to-heading-in-indirect-buffer)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . visual-line-mode)
  
  :custom
  (org-todo-keywords
   (quote
    ((sequence "TODO(t!)" "TODAY(a!)" "NEXT(n!)" "STARTED(s!)" "UNDERWAY(u!)" "WAITING(w@)" "SOMEDAY(o!)" "MAYBE(m!)" "|" "DONE(d@)" "CANCELED(c@)")
     (sequence "CHECK(k!)" "|" "DONE(d@)")
     (sequence "TO-READ(r!)" "READING(R!)" "|" "HAVE-READ(d@)")
     (sequence "TO-WATCH(!)" "WATCHING(!)" "SEEN(!)")
     (type "NOW(N!)" "|")
     (type "IMPORTANT(i!)" "|")
     (sequence "PROJECT(p!)" "|" "COMPLETED(c!)"))))

  :config
  (defun ap/org-agenda-goto-heading-in-indirect-buffer (&optional switch-to)
    "Go to the current agenda headline in an indirect buffer. If SWITCH-TO is non-nil, close the org-agenda window."
    (interactive)
    (if switch-to
        (org-agenda-switch-to)
      (org-agenda-goto))
    (org-tree-to-indirect-buffer)

    ;; Put the non-indirect buffer at the bottom of the prev-buffers
    ;; list so it won't be selected when the indirect buffer is killed
    (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                         (car (window-prev-buffers)))))

  (defun ap/org-agenda-switch-to-heading-in-indirect-buffer ()
    (interactive)
    (ap/org-agenda-goto-heading-in-indirect-buffer t)))

(use-package org-make-toc
  :quelpa (org-make-toc :fetcher github :repo "alphapapa/org-make-toc"
			:branch "wip/rewrite"))

(use-package org-now
  :quelpa (org-now :fetcher github :repo "alphapapa/org-now"))

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"))

(use-package org-sidebar
  :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar"))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(find-file user-init-file)
