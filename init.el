;;; ap.el --- A simple, Emacs Lisp-focused config   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author:  Adam Porter <adam@alphapapa.net>
;; Keywords:
;; Package-Requires: ((emacs "28.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple Emacs configuration focused on editing Emacs Lisp.
;; It's mostly intended as a sample from which others may borrow code
;; or inspiration, but it may be used as-is.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1E1C31" "#FF8080" "#95FFA4" "#FFE9AA" "#91DDFF" "#C991E1" "#AAFFE4" "#CBE3E7"])
 '(async-bytecomp-package-mode t)
 '(async-shell-command-buffer 'new-buffer)
 '(avy-timeout-seconds 0.25)
 '(browse-url-browser-function 'eww-browse-url)
 '(bufler-columns '("Name" "Size" "Mode" "VC" "Path"))
 '(bufler-filter-buffer-modes
   '(bufler-list-mode calendar-mode fundamental-mode helm-major-mode magit-diff-mode magit-process-mode magit-revision-mode magit-section-mode special-mode timer-list-mode deffy-mode))
 '(burly-before-open-bookmark-hook '(tab-bar-new-tab))
 '(comint-input-ignoredups t)
 '(comp-deferred-compilation t t)
 '(compilation-scroll-output 'first-error)
 '(completion-cycle-threshold 5)
 '(completions-format 'vertical)
 '(corfu-auto t)
 '(custom-enabled-themes '(doom-solarized-dark))
 '(custom-safe-themes
   '("a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" default))
 '(delete-by-moving-to-trash t)
 '(dired-create-destination-dirs 'ask)
 '(dired-listing-switches "-alh")
 '(dired-omit-verbose nil)
 '(doom-outrun-electric-brighter-modeline t)
 '(doom-outrun-electric-comment-bg t)
 '(ement-save-sessions t)
 '(fci-rule-color "#62686E")
 '(forge-topic-list-limit '(60 . -5))
 '(frame-resize-pixelwise t)
 '(global-tab-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(ivy-sort-matches-functions-alist
   '((t . ivy--shorter-matches-first)
     (ivy-completion-in-region . ivy--shorter-matches-first)
     (ivy-switch-buffer . ivy-sort-function-buffer)
     (t . ivy--prefix-sort)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format " [%s of %s]")
 '(list-directory-brief-switches "-CFh")
 '(list-directory-verbose-switches "-lh")
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk 'all)
 '(magit-status-goto-file-position t)
 '(magit-wip-after-apply-mode t)
 '(magit-wip-after-save-mode t)
 '(magit-wip-before-change-mode t)
 '(menu-bar-mode nil)
 '(minions-direct
   '(auto-revert-mode aggressive-indent-mode salv-mode obvious-mode hammy-mode))
 '(minions-mode t)
 '(modus-themes-bold-constructs t)
 '(modus-themes-hl-line '(accented))
 '(modus-themes-org-blocks 'gray-background)
 '(modus-themes-tabs-accented t)
 '(objed-cursor-color "#ff665c")
 '(org-agenda-files '("~/org/inbox.org"))
 '(org-attach-use-inheritance nil)
 '(org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
 '(org-bookmark-heading-make-ids
   '(lambda nil
      (and
       (buffer-file-name)
       (file-in-directory-p
        (buffer-file-name)
        org-directory))))
 '(org-catch-invisible-edits 'smart)
 '(org-catch-invisible-edits 'error)
 '(org-clock-mode-line-total 'current)
 '(org-clock-report-include-clocking-task t)
 '(org-ctrl-k-protect-subtree t)
 '(org-ellipsis "↵")
 '(org-fontify-quote-and-verse-blocks t)
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id t)
 '(org-id-locations-file-relative t)
 '(org-imenu-depth 8)
 '(org-indirect-buffer-display 'current-window)
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet '(("+" . "-") ("-" . "*") ("*" . "+")))
 '(org-list-two-spaces-after-bullet-regexp nil)
 '(org-log-into-drawer t)
 '(org-log-refile 'note)
 '(org-modern-checkbox '((88 . "✓") (45 . "□–") (32 . "□")))
 '(org-modern-hide-stars nil)
 '(org-modern-list '((43 . "•") (45 . "◦") (42 . "★")))
 '(org-now-default-cycle-level nil)
 '(org-now-hook '(hl-line-mode))
 '(org-now-location '("~/org/now.org"))
 '(org-ql-find-goto-hook '(org-show-entry ap/org-ql-find-tree-to-indirect-buffer))
 '(org-ql-views
   '(("Watching / To-Watch" :buffers-files
      ("/home/me/org/articles.org" "/home/me/org/bible.org" "/home/me/org/books.org" "/home/me/org/calendar.org" "/home/me/org/cpb.org" "/home/me/org/inbox.org" "/home/me/org/job.org" "/home/me/org/links.org" "/home/me/org/log.org" "/home/me/org/main.org" "/home/me/org/misc.org" "/home/me/org/music.org" "/home/me/org/now.org" "/home/me/org/onyx-upgrade.org" "/home/me/org/op.org" "/home/me/org/people.org" "/home/me/org/posts.org" "/home/me/org/prayers.org" "/home/me/org/quotes.org" "/home/me/org/reference.org" "/home/me/org/research.org" "/home/me/org/scratch.org" "/home/me/org/sparky.org" "/home/me/org/temp.org")
      :query
      (todo "WATCHING" "TO-WATCH")
      :sort
      (todo date)
      :narrow nil :super-groups
      ((:tag "games")
       (:tag "DIY")
       (:tag "Emacs")
       (:tag
        ("software" "programming" "Lisp"))
       (:tag "music")
       (:auto-category))
      :title "Watching / To-Watch")
     ("Overview: Agenda-like" :buffers-files org-agenda-files :query
      (and
       (not
        (done))
       (or
        (habit)
        (deadline auto)
        (scheduled :to today)
        (ts-active :on today)))
      :sort
      (todo priority date)
      :super-groups org-super-agenda-groups :title "Agenda-like")
     ("Overview: NEXT tasks" :buffers-files org-agenda-files :query
      (todo "NEXT")
      :sort
      (date priority)
      :super-groups org-super-agenda-groups :title "Overview: NEXT tasks")
     ("Calendar: Today" :buffers-files org-agenda-files :query
      (ts-active :on today)
      :title "Today" :super-groups org-super-agenda-groups :sort
      (priority))
     ("Review: Recently timestamped" . org-ql-view-recent-items)
     (#("Review: Dangling tasks" 0 22
        (help-echo "Tasks whose ancestor is done"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (ancestors
        (done)))
      :title
      #("Review: Dangling tasks" 0 22
        (help-echo "Tasks whose ancestor is done"))
      :sort
      (todo priority date)
      :super-groups
      ((:auto-parent t)))
     (#("Review: Stale tasks" 0 19
        (help-echo "Tasks without a timestamp in the past 2 weeks"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (not
        (ts :from -14)))
      :title
      #("Review: Stale tasks" 0 19
        (help-echo "Tasks without a timestamp in the past 2 weeks"))
      :sort
      (todo priority date)
      :super-groups
      ((:auto-parent t)))
     (#("Review: Stuck projects" 0 22
        (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :buffers-files org-agenda-files :query
      (and
       (todo)
       (descendants
        (todo))
       (not
        (descendants
         (todo "NEXT"))))
      :title
      #("Review: Stuck projects" 0 22
        (help-echo "Tasks with sub-tasks but no NEXT sub-tasks"))
      :sort
      (date priority)
      :super-groups org-super-agenda-groups)))
 '(org-recent-headings-mode t)
 '(org-recent-headings-reject-any-fns
   '((lambda
       (entry)
       (not
        (file-in-directory-p
         (org-recent-headings-entry-file entry)
         org-directory)))))
 '(org-recent-headings-reverse-paths t)
 '(org-refile-targets '((org-ql-search-directories-files :tag . "")))
 '(org-refile-use-outline-path t)
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-startup-folded t)
 '(org-sticky-header-full-path 'reversed)
 '(org-superstar-remove-leading-stars nil)
 '(org-use-property-inheritance t)
 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(package-selected-packages
   '(pocket-reader org-ql ement typescript-mode emms ampc cape iscroll helpful hammy dogears company topsy ef-themes org-notely consult org-bookmark-heading hy-mode xr corfu dirvish salv org-recent-headings plz burly prism deffy embark geiser with-simulated-input buttercup inspector dtache macrostep unpackaged queue svg-lib taxy-magit-section taxy org-auto-expand org-sidebar helm-org-ql try htmlize modus-themes marginalia orderless vertico minions bufler helm-bufler snow which-key sr-speedbar org-sticky-header org-web-tools org-super-agenda doom-themes forge imenu-list magit-todos org-make-toc helm-org helm helm-core popup org-superstar org-now magit debbugs org-bullets spacemacs-theme highlight-function-calls scrollkeeper aggressive-indent general lispy magit-section pretty-hydra hydra lv f s dash-functional dash quelpa-use-package))
 '(pdf-view-midnight-colors (cons "#CBE3E7" "#1E1C31"))
 '(prism-comments nil)
 '(prism-parens t)
 '(quelpa-build-dir "/home/me/.cache/emacs/quelpa/build")
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-dir "/home/me/.cache/emacs/quelpa")
 '(quelpa-melpa-dir "/home/me/.cache/emacs/quelpa/melpa")
 '(quelpa-packages-dir "/home/me/.cache/emacs/quelpa/packages")
 '(quelpa-persistent-cache-file "/home/me/.cache/emacs/quelpa/cache")
 '(quelpa-self-upgrade-p nil)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(recentf-mode t)
 '(rustic-ansi-faces
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(safe-local-variable-values
   '((eval require 'org-make-toc)
     (org-tags-column . 0)
     (eval org-auto-expand)
     (magit-wip-after-apply-mode)
     (magit-wip-after-save-mode)
     (magit-wip-before-change-mode)
     (magit-todos-exclude-globs "makem.sh")
     (magit-todos-exclude-globs "elpa/")
     (eval when
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
 '(standard-themes-bold-constructs t)
 '(standard-themes-italic-constructs t)
 '(standard-themes-mixed-fonts t)
 '(standard-themes-mode-line-accented t)
 '(standard-themes-prompts '(background bold))
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs-groups tab-bar-separator tab-bar-format-add-tab))
 '(tab-bar-mode t)
 '(tab-bar-new-tab-choice 'bookmark-bmenu-get-buffer)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)
 '(tab-bar-tab-post-change-group-functions '(tab-bar-move-tab-to-group))
 '(tab-line-close-button-show nil)
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
 '(vc-annotate-very-old-color nil)
 '(which-key-mode t)
 '(window-resize-pixelwise t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :family "Fantasque Sans Mono"))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(italic ((t (:slant italic))))
 '(org-ellipsis ((t (:inherit font-lock-comment-face))))
 '(org-meta-line ((t (:inherit font-lock-comment-face))))
 '(org-todo ((t (:foreground "#859900" :inverse-video t :box (:line-width (2 . 2) :color "dark red" :style flat-button) :weight bold))))
 '(scrollkeeper-guideline-highlight ((t (:extend t :background "#2aa198"))) t)
 '(tab-bar ((t (:inherit (header-line default) :box nil :weight bold :height 1.1 :width condensed :family "NK57 Monospace"))))
 '(tab-bar-tab ((t (:inherit (highlight tab-bar)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar))))
 '(tab-line ((t (:inherit modus-themes-tab-backdrop :box nil :width condensed :family "NK57 Monospace")))))

(cl-pushnew '("melpa" . "https://melpa.org/packages/") package-archives :test #'equal)

(package-initialize)

;; (native-compile-async "~/.emacs.d/elpa/" 3 t)

;; Early package config (required by later config).

(use-package use-package
  :init
  ;; Doesn't work with :custom because it must be set before loading
  ;; `use-package'.
  (setf use-package-enable-imenu-support t))

(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :demand t)

(use-package general

  :config
  (global-unset-key (kbd "M-SPC"))
  (general-create-definer ap/general-def
    :prefix "M-SPC"))

;;; Per-package configuration

(use-package aggressive-indent-mode
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package auto-revert
  :hook (prog-mode . auto-revert-mode))

(use-package avy
  :bind* (("C-j" . avy-goto-char-timer)))

(use-package bufler
  :quelpa
  (bufler :fetcher github :repo "alphapapa/bufler.el"
          :files (:defaults (:exclude "helm-bufler.el")))

  :general
  ("C-x b" #'bufler-switch-buffer
   "C-x B" #'bufler-workspace-focus-buffer
   "C-x C-b" #'bufler)
  ("C-x w o" #'bufler-workspace-open
   "C-x w r" #'bufler-workspace-reset
   "C-x w s" #'bufler-workspace-save)

  :custom
  (bufler-groups
   (bufler-defgroups
     (group (auto-workspace))
     (group (mode-match "Ement" (rx bos "ement-")))
     (group (group-or "Elfeed"
                      (mode-match "*Elfeed*" (rx bos "elfeed-"))
                      (name-match "elfeed config" (rx bos "elfeed." (or "el" "org")))))
     (group
      (group-not "*Special*"
                 (group-or "*Special*"
                           (mode-match "Magit" (rx bos "magit-status"))
                           (mode-match "Forge" (rx bos "forge-"))
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
               (dir "~/src/emacs/emacs")
               (dir "~/src/archive/emacs"))
     (group-or "Home"
               (dir '("~/.config" "~/.homesick/repos/main"))
               (dir "~/.bin"))
     (group
      (auto-parent-project)
      (auto-indirect))
     (auto-directory)
     (auto-mode)))
  (bufler-reverse t)
  (bufler-face-prefix "prism-level-")
  (bufler-initial-face-depth 1)
  (bufler-workspace-mode t)
  ;; (bufler-workspace-tabs-mode t)
  )

(use-package burly
  :quelpa
  (burly :fetcher github :repo "alphapapa/burly.el"
	 :branch "wip/tabs")

  :bind (("C-x t R" . burly-reset-tab))
  :init
  (burly-tabs-mode))

;; [2022-09-01 Thu 01:22] Disabling Company in favor of Corfu, which
;; seems to generally perform better (e.g. it doesn't freeze when
;; backspacing out of a long completion list).

;; (use-package company
;;   :hook (prog-mode . company-mode))

(use-package corfu
  :hook (prog-mode . corfu-mode))

(use-package compile
  :config
  (progn
    ;; Apply ANSI terminal color escape codes. <http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html>
    (require 'ansi-color)
    (defun endless/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook #'endless/colorize-compilation))

  (setf compilation-save-buffers-predicate
        ;; NOTE: For some reason setting this in :custom doesn't seem to work.
        ;; HACK: The docstring says that `compilation-directory' can't "generally" be used,
        ;; apparently because it's only set *after* `compile' calls `save-some-buffers'.  If that
        ;; line of code wre moved up one line, it would solve the problem (probably without causing
        ;; any others).  Anyway, using `default-directory' when `compilation-directory' isn't set
        ;; should work.
        (lambda ()
          (when (buffer-file-name)
            (string-prefix-p (or compilation-directory default-directory)
                             (file-truename (buffer-file-name)))))))

(use-package consult
  :bind (:map global-map
              ("M-g i" . consult-imenu)
              ("M-g M-i" . consult-imenu-multi)
              ("M-g l" . consult-line)
              ("M-g M-l" . consult-line-multi))
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (require 'consult-imenu)
              (cl-pushnew
               ;; Elisp Imenu section headings.
               '(115 "Sections" font-lock-comment-face)
               (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config) :types)))))

(use-package custom
  :config
  (defun ap/switch-theme (theme)
    "Disable active themes and load THEME."
    (interactive
     (list (intern (completing-read "Theme: "
                                    (->> (custom-available-themes)
                                         (-map #'symbol-name))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme 'no-confirm)))

(use-package deffy
  :quelpa (deffy :fetcher github :repo "alphapapa/taxy.el"
            :files ("examples/deffy.el"))
  :bind (:map global-map
              ("C-x p d" . deffy-project)
              ("M-g d" . deffy-jump)))

(use-package dired
  :bind (:map dired-mode-map
              ([mouse-2]
               ;; I don't understand how or why mouse-1 is translated
               ;; into mouse-2 in Dired, but it is, and this works.
               . dired-mouse-find-file))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-omit-mode)
  (dired-mode . (lambda ()
                  (toggle-truncate-lines 1))))

(use-package dogears
  :init (dogears-mode)
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g M-d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-D" . dogears-list)
              ;; ("M-g M-D" . dogears-sidebar)
	      ))

(use-package doom-themes
  :config
  (unpackaged/customize-theme-faces 'doom-solarized-dark
    `(ement-room-self  ((t :foreground ,(face-foreground 'warning))))
    `(ement-room-self-message ((t :foreground ,(face-foreground 'warning))))
    `(mode-line ((t :box (:line-width 1 :color ,(face-foreground 'font-lock-builtin-face)))))
    `(header-line ((t :box nil :background ,(face-background 'region))))))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package embark
  ;; TODO: Install `embark-consult'.
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))

  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package emacs
  :init
  (setq completion-ignore-case t)

  :config
  (defun ap/kill-this-buffer ()
    "Kill current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  :bind (("C-x C-k" . #'ap/kill-this-buffer)))

(use-package eww
  :general
  (:keymaps 'eww-mode-map
            [mouse-8] #'eww-back-url
            [mouse-9] #'eww-forward-url))

(use-package faces
  :config
  (defvar ap/random-fonts
    `(
      ;; A list of lists or alists. Alists should be in (STRING
      ;; . STRING) format, with the first string being the frame-font, and
      ;; the second the variable-pitch font.  If an element is a list
      ;; rather than an alist, it will be set as the frame-font, and the
      ;; variable-pitch font will be set to a default.
      ("Fantasque Sans Mono-10")
      ("DejaVu Sans Mono-9" . "DejaVu Sans")
      ("Ubuntu Mono-10" . "Ubuntu")
      ("Droid Sans Mono-9" . "Droid Sans")
      ("Input Mono Narrow-9" . "Input Sans Condensed")
      ("Input Sans Condensed-9" . "Input Sans Condensed")
      ("Consolas-10")
      ("Inconsolata-10")
      ("Anonymous Pro-10")
      ("Liberation Mono-9")
      ("Fira Mono-9" . "Fira Sans")
      ("Fira Code-9" . "Fira Sans")
      ("Hack-9")
      ("NK57 Monospace-9:width=semi-condensed")
      ("NK57 Monospace-9")
      ))

  (defun ap/set-random-frame-font ()
    "Set random fonts from ap/random-fonts list."
    (interactive)
    (ap/set-custom-fonts (seq-random-elt ap/random-fonts)))

  (defun ap/set-custom-fonts (font)
    "Set frame-font and variable-pitch font using FONT.

FONT should be either a single-element list containing the
frame-font, or a cons cell in (FRAME-FONT . VARIABLE-PITCH-FONT)
format."
    (interactive
     (list (let ((choice (completing-read "Font: " ap/random-fonts)))
             (assoc choice ap/random-fonts))))
    (let ((frame-font (car font))
          (variable-font (or (cdr font) "DejaVu Sans")))
      (set-frame-font frame-font t)
      (set-face-font 'default frame-font)
      (set-face-font 'variable-pitch variable-font)
      ;; Set org faces
      (dolist (face '(org-block org-block-begin-line org-meta-line))
        (when (facep face)
          (set-face-attribute face nil :font frame-font)))
      ;; Set buffer-face for org buffers
      (when (symbol-function 'org-buffer-list)
        (dolist (buffer (org-buffer-list))
          (with-current-buffer buffer
            (buffer-face-set :family (face-attribute 'variable-pitch :family)
                             :height (face-attribute 'variable-pitch :height)))))
      (message "%s" frame-font))))

(use-package flymake
  :config
  (advice-add #'elisp-flymake-byte-compile :around
              (defun ap/elisp-flymake-byte-compile-around (oldfun &rest args)
                "Call `elisp-flymake-byte-compile' having added directories of `load-path' to `elisp-flymake-byte-compile-load-path'.
Otherwise, `elisp-flymake-byte-compile' is practically useless,
because it will always fail to find third-party libraries,
causing an error that prevents it from even linting the rest of
the file!"
                (let ((elisp-flymake-byte-compile-load-path
                       (cons "./" load-path)))
                  (apply oldfun args)))))

(use-package hammy
  :quelpa (hammy :fetcher github :repo "alphapapa/hammy.el")

  :general (ap/general-def "hn" #'hammy-next)

  :init
  (hammy-mode)

  :config
  (progn
    (require 'mpc)

    (cl-defun ap/mpris-toggle-players (&key (bus :session) (messagep t))
      "Toggle playback in MPRIS players that are playing or paused."
      (require 'dbus)
      (cl-labels
          ((mpris-service-p (service)
             (string-prefix-p "org.mpris.MediaPlayer2." service))
           (service-playback-status (service)
             (dbus-get-property bus service "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"
                                "PlaybackStatus"))
           (service-player-name (service)
             (dbus-get-property bus service "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2"
                                "Identity"))
           (service-player-metadata (service)
             (dbus-get-property bus service "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"
                                "Metadata"))
           (toggle-playback (service)
             (dbus-call-method bus service "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player"
                               "PlayPause")
             (when messagep
               (message "Toggled playback of %S in %s"
                        (format-metadata (service-player-metadata service))
                        (service-player-name service))))
           (format-metadata (metadata)
             (-let (((&alist "xesam:artist" ((artists))
                             "xesam:album" ((album))
                             "xesam:title" ((title)))
                     metadata))
               (format "%s - %s: %s" (s-join ", " artists) album title))))
        (->> (dbus-list-known-names bus)
             (-select #'mpris-service-p)
             (--select (member (service-playback-status it)
                               '("Playing" "Paused")))
             (mapc #'toggle-playback))))

    (defcustom ap/hammy-mpc-before-command "pmm cheerful -e vocal %s"
      "Command used to play music in function `ap/hammy-mpc-before'.
Includes \"%s\" format spec for length of playlist in minutes."
      :type 'string)

    (defcustom ap/hammy-mpc-enabled t
      "Whether to toggle MPC playback in Hammy timers."
      :type 'boolean)

    (defun ap/hammy-mpc-toggle ()
      "Toggle option `ap/hammy-mpc-enabled'."
      (interactive)
      (setq ap/hammy-mpc-enabled (not ap/hammy-mpc-enabled))
      (message "Hammy MPC playback %s." (if ap/hammy-mpc-enabled "enabled" "disabled")))

    (defmacro ap/hammy-mpc-before (minutes)
      ;; A macro so that the `run' function expanded by `hammy-define' will work.
      "Play/pause MPC, or run `ap/hammy-mpc-before-command' for MINUTES."
      `(progn
         (mpc-status-refresh)
         (pcase (map-elt mpc-status 'state)
           ("play" nil)
           ("pause" (mpc-play))
           (_ (run (format ap/hammy-mpc-before-command ,minutes))))))

    (hammy-define (propertize "🍅𝅘𝅥𝅮" 'face '(:foreground "tomato"))
      :documentation "The classic pomodoro timer, enhanced (with MPC)."
      :intervals
      (list
       (interval :name "Working"
                 :duration "25 minutes"
                 :before (do (announce "Starting work time.")
                             (notify "Starting work time.")
                             ;; TODO: Get the duration from the interval itself.
                             (when ap/hammy-mpc-enabled
                               (ap/hammy-mpc-before (/ current-duration 60))))
                 :advance (remind "10 minutes"
                                  (do (announce "Break time!")
                                      (notify "Break time!")
                                      (run (concat "aplay " (expand-file-name "~/Misc/Sounds/Mario/smw_coin.wav"))))))
       (interval :name "Resting"
                 :duration (do (if (and (not (zerop cycles))
                                        (zerop (mod cycles 3)))
                                   ;; If a multiple of three cycles have
                                   ;; elapsed, the fourth work period was
                                   ;; just completed, so take a longer break.
                                   "30 minutes"
                                 "5 minutes"))
                 :before (do (announce "Starting break time.")
                             (notify "Starting break time.")
                             (when ap/hammy-mpc-enabled
                               (mpc-pause)))
                 :advance (remind "10 minutes"
                                  (do (announce "Break time is over!")
                                      (notify "Break time is over!")
                                      (run (concat "aplay " (expand-file-name "~/Misc/Sounds/Mario/smw_princess_help.wav"))))))))

    (defcustom ap/hammy-flywheel-rest-duration "5 minutes"
      "Duration of Hammy flywheel rest intervals."
      :type 'string)

    (hammy-define (propertize "🎡𝅘𝅥𝅮" 'face '(:foreground "orange"))
      :documentation "Get your momentum going! (with MPC)"
      :intervals (list
                  (interval :name "Rest"
                            :face 'font-lock-type-face
                            :duration (lambda (&rest _ignore)
                                        ap/hammy-flywheel-rest-duration)
                            :before (do (announce "Rest time!")
                                        (notify "Rest time!")
                                        (when ap/hammy-mpc-enabled
                                          (mpc-pause)))
                            :advance (remind "10 minutes"
                                             (do (announce "Rest time is over!")
                                                 (notify "Rest time is over!")
                                                 (run (concat "aplay " (expand-file-name "~/Misc/Sounds/Mario/smw_princess_help.wav"))))))
                  (interval :name "Work"
                            :face 'font-lock-builtin-face
                            :duration (climb "5 minutes" "45 minutes"
                                             :descend t :step "5 minutes")
                            :before (do (announce "Work time!")
                                        (notify "Work time!")
                                        (when ap/hammy-mpc-enabled
                                          (ap/hammy-mpc-before (/ current-duration 60))))
                            :advance (remind "10 minutes"
                                             (do (announce "Work time is over!")
                                                 (notify "Work time is over!")
                                                 (run (concat "aplay " (expand-file-name "~/Misc/Sounds/Mario/smw_coin.wav")))))))
      :after (do (announce "Flywheel session complete!")
                 (notify "Flywheel session complete!"))
      :complete-p (do (and (> cycles 1)
                           interval
                           (equal "Work" interval-name)
                           (equal (duration "5 minutes") current-duration)))))

  (hammy-define "🐮"
    :documentation "Don't forget to stretch your legs."
    :intervals (list (interval :name "💺"
                               :duration "45 minutes"
                               :face 'font-lock-type-face
                               :before (do (announce "Whew!")
                                           (notify "Whew!"))
                               :advance (remind "10 minutes"
                                                (do (announce "Time to stretch your legs!")
                                                    (notify "Time to stretch your legs!")
                                                    (run (concat "aplay "
                                                                 (expand-file-name "~/Misc/Sounds/Mario/smw_yoshi_swallow.wav"))))))
                     (interval :name "🤸"
                               :duration "5 minutes"
                               :face 'font-lock-builtin-face
                               :before (do (announce "Mooove it!")
                                           (notify "Mooove it!"))
                               :advance (do (announce "Time for a sit-down...")
                                            (notify "Time for a sit-down...")
                                            (run (concat "aplay "
                                                         (expand-file-name "~/Misc/Sounds/Mario/smw_yoshi_tongue.wav"))))))))

(use-package helm-bufler
  :quelpa
  (helm-bufler :fetcher github :repo "alphapapa/bufler.el"
               :files ("helm-bufler.el"))

  :config
  ;; (cl-pushnew 'helm-bufler-source ap/helm-find-files-sources)
  )

(use-package helm-org-ql
  :quelpa (helm-org-ql :fetcher github :repo "alphapapa/org-ql"
                       :files ("helm-org-ql.el")))

(use-package highlight-function-calls
  :hook
  (emacs-lisp-mode . highlight-function-calls-mode))

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package imenu
  :config
  (defun ap/emacs-lisp-imenu-hook ()
    "Add entry to `imenu-generic-expression' for Emacs Lisp buffers."
    ;; TODO: Upstream this.
    (cl-pushnew '("Sections"
                  ;; (rx bol (0+ blank) (>= 3 ";") (0+ blank) (group (1+ nonl)))
                  "^[[:blank:]]*;\\{3,\\}[[:blank:]]*\\(.+\\)"
                  1)
                imenu-generic-expression))
  (add-hook 'emacs-lisp-mode-hook #'ap/emacs-lisp-imenu-hook))

(use-package ivy
  :general
  (:keymaps 'ivy-minibuffer-map
            "TAB" #'ivy-next-line)
  ;; :custom
  ;; (ivy-mode t)
  )

(use-package lispy
  :general (:map 'lispy-mode-map
                 [remap lispy-fill] #'unpackaged/flex-fill-paragraph)
  :hook
  (emacs-lisp-mode . lispy-mode))

(use-package magit
  :custom
  (magit-status-sections-hook
   '(magit-insert-status-headers magit-insert-unpushed-to-upstream-or-recent magit-insert-unpushed-to-pushremote magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream magit-insert-staged-changes magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-unstaged-changes magit-insert-untracked-files magit-insert-stashes)))

(use-package magit-todos
  :quelpa (magit-todos :fetcher github :repo "alphapapa/magit-todos")
  :after magit
  :config (magit-todos-mode 1))

(use-package marginalia
  :init (marginalia-mode))

(use-package modus-themes
  :config
  (dolist (theme '(modus-operandi modus-vivendi))
    (unpackaged/customize-theme-faces theme
      '(ement-room-membership nil)
      '(ement-room-list-name nil)
      `(ement-room-timestamp-header ((t :inherit header-line :weight bold :extend t)))
      ;; I like the way this looks in Lisp buffers, but so many faces
      ;; inherit from it that shouldn't be underlined...
      ;; '(font-lock-function-name-face ((t :underline t)))
      '(font-lock-keyword-face ((t :weight bold)))
      '(font-lock-warning-face ((t :weight bold)))
      `(org-done ((t :foreground ,(face-background 'default) :background ,(face-foreground 'default)
                     :box (:line-width 2 :color "black" :style flat) :inverse-video t))))))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package org
  :general
  ("C-c c" #'org-capture
   "C-c l" #'org-store-link)
  (:keymaps 'org-mode-map
            "C-c c" #'org-capture
            "C-c l" #'org-store-link)
  (:keymaps 'org-agenda-mode-map
            "RET" #'ap/org-agenda-switch-to-heading-in-indirect-buffer)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . auto-revert-mode)
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
  (org-todo-keyword-faces
   `(("WAITING" :inherit org-todo
      ;; This doesn't work properly, maybe a bug in Org or...?
      :background ,(face-attribute 'org-scheduled-previously :foreground))))

  :config
  (define-advice org-id-update-id-locations (:around (oldfun &rest args) include-work-files)
    "Call `org-id-update-id-locations' with `org-id-extra-files' set to relevant Org files in \"~/work\"."
    (let ((org-id-extra-files (directory-files-recursively
                               "~/work" (rx ".org" eos)
                               nil (lambda (filename)
                                     ;; Ignore files in clones of Elisp package repos.
                                     (not (string-match-p (rx "/.sandbox/") filename))))))
      (apply oldfun args)))

  (progn
    ;; Org link type for running commands.
    (require 'ol)

    (org-link-set-parameters "start-process-shell-command"
                             :follow #'org-link-start-process-shell-command-follow)

    (defcustom org-link-start-process-shell-command-safe-commands nil
      "Commands which are considered safe to run without prompting."
      :type '(repeat string))

    (defun org-link-start-process-shell-command-follow (command _)
      (when (or (member command org-link-start-process-shell-command-safe-commands)
                (pcase (read-answer (format "Run command %S? " command)
                                    '(("yes" ?y "run command")
                                      ("no" ?n "don't run command")
                                      ("remember" ?! "run command and remember that it's safe to run")
                                      ("help" ?h "show help")))
                  ("yes" t)
                  ("remember"
                   (cl-pushnew command org-link-start-process-shell-command-safe-commands :test #'equal)
                   (customize-save-variable 'org-link-start-process-shell-command-safe-commands
                                            org-link-start-process-shell-command-safe-commands)
                   t)))
        (start-process-shell-command "org-link-start-process-shell-command-follow" nil command))))

  (cl-defun ap/org-get-indirect-buffer (&optional (buffer (current-buffer)) heading)
    ;; TODO: [2022-11-04 Fri 15:05] I just emailed this function as a
    ;; patch to the Org list.  Remove this from my config when
    ;; appropriate.
    "Return an indirect buffer based on BUFFER.
If HEADING, append it to the name of the new buffer."
    (let* ((base-buffer (or (buffer-base-buffer buffer) buffer))
           (buffer-name (generate-new-buffer-name
                         (format "%s%s"
                                 (buffer-name base-buffer)
                                 (if heading
                                     (concat "::" heading)
                                   ""))))
           (indirect-buffer (make-indirect-buffer base-buffer buffer-name 'clone)))
      ;; ;; Decouple folding state.  We need to do it manually since
      ;; ;; `make-indirect-buffer' does not run
      ;; ;; `clone-indirect-buffer-hook'.
      ;; (org-fold-core-decouple-indirect-buffer-folds)
      indirect-buffer))
  (advice-add #'org-get-indirect-buffer :override #'ap/org-get-indirect-buffer)

  (defun ap/org-tree-to-indirect-buffer (&optional arg)
    "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
    ;; TODO: Upstream this into Org as one of the options for `org-indirect-buffer-display'.
    (interactive "P")
    (let* ((pos (point))
           ;; (buffer-name (let* ((heading (org-get-heading t t))
           ;;                     (level (org-outline-level))
           ;;                     (face (intern (concat "outline-" (number-to-string level))))
           ;;                     (heading-string (propertize (org-link-display-format heading)
           ;;                                                 'face face)))
           ;;                (concat heading-string "::" (buffer-name))))
           (new-buffer (org-get-indirect-buffer (current-buffer) (org-get-heading t t))))
      (switch-to-buffer new-buffer)
      ;; I don't understand why setting the point again is necessary, but it is.
      ;; (rename-buffer buffer-name)
      (goto-char pos)
      (org-narrow-to-subtree)
      ;; NOTE: It took way too much time and effort and
      ;; experimentation to arrive at calling
      ;; `org-cycle-internal-local' in the new indirect buffer to
      ;; provide a useful view of it.  Org needs a better visibility
      ;; API, because the current situation is a mess of
      ;; `org-show-all', `org-cycle', `org-cycle-internal-local',
      ;; `org-cycle-internal-global', etc.  There should be one public
      ;; function for users to call that should take sensible,
      ;; intuitive, well-documented arguments to cause the desired
      ;; behavior.
      (org-cycle-internal-local)))
  (advice-add #'org-tree-to-indirect-buffer :override #'ap/org-tree-to-indirect-buffer)
  ;; (advice-remove #'org-tree-to-indirect-buffer #'ap/org-tree-to-indirect-buffer)

  (defun ap/org-agenda-goto-heading-in-indirect-buffer (&optional switch-to)
    "Go to the current agenda headline in an indirect buffer. If SWITCH-TO is non-nil, close the org-agenda window."
    (interactive)
    (if switch-to
        (org-agenda-switch-to)
      (org-agenda-goto))
    (org-tree-to-indirect-buffer)

    ;; Put the non-indirect buffer at the bottom of the prev-buffers
    ;; list so it won't be selected when the indirect buffer is killed
    (let ((prev-buffers (window-prev-buffers)))
      (cl-rotatef prev-buffers)
      (set-window-prev-buffers nil prev-buffers)))

  (defun ap/org-agenda-switch-to-heading-in-indirect-buffer ()
    (interactive)
    (ap/org-agenda-goto-heading-in-indirect-buffer t)))

(use-package org-agenda
  :config
  (define-minor-mode ap/org-agenda-bulk-allow-search-type-mode
    "Advise `org-agenda-check-type' to allow `search' type agenda-like buffers to use bulk actions.
Useful in, e.g. `org-ql-view' buffers that work fine with bulk
actions but set the `org-agenda-type' to `search', causing
org-agenda to deny bulk actions."
    :global t
    (if ap/org-agenda-bulk-allow-search-type-mode
        (define-advice org-agenda-check-type (:filter-args (error &rest types) allow-search-type)
          (cons error (cons 'search types)))
      (advice-remove 'org-agenda-check-type 'org-agenda-check-type@allow-search-type))))

(use-package org-clock
  :general (ap/general-def
             "ocg" #'org-clock-goto
             "oci" #'org-clock-in
             "oco" #'org-clock-out
             "ocl" #'org-clock-in-last
             "ocz" #'ap/org-clock-add-note)
  :config
  (defun ap/org-clock-add-note ()
    "Call `org-add-note' on currently clocked item."
    (interactive)
    (save-window-excursion
      (org-clock-goto)
      (add-hook 'org-log-buffer-setup-hook 'ap/org-clock-add-note--setup)
      (org-add-note)))

  (defun ap/org-clock-add-note--setup ()
    (remove-hook 'org-log-buffer-setup-hook 'ap/org-clock-add-note--setup)
    ;; TODO: Fix this.
    ;; (setq-local header-line-format
    ;;             (org-with-wide-buffer
    ;;              (format "Adding note to: %S in %S"
    ;;                      (org-entry-get nil "ITEM") (buffer-name))))
    ))

(use-package org-bookmark-heading
  :quelpa (org-bookmark-heading :fetcher github :repo "alphapapa/org-bookmark-heading"))

(use-package org-auto-expand
  :hook (org-mode . org-auto-expand-mode))

(use-package org-make-toc
  :quelpa (org-make-toc :fetcher github :repo "alphapapa/org-make-toc"
                        :branch "wip/rewrite"))

(use-package org-notely
  :quelpa (org-notely :fetcher github :repo "alphapapa/org-notely")
  :general
  ("M-g N" #'org-notely))

(use-package org-now
  :quelpa (org-now :fetcher github :repo "alphapapa/org-now")
  :general
  ("M-g w" #'org-now))

(use-package org-ql
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
	          :files (:defaults (:exclude "helm-org-ql.el")))
  :general
  (:map org-mode-map
        "M-g o" #'org-ql-find)
  ("M-g O" #'org-ql-find-in-org-directory)

  :config
  (defun ap/org-ql-find-tree-to-indirect-buffer ()
    "Show entry in indirect buffer and bury base buffer."
    ;; This is not ideal, because e.g. when using `org-ql-find' in an
    ;; Org buffer (rather than from elsewhere in Emacs), I might not
    ;; want the base buffer to be buried.  But by the time this
    ;; function is called from the hook, it's too late to know what
    ;; buffer was current when the user called the command.  So for
    ;; now we'll just try this.
    (org-tree-to-indirect-buffer)
    (let* ((base-buffer (buffer-base-buffer (current-buffer)))
           (window (selected-window))
           (entry (assq base-buffer (window-prev-buffers window))))
      ;; Copied from `switch-to-prev-buffer':
      ;; Remove `old-buffer' from WINDOW's previous and (restored list
      ;; of) next buffers.
      (set-window-prev-buffers window (assq-delete-all base-buffer (window-prev-buffers window)))
      (set-window-next-buffers window (delq base-buffer (window-next-buffers window)))
      (when entry
        ;; Append old-buffer's entry to list of WINDOW's previous
        ;; buffers so it's less likely to get switched to soon but
        ;; `display-buffer-in-previous-window' can nevertheless find
        ;; it.
        (set-window-prev-buffers window (append (window-prev-buffers window)
                                                (list entry)))))))

(use-package org-sidebar
  :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar"))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

;; (use-package org-superstar
;;   ;; FIXME: This needs to be activated after org-indent-mode to
;;   ;; prevent `org-superstar-leading-bullet' from being shown.  Weird.
;; [2023-05-08 Mon] Replacing with org-modern.
;;   :hook (org-mode . org-superstar-mode))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package prism
  :quelpa (prism :fetcher github :repo "alphapapa/prism.el")

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
      :lightens '(10 20 30)))

  (unpackaged/define-chooser ap/prism-theme
    ("Shuffle random number of theme faces"
     (prism-set-colors :num 24
       :colors (let* ((faces (list 'font-lock-regexp-grouping-backslash 'font-lock-regexp-grouping-construct
                                   'font-lock-negation-char-face 'font-lock-preprocessor-face
                                   'font-lock-function-name-face 'font-lock-keyword-face
                                   'font-lock-variable-name-face 'font-lock-warning-face
                                   'font-lock-builtin-face 'font-lock-constant-face
                                   'font-lock-string-face 'font-lock-type-face))
                      (colors (->> faces
                                   (--map (face-attribute it :foreground))
                                   (--remove (eq 'unspecified it))
                                   -uniq))
                      (num (max 3 (random (1+ (length colors))))))
                 (prism-shuffle (seq-take colors num)))))
    ("Default"
     (prism-set-colors :num 16
       :colors (list 'font-lock-type-face 'font-lock-function-name-face
                     'font-lock-keyword-face 'font-lock-doc-face)))
    ("Bunting"
     (prism-set-colors :num 16
       :local (pcase current-prefix-arg
                ('(16) 'reset)
                (_ current-prefix-arg))
       :desaturations (cl-loop for i from 0 below 16
                               collect (* i 3))
       :lightens (cl-loop for i from 0 below 16
                          collect (* 3 i))
       :colors (list "red" "white" "dodgerblue" "white")))
    ("Keen"
     (prism-set-colors :num 24
       :local (pcase current-prefix-arg
                ('(24) 'reset)
                (_ current-prefix-arg))
       :desaturations (cl-loop for i from 0 below 24
                               collect (* i 2.5))
       :lightens (cl-loop for i from 0 below 24
                          collect (* i 2.5))
       :colors (list "sandy brown" "dodgerblue" "medium sea green")
       :comments-fn
       (lambda (color)
         (prism-blend color (face-attribute 'font-lock-comment-face :foreground) 0.25))
       :strings-fn
       (lambda (color)
         (prism-blend color "white" 0.5))
       :parens-fn
       (lambda (color)
         (prism-blend color (face-attribute 'default :background) 0.25))))
    ("Solarized: rainbow"
     (prism-set-colors :num 24
       :local (pcase current-prefix-arg
                ('(16) 'reset)
                (_ current-prefix-arg))
       :lightens '(5 15 25)
       :colors (mapcar #'doom-color '(red orange yellow green blue cyan violet magenta))
       :comments-fn (lambda (color)
                      (--> color
                           (color-desaturate-name it 50)))
       :strings-fn (lambda (color)
                     (prism-blend color "white" 0.5))))
    ("Solarized: rainbow inverted"
     (prism-set-colors :num 24
       :local (pcase current-prefix-arg
                ('(16) 'reset)
                (_ current-prefix-arg))
       :lightens '(5 15 25)
       :colors (reverse (mapcar #'doom-color '(red orange yellow green blue cyan violet magenta)))
       :comments-fn (lambda (color)
                      (--> color
                           (color-desaturate-name it 50)))
       :strings-fn (lambda (color)
                     (prism-blend color "white" 0.5))))
    ("Manegarm"
     (prism-set-colors :num 16
       :colors (list 'font-lock-type-face 'custom-face-tag
                     'font-lock-builtin-face
                     'font-lock-variable-name-face)))))

(use-package re-builder
  ;; From Karthik Chikmagalur's blog post at
  ;; <https://karthinks.com/software/bridging-islands-in-emacs-1/>.
  ;; See also bug#48009.
  :config
  (defvar ap/re-builder-positions nil
    "Point and region bounds before calling `re-builder'.")
  (advice-add #'re-builder :before
              (defun ap/re-builder-save-state (&rest _)
                "Save point and region before calling `re-builder'."
                (setq ap/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))
  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of `re-builder'.
With DELIMITED, only replace matches surrounded by word
boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-binding reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car ap/re-builder-positions))
           (beg (cadr ap/re-builder-positions))
           (end (caddr ap/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
        (setq ap/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end))))

  (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
  (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp)
  (global-set-key (kbd "C-M-%") #'re-builder))

(use-package salv
  :quelpa
  (salv :fetcher github :repo "alphapapa/salv.el")

  :init
  (defun ap/salv-mode-org-init-hook ()
    (when (and (buffer-file-name)
               (file-in-directory-p (buffer-file-name) org-directory))
      (salv-mode 1)))
  (add-hook 'org-mode-hook #'ap/salv-mode-org-init-hook))

(use-package scrollkeeper
  :general
  ([remap scroll-down-command] #'scrollkeeper-up)
  ([remap scroll-up-command] #'scrollkeeper-down))

(use-package selectrum
  ;; Trying Vertico instead.  See comment below.
  ;; :init (selectrum-mode)
  )

(use-package selectrum-prescient
  ;; Trying Vertico instead.  See comment below.
  :after selectrum
  ;; :init (selectrum-prescient-mode)
  )

(use-package switchy-window
  :general
  (:map switchy-window-minor-mode-map
        [remap other-window] #'switchy-window)
  :init
  (switchy-window-minor-mode)
  :config
  (add-hook 'window-selection-change-functions
            (defun ap/pulse-line-on-window-selection-change (frame)
              "For use in `window-selection-change-functions', with `switchy-window-minor-mode'."
              (when (eq frame (selected-frame))
                (pulse-momentary-highlight-one-line)))))

(use-package tab-bar
  :init
  (setf mode-line-misc-info
        ;; When the tab-bar is active, don't show global-mode-string
        ;; in mode-line-misc-info, because we now show that in the
        ;; tab-bar using `tab-bar-format-align-right' and
        ;; `tab-bar-format-global'.
        (remove '(global-mode-string ("" global-mode-string))
                mode-line-misc-info)))

(use-package taxy
  :quelpa
  (taxy :fetcher github :repo "alphapapa/taxy.el"))

(use-package taxy-magit-section
  :quelpa
  (taxy-magit-section :fetcher github :repo "alphapapa/taxy.el"
                      :branch "package/taxy-magit-section"))

(use-package topsy
  :hook (prog-mode . topsy-mode))

(use-package unpackaged
  :quelpa
  (unpackaged :fetcher github :repo "alphapapa/unpackaged.el"))

(use-package vertico
  ;; Commenting out for now, because `selectrum-prescient-mode' is
  ;; very nice.  If/when
  ;; <https://github.com/raxod502/prescient.el/issues/89> is done, it
  ;; should be usable with `vertico', I guess.  Actually, daviwil and
  ;; karthink in #systemcrafters showed me that Vertico does a kind of
  ;; frecency or recency itself, so `selectrum-prescient-mode' may not
  ;; be needed, or Vertico may be close enough.  Let's leave it on and
  ;; try it.
  :init (vertico-mode)
  :bind (:map vertico-map
              ("TAB" . vertico-next)
              ("<backtab>" . vertico-previous)
              ;; NOTE: Not completely satisfied with this binding.
              ("M-i" . vertico-insert)))

(use-package window
  :general ("C-x s" #'window-toggle-side-windows
            "C-x q" #'bury-buffer
            "C-x Q" #'unbury-buffer)
  :config
  (cl-defun ap/display-buffer-in-side-window (&optional (buffer (current-buffer))
                                                        &key (side 'right) (slot 0))
    "Display BUFFER in dedicated side window.
With universal prefix, use left SIDE instead of right.  With two
universal prefixes, prompt for side and slot (which allows
setting up an IDE-like layout)."
    (interactive (list (current-buffer)
                       :side (pcase current-prefix-arg
                               ('nil 'right)
                               ('(0) left)
                               (_ (intern (completing-read "Side: " '(left right top bottom) nil t))))
                       :slot (pcase current-prefix-arg
                               ('nil 0)
                               ('(0) 0)
                               (_ (read-number "Slot: ")))))
    (let ((display-buffer-mark-dedicated t))
      (display-buffer buffer
                      `(display-buffer-in-side-window
                        (side . ,side)
                        (slot . ,slot)
                        (window-parameters
			 (no-delete-other-windows . t))))))

  (defun ap/toggle-window-dedicated-p (&optional window)
    "Toggle WINDOW's dedicated flag.
Also set its `no-delete-other-windows' parameter to match."
    (interactive)
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (set-window-parameter window 'no-delete-other-windows
			  (window-dedicated-p window))
    (message "Dedicated: %s" (window-dedicated-p window)))

  (defun ap/set-window-parameter (window parameter value)
    "Set WINDOW's PARAMETER to VALUE.
Interactively (the whole point of this function), select from a
few common parameters with completion."
    (interactive (list (selected-window)
                       (intern
                        (completing-read
                         "Set window parameter: " '(no-delete-other-windows no-other-window side slot preserve-size)
                         nil t))
                       (read (read-string "Value: "))))
    (set-window-parameter window parameter value)))

;; Install Ement.
(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el")
  :custom
  (ement-auto-view-rooms '(("@alphapapa:matrix.org"
			    "#ement.el:matrix.org" "#org-mode:matrix.org"
			    "#emacs:matrix.org"
			    ;; #systemcrafters:libera.chat (which doesn't seem to have a canonical alias through the bridge)
			    "!EoRhMvNpnWxCMTMPeP:libera.chat")))
  (ement-room-prism 'both)
  (ement-room-send-message-filter #'ement-room-send-org-filter))

(use-package derpit
  :load-path ("~/src/emacs/misc/derpit.el")
  :custom ((derpit-fonts '("Comic-Sans-MS-Regular"
                           "DejaVu-Sans-Book"
                           "Hamish-Regular"
                           "edenshappell-Medium"
                           "KBTrueBeliever-Medium"
                           "KBGoogleyEyes-Medium"
                           "KBPancakeParty-Medium"
                           "KBGobbleDay-Medium"
                           "KBwhenpigsfly-Medium"
                           "Bookmark-Regular"
                           "Just-Alphabetty-Thing!-Thing!"))
           (derpit-avatar-dirs '("~/Dropbox/Images/Icons/twitch"
                                 "~/Dropbox/Images/Icons/avatars"))))

(use-package ement-derpit
  :load-path ("~/src/emacs/misc/derpit.el"))

;;; Footer

(find-file user-init-file)

;;; ap.el ends here
