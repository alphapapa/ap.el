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
 '(avy-timeout-seconds 0.25)
 '(browse-url-browser-function 'eww-browse-url)
 '(bufler-columns '("Name" "Size" "Mode" "VC" "Path"))
 '(burly-before-open-bookmark-hook '(tab-bar-new-tab))
 '(comint-input-ignoredups t)
 '(comp-deferred-compilation t t)
 '(completion-cycle-threshold 5)
 '(completions-format 'vertical)
 '(custom-enabled-themes '(doom-solarized-dark))
 '(custom-safe-themes
   '("a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "f7216d3573e1bd2a2b47a2331f368b45e7b5182ddbe396d02b964b1ea5c5dc27" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "3e3a1caddeee4a73789ff10ba90b8394f4cd3f3788892577d7ded188e05d78f4" default))
 '(delete-by-moving-to-trash t)
 '(dired-omit-verbose nil)
 '(doom-outrun-electric-brighter-modeline t)
 '(doom-outrun-electric-comment-bg t)
 '(ement-save-sessions t)
 '(fci-rule-color "#62686E")
 '(frame-resize-pixelwise t)
 '(global-tab-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-sort-matches-functions-alist
   '((t . ivy--shorter-matches-first)
     (ivy-completion-in-region . ivy--shorter-matches-first)
     (ivy-switch-buffer . ivy-sort-function-buffer)
     (t . ivy--prefix-sort)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk 'all)
 '(magit-wip-after-apply-mode t)
 '(magit-wip-after-save-mode t)
 '(magit-wip-before-change-mode t)
 '(menu-bar-mode nil)
 '(minions-direct '(auto-revert-mode aggressive-indent-mode))
 '(minions-mode t)
 '(objed-cursor-color "#ff665c")
 '(org-agenda-files '("~/org/inbox.org"))
 '(org-catch-invisible-edits 'smart)
 '(org-ctrl-k-protect-subtree t)
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id t)
 '(org-id-locations-file-relative t)
 '(org-imenu-depth 8)
 '(org-log-into-drawer t)
 '(org-startup-folded t)
 '(org-sticky-header-full-path 'reversed)
 '(org-superstar-remove-leading-stars t)
 '(org-use-speed-commands t)
 '(package-selected-packages
   '(plz burly prism ement deffy org-ql embark geiser with-simulated-input buttercup inspector dtache macrostep unpackaged queue svg-lib taxy-magit-section taxy org-auto-expand org-sidebar helm-org-ql try htmlize modus-themes consult marginalia orderless vertico minions bufler helm-bufler snow which-key sr-speedbar org-sticky-header org-web-tools org-super-agenda doom-themes forge imenu-list magit-todos org-make-toc helm-org helm helm-core popup org-superstar org-now magit debbugs org-bullets spacemacs-theme highlight-function-calls scrollkeeper aggressive-indent general lispy magit-section pretty-hydra hydra lv f s dash-functional dash quelpa-use-package))
 '(pdf-view-midnight-colors (cons "#CBE3E7" "#1E1C31"))
 '(quelpa-checkout-melpa-p nil)
 '(quelpa-self-upgrade-p nil)
 '(recentf-mode t)
 '(rustic-ansi-faces
   ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF" "#bbc2cf"])
 '(safe-local-variable-values
   '((magit-todos-exclude-globs "elpa/")
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
 '(window-resize-pixelwise t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :family "Fantasque Sans Mono"))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(italic ((t (:slant italic))))
 '(org-meta-line ((t (:inherit font-lock-comment-face))))
 '(org-todo ((t (:foreground "#859900" :inverse-video t :box (:line-width (2 . 2) :color "dark red" :style flat-button) :weight bold))))
 '(scrollkeeper-guideline-highlight ((t (:extend t :background "#2aa198"))) t)
 '(tab-bar ((t (:inherit (header-line default) :background "#00212B" :foreground "#00212B" :box nil :weight bold :height 1.2))))
 '(tab-bar-tab ((t (:inherit (highlight tab-bar)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar))))
 '(tab-line ((t (:background "#00212B" :foreground "#00212B" :box nil :height 1.05)))))

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

(use-package general)

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

(use-package company
  :hook (prog-mode . company-mode))

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
  :bind (("C-." . embark-act)))

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
  :after magit
  :config (magit-todos-mode 1))

(use-package marginalia
  :init (marginalia-mode))

(use-package modus-themes
  :config
  (dolist (theme '(modus-operandi modus-vivendi))
    (unpackaged/customize-theme-faces theme
      '(ement-room-membership ((t :height 0.8)))
      `(ement-room-timestamp-header ((t :inherit header-line :weight bold :extend t)))
      '(font-lock-keyword-face ((t :weight bold)))
      '(font-lock-warning-face ((t :weight bold))))))

(use-package orderless
  :custom
  (completion-styles '(orderless flex)))

(use-package org
  :general
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
  :quelpa (org-ql :fetcher github :repo "alphapapa/org-ql"
	    :files (:defaults (:exclude "helm-org-ql.el"))))

(use-package org-sidebar
  :quelpa (org-sidebar :fetcher github :repo "alphapapa/org-sidebar"))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

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
	      ("<backtab>" . vertico-previous)))

(use-package window
  :general ("C-x s" #'window-toggle-side-windows)
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
      (display-buffer-in-side-window buffer
                                     `((side . ,side)
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
