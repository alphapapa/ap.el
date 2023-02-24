;;; org-superstar-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-superstar" "org-superstar.el" (0 0 0 0))
;;; Generated autoloads from org-superstar.el

(put 'org-superstar-leading-bullet 'safe-local-variable #'char-or-string-p)

(autoload 'org-superstar-toggle-lightweight-lists "org-superstar" "\
Toggle syntax checking for plain list items.

Disabling syntax checking will cause Org Superstar to display
lines looking like plain lists (for example in code) like plain
lists.  However, this may cause significant speedup for org files
containing several hundred list items." t nil)

(autoload 'org-superstar-mode "org-superstar" "\
Use UTF8 bullets for headlines and plain lists.

This is a minor mode.  If called interactively, toggle the
`Org-Superstar mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-superstar-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-superstar" '("org-superstar-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-superstar-autoloads.el ends here
