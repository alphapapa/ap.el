;;; org-auto-expand-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-auto-expand" "org-auto-expand.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-auto-expand.el

(defvar org-auto-expand-mode nil "\
Non-nil if Org-Auto-Expand mode is enabled.
See the `org-auto-expand-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-auto-expand-mode'.")

(custom-autoload 'org-auto-expand-mode "org-auto-expand" nil)

(autoload 'org-auto-expand-mode "org-auto-expand" "\
Automatically expand certain headings when `org-mode' is activated.

This is a minor mode.  If called interactively, toggle the
`Org-Auto-Expand mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='org-auto-expand-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-auto-expand "org-auto-expand" "\
Set current buffer's outline visibility accordingly.
If STARTUP is non-nil (interactively, with prefix), call
`org-set-startup-visibility' first.

\(fn &optional STARTUP)" t nil)

(register-definition-prefixes "org-auto-expand" '("org-auto-expand-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-auto-expand-autoloads.el ends here
