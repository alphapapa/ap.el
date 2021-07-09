;;; prism-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "prism" "prism.el" (0 0 0 0))
;;; Generated autoloads from prism.el

(autoload 'prism-mode "prism" "\
Disperse lisp forms (and other non-whitespace-sensitive syntax) into a spectrum of colors according to depth.
Depth is determined by list nesting.  Suitable for Lisp, C-like
languages, etc.

If called interactively, toggle `Prism mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'prism-whitespace-mode "prism" "\
Disperse whitespace-sensitive syntax into a spectrum of colors according to depth.
Depth is determined by indentation and list nesting.  Suitable
for Python, Haskell, etc.

If called interactively, toggle `Prism-Whitespace mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "prism" '("prism-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; prism-autoloads.el ends here
