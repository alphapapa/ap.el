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

If called interactively, enable Prism mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'prism-whitespace-mode "prism" "\
Disperse whitespace-sensitive syntax into a spectrum of colors according to depth.
Depth is determined by indentation and list nesting.  Suitable
for Python, Haskell, etc.

If called interactively, enable Prism-Whitespace mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

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
