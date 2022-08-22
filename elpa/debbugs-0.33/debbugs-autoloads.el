;;; debbugs-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "debbugs" "debbugs.el" (0 0 0 0))
;;; Generated autoloads from debbugs.el

(register-definition-prefixes "debbugs" '("debbugs-"))

;;;***

;;;### (autoloads nil "debbugs-browse" "debbugs-browse.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from debbugs-browse.el

(defconst debbugs-browse-gnu-url-regexp (format "^%s\\(%s\\)?\\([[:digit:]]+\\)$" "https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/" (regexp-quote "cgi/bugreport.cgi?bug=")) "\
A regular expression matching bug report URLs on GNU's debbugs instance.")

(autoload 'debbugs-browse-url "debbugs-browse" "\


\(fn URL &optional NEW-WINDOW)" nil nil)

(when (boundp 'browse-url-default-handlers) (add-to-list 'browse-url-default-handlers `(,debbugs-browse-gnu-url-regexp . debbugs-browse-url)))

(autoload 'debbugs-browse-mode "debbugs-browse" "\
Browse GNU Debbugs bug URLs with debbugs-gnu or debbugs-org.
With a prefix argument ARG, enable Debbugs Browse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.
The customer option `debbugs-browse-function' controls, which of
the two packages is used for showing bugs.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "debbugs-browse" '("debbugs-browse-"))

;;;***

;;;### (autoloads nil "debbugs-compat" "debbugs-compat.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from debbugs-compat.el

(register-definition-prefixes "debbugs-compat" '("debbugs-compat-string-replace"))

;;;***

;;;### (autoloads nil "debbugs-gnu" "debbugs-gnu.el" (0 0 0 0))
;;; Generated autoloads from debbugs-gnu.el

(autoload 'debbugs-gnu-search "debbugs-gnu" "\
Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

When using interactively, use \\[repeat-complex-command] after
this command for reusing the argument list.  Be careful in
editing the arguments, because the allowed attributes for QUERY
depend on PHRASE being a string, or nil.  See Info node
`(debbugs-ug) Searching Bugs'.

\(fn PHRASE &optional QUERY SEVERITIES PACKAGES ARCHIVEDP)" t nil)

(autoload 'debbugs-gnu-patches "debbugs-gnu" "\
List the bug reports that have been marked as containing a patch." t nil)

(autoload 'debbugs-gnu-tagged "debbugs-gnu" "\
List the bug reports that have been tagged locally." t nil)

(autoload 'debbugs-gnu-package "debbugs-gnu" "\
List the bug reports of default packages, divided by severity.

\(fn &optional PACKAGES)" t nil)

(autoload 'debbugs-gnu "debbugs-gnu" "\
List all outstanding bugs.

\(fn SEVERITIES &optional PACKAGES ARCHIVEDP SUPPRESS TAGS)" t nil)

(autoload 'debbugs-gnu-usertags "debbugs-gnu" "\
List all user tags for USERS, which is (\"emacs\") by default.

\(fn &rest USERS)" t nil)

(autoload 'debbugs-gnu-bugs "debbugs-gnu" "\
List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'.

\(fn &rest BUGS)" t nil)

(register-definition-prefixes "debbugs-gnu" '("debbugs-gnu-"))

;;;***

;;;### (autoloads nil "debbugs-org" "debbugs-org.el" (0 0 0 0))
;;; Generated autoloads from debbugs-org.el

(autoload 'debbugs-org-search "debbugs-org" "\
Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\"." t nil)

(autoload 'debbugs-org-patches "debbugs-org" "\
List the bug reports that have been marked as containing a patch." t nil)

(autoload 'debbugs-org-tagged "debbugs-org" "\
List the bug reports that have been tagged locally." t nil)

(autoload 'debbugs-org "debbugs-org" "\
List all outstanding bugs." t nil)

(autoload 'debbugs-org-mode "debbugs-org" "\
Minor mode for providing a debbugs interface in org-mode buffers.

This is a minor mode.  If called interactively, toggle the
`Debbugs-Org mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `debbugs-org-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{debbugs-org-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'debbugs-org-emacs-release-blocking-reports "debbugs-org" "\
Show the reports that are blocking an Emacs release." t nil)

(autoload 'debbugs-org-bugs "debbugs-org" "\
List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'." t nil)

(register-definition-prefixes "debbugs-org" '("debbugs-org-"))

;;;***

;;;### (autoloads nil nil ("debbugs-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; debbugs-autoloads.el ends here
