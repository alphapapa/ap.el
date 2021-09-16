;;; helm-org-ql-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-org-ql" "helm-org-ql.el" (0 0 0 0))
;;; Generated autoloads from helm-org-ql.el

(autoload 'helm-org-ql "helm-org-ql" "\
Display results in BUFFERS-FILES for an `org-ql' non-sexp query using Helm.
Interactively, search the current buffer.  Note that this command
only accepts non-sexp, \"plain\" queries.

NOTE: Atoms in the query are turned into strings where
appropriate, which makes it unnecessary to type quotation marks
around words that are intended to be searched for as indepenent
strings.

All query tokens are wrapped in the operator BOOLEAN (default
`and'; with prefix, `or').

For example, this raw input:

    Emacs git

Is transformed into this query:

    (and \"Emacs\" \"git\")

However, quoted strings remain quoted, so this input:

    \"something else\" (tags \"funny\")

Is transformed into this query:

    (and \"something else\" (tags \"funny\"))

\(fn BUFFERS-FILES &key (BOOLEAN \\='and) (NAME \"helm-org-ql\"))" t nil)

(autoload 'helm-org-ql-agenda-files "helm-org-ql" "\
Search agenda files with `helm-org-ql', which see." t nil)

(autoload 'helm-org-ql-org-directory "helm-org-ql" "\
Search Org files in `org-directory' with `helm-org-ql'." t nil)

(autoload 'helm-org-ql-views "helm-org-ql" "\
Show an `org-ql' view selected with Helm." t nil)

(autoload 'helm-org-ql-source "helm-org-ql" "\
Return Helm source named NAME that searches BUFFERS-FILES with `helm-org-ql'.

\(fn BUFFERS-FILES &key (NAME \"helm-org-ql\"))" nil nil)

(register-definition-prefixes "helm-org-ql" '("helm-org-ql-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-org-ql-autoloads.el ends here
