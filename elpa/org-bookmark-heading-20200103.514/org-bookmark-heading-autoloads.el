;;; org-bookmark-heading-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-bookmark-heading" "org-bookmark-heading.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-bookmark-heading.el

(autoload 'org-bookmark-make-record "org-bookmark-heading" "\
Return alist for `bookmark-set' for current `org-mode'
heading.  Set org-id for heading if necessary." nil nil)

(autoload 'org-bookmark-jump "org-bookmark-heading" "\
Jump to BOOKMARK, where BOOKMARK is one whose
`front-context-string' is an org-id.

\(fn BOOKMARK)" nil nil)

(register-definition-prefixes "org-bookmark-heading" '("org-bookmark-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-bookmark-heading-autoloads.el ends here
