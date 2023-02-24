;;; org-bookmark-heading-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-bookmark-heading" "org-bookmark-heading.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-bookmark-heading.el

(autoload 'org-bookmark-heading-make-record "org-bookmark-heading" "\
Return bookmark record for current heading.
Sets ID property for heading if necessary." nil nil)

(autoload 'org-bookmark-heading-jump "org-bookmark-heading" "\
Jump to `org-bookmark-heading' BOOKMARK.
BOOKMARK record should have fields `map', `outline-path', and
`id', (and, for compatibility, `front-context-string' is also
supported, in which case it should be an entry ID).

\(fn BOOKMARK)" nil nil)

(define-obsolete-function-alias 'org-bookmark-jump 'org-bookmark-heading-jump "1.2")

(register-definition-prefixes "org-bookmark-heading" '("org-bookmark-heading-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-bookmark-heading-autoloads.el ends here
