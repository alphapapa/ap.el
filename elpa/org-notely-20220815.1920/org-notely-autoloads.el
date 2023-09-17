;;; org-notely-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-notely" "org-notely.el" (0 0 0 0))
;;; Generated autoloads from org-notely.el

(autoload 'org-notely "org-notely" "\
Return buffer showing the `org-notely' heading with a new, empty note.
Return indirect buffer.  In the indirect buffer, \"RET\" is bound
to a function which renames the buffer to the first heading when
point is on a heading." t nil)

(register-definition-prefixes "org-notely" '("org-notely-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-notely-autoloads.el ends here
