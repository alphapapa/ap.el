;;; org-now-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-now" "org-now.el" (0 0 0 0))
;;; Generated autoloads from org-now.el

(autoload 'org-now "org-now" "\
Focus `org-now' sidebar window, displaying it anew if necessary." t nil)

(autoload 'org-now-buffer "org-now" "\
Return the \"now\" buffer, creating it if necessary." nil nil)

(autoload 'org-now-link "org-now" "\
Add link to current Org entry to `org-now' entry.
This command works in any buffer that `org-store-link' can store
a link for, not just in Org buffers." t nil)

(autoload 'org-now-refile-to-now "org-now" "\
Refile current entry to the `org-now' entry." t nil)

(autoload 'org-now-refile-to-previous-location "org-now" "\
Refile current entry to its previous location.
Requires the entry to have a \"refiled_from\" property whose
value is a `read'able outline path list or an Org UUID.  The
property is removed after refiling." t nil)

(defalias 'org-now-agenda-link #'org-now-link)

(autoload 'org-now-agenda-refile-to-now "org-now" "\
Call `org-now-refile-to-now' from an Agenda buffer.
Also usable in `org-agenda-bulk-custom-functions'." t nil)

(autoload 'org-now-agenda-refile-to-previous-location "org-now" "\
Call `org-now-refile-to-previous-location' from an Agenda buffer.
Also usable in `org-agenda-bulk-custom-functions'." t nil)

(register-definition-prefixes "org-now" '("org-now-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-now-autoloads.el ends here
