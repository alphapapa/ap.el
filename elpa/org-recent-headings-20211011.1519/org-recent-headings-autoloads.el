;;; org-recent-headings-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-recent-headings" "org-recent-headings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-recent-headings.el

(defvar org-recent-headings-mode nil "\
Non-nil if Org-Recent-Headings mode is enabled.
See the `org-recent-headings-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-recent-headings-mode'.")

(custom-autoload 'org-recent-headings-mode "org-recent-headings" nil)

(autoload 'org-recent-headings-mode "org-recent-headings" "\
Global minor mode to keep a list of recently used Org headings so they can be quickly selected and jumped to.
With prefix argument ARG, turn on if positive, otherwise off.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-recent-headings" '("org-recent-headings"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-recent-headings-autoloads.el ends here
