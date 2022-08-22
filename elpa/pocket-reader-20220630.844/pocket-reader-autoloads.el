;;; pocket-reader-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pocket-reader" "pocket-reader.el" (0 0 0 0))
;;; Generated autoloads from pocket-reader.el

(autoload 'pocket-reader "pocket-reader" "\
Show Pocket reading list." t nil)

(autoload 'pocket-reader-add-link "pocket-reader" "\
Add link at point to Pocket.
This function tries to work in multiple major modes, such as w3m,
eww, elfeed, and Org." t nil)

(autoload 'pocket-reader-eww-add-link "pocket-reader" "\
Add link at point to Pocket in eww buffers." t nil)

(autoload 'pocket-reader-org-add-link "pocket-reader" "\
Add link at point to Pocket in Org buffers." t nil)

(with-eval-after-load 'w3m-lnum (cl-defun pocket-reader-w3m-lnum-add-link (&key (type 1)) "Add link to Pocket with lnum in w3m buffers." (interactive) (w3m-with-lnum type "" (when-let ((num (car (w3m-lnum-read-interactive "Anchor number: " 'w3m-lnum-highlight-anchor type last-index w3m-current-url))) (info (w3m-lnum-get-anchor-info num)) (url (car info))) (when (pocket-lib-add-urls url) (message "Added: %s" url))))))

(with-eval-after-load 'w3m (defun pocket-reader-w3m-add-link nil "Add link at point to Pocket in w3m buffers." (interactive) (if-let ((url (or (get-text-property (point) 'w3m-href-anchor) (unless (bolp) (save-excursion (get-text-property (1- (point)) 'w3m-href-anchor))) (unless (eolp) (save-excursion (get-text-property (1+ (point)) 'w3m-href-anchor))) (thing-at-point-url-at-point)))) (when (pocket-lib-add-urls url) (message "Added: %s" url)) (if (member 'w3m-lnum-mode minor-mode-list) (pocket-reader-w3m-lnum-add-link) (message "No URL found around point.")))))

(autoload 'pocket-reader-shr-add-link "pocket-reader" "\
Add link at point in `shr-mode' buffer to Pocket." t nil)

(with-eval-after-load 'elfeed (defun pocket-reader-elfeed-search-add-link nil "Add links for selected entries in Elfeed search-mode buffer to Pocket.\nThis is only for the elfeed-search buffer, not for entry buffers." (interactive) (when-let ((entries (elfeed-search-selected)) (links (mapcar #'elfeed-entry-link entries))) (when (pocket-lib-add-urls links) (message "Added: %s" (s-join ", " links))))) (defun pocket-reader-elfeed-entry-add-link nil "Add links for selected entries in elfeed-show-mode buffer to Pocket.\nThis is only for the elfeed-entry buffer, not for search buffers." (interactive) (when-let ((link (elfeed-entry-link elfeed-show-entry))) (when (pocket-lib-add-urls link) (message "Added: %s" link)))))

(autoload 'pocket-reader-generic-add-link "pocket-reader" "\
Try to add URL at point to Pocket using `thing-at-pt'." t nil)

(register-definition-prefixes "pocket-reader" '("pocket-reader-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pocket-reader-autoloads.el ends here
