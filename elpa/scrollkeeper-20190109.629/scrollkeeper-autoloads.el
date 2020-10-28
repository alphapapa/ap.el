;;; scrollkeeper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "scrollkeeper" "scrollkeeper.el" (0 0 0 0))
;;; Generated autoloads from scrollkeeper.el

(autoload 'scrollkeeper-contents-up "scrollkeeper" "\
Scroll page contents up by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'.

\(fn &optional (LINES scrollkeeper-scroll-distance))" t nil)

(defalias 'scrollkeeper-down #'scrollkeeper-contents-up)

(autoload 'scrollkeeper-contents-down "scrollkeeper" "\
Scroll page contents down by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'.

\(fn &optional (LINES scrollkeeper-scroll-distance))" t nil)

(defalias 'scrollkeeper-up #'scrollkeeper-contents-down)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "scrollkeeper" '("scrollkeeper-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scrollkeeper-autoloads.el ends here
