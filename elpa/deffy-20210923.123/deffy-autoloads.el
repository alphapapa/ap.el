;;; deffy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "deffy" "deffy.el" (0 0 0 0))
;;; Generated autoloads from deffy.el

(autoload 'deffy "deffy" "\
Show definitions defined in PROJECT or FILES.
Interactively, with PREFIX, show only definitions in current
buffer.

\(fn &key (PROJECT (or (project-current) (cons \\='transient default-directory))) (KEYS deffy-taxy-default-keys) (FILES deffy-files) (BUFFER-NAME (format \"*Deffy: %s*\" (if files (string-join (mapcar #\\='file-relative-name files) \", \") (file-name-nondirectory (directory-file-name (project-root project)))))) VISIBILITY-FN DISPLAY-BUFFER-ACTION)" t nil)

(autoload 'deffy-buffer "deffy" "\
Show an Deffy view for BUFFER.
Interactively, with prefix, display in dedicated side window.

\(fn &optional (BUFFER (current-buffer)) &key DISPLAY-BUFFER-ACTION)" t nil)

(autoload 'deffy--bookmark-handler "deffy" "\
Show Deffy buffer for bookmark RECORD.

\(fn RECORD)" nil nil)

(register-definition-prefixes "deffy" '("deffy-" "file" "type"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; deffy-autoloads.el ends here
