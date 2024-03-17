;;; ini-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ini-mode" "ini-mode.el" (0 0 0 0))
;;; Generated autoloads from ini-mode.el

(autoload 'ini-mode "ini-mode" "\
Major mode for editing Windows-style ini files.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.ini\\'" . ini-mode))

(register-definition-prefixes "ini-mode" '("ini-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ini-mode-autoloads.el ends here
