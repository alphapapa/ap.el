;;; burly-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "burly" "burly.el" (0 0 0 0))
;;; Generated autoloads from burly.el

(autoload 'burly-open-last-bookmark "burly" "\
Open the last-opened Burly bookmark.
Helpful for, e.g. quickly restoring an overview while working on
a project." t nil)

(autoload 'burly-kill-buffer-url "burly" "\
Copy BUFFER's URL to the kill ring.

\(fn BUFFER)" t nil)

(autoload 'burly-kill-frames-url "burly" "\
Copy current frameset's URL to the kill ring." t nil)

(autoload 'burly-kill-windows-url "burly" "\
Copy current frame's window configuration URL to the kill ring." t nil)

(autoload 'burly-open-url "burly" "\
Open Burly URL.

\(fn URL)" t nil)

(autoload 'burly-bookmark-frames "burly" "\
Bookmark the current frames as NAME.

\(fn NAME)" t nil)

(autoload 'burly-bookmark-windows "burly" "\
Bookmark the current frame's window configuration as NAME.

\(fn NAME)" t nil)

(autoload 'burly-open-bookmark "burly" "\
Restore a window configuration to the current frame from a Burly BOOKMARK.

\(fn BOOKMARK)" t nil)

(autoload 'burly-bookmark-handler "burly" "\
Handler function for Burly BOOKMARK.

\(fn BOOKMARK)" nil nil)

(register-definition-prefixes "burly" '("burly-"))

;;;***

;;;### (autoloads nil "burly-tabs" "burly-tabs.el" (0 0 0 0))
;;; Generated autoloads from burly-tabs.el

(defvar burly-tabs-mode nil "\
Non-nil if Burly-Tabs mode is enabled.
See the `burly-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `burly-tabs-mode'.")

(custom-autoload 'burly-tabs-mode "burly-tabs" nil)

(autoload 'burly-tabs-mode "burly-tabs" "\
Integrate Burly with `tab-bar-mode'.
When active, Burly bookmarks are opened in new tabs and named
accordingly.

This is a minor mode.  If called interactively, toggle the
`Burly-Tabs mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='burly-tabs-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "burly-tabs" '("burly-"))

;;;***

;;;### (autoloads nil nil ("burly-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; burly-autoloads.el ends here
