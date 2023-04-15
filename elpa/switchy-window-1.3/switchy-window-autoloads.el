;;; switchy-window-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "switchy-window" "switchy-window.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from switchy-window.el

(defvar switchy-window-minor-mode nil "\
Non-nil if Switchy-Window minor mode is enabled.
See the `switchy-window-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `switchy-window-minor-mode'.")

(custom-autoload 'switchy-window-minor-mode "switchy-window" nil)

(autoload 'switchy-window-minor-mode "switchy-window" "\
Activates recording of window selection ticks.
Those are the timestamps for figuring out the most-recently-used
order of windows.

This is a minor mode.  If called interactively, toggle the
`Switchy-Window minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='switchy-window-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The minor-mode provides the keymap `switchy-window-minor-mode-map',
which see.

\(fn &optional ARG)" t nil)

(autoload 'switchy-window "switchy-window" "\
Switch to other windows in most-recently-used order.
If prefix ARG is given, use least-recently-used order.

If the time between consecutive invocations is smaller than
`switchy-window-delay' seconds, selects one after the other window in
LRU order and cycles when all windows have been visited.  If
`switchy-window-delay' has passed, the current switching cycle ends and
the now selected window gets its tick updated (a kind of
timestamp).

\(fn &optional ARG)" t nil)

(register-definition-prefixes "switchy-window" '("switchy-window-"))

;;;***

;;;### (autoloads nil nil ("switchy-window-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; switchy-window-autoloads.el ends here
