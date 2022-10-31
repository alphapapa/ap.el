;;; bufler-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bufler" "bufler.el" (0 0 0 0))
;;; Generated autoloads from bufler.el

(autoload 'bufler-list "bufler" "\
Show Bufler's list.
With prefix argument ARG, force refreshing of buffers' VC state,
clear `bufler-cache', and regenerate buffer groups (which can be
useful after changing `bufler-groups' if the buffer list has not
yet changed).  With two universal prefix args, also show buffers
which are otherwise filtered by `bufler-filter-buffer-fns'.

\(fn &optional ARG)" t nil)

(defalias 'bufler #'bufler-list)

(autoload 'bufler-sidebar "bufler" "\
Display Bufler list in dedicated side window.
With universal prefix, use left SIDE instead of right.  With two
universal prefixes, prompt for side and slot.

\(fn &key (SIDE \\='right) (SLOT 0))" t nil)

(defalias 'bufler-switch-buffer #'bufler-workspace-switch-buffer)

(defalias 'bufler-mode #'bufler-workspace-mode)

(autoload 'bufler-defgroups "bufler" "\
Expand GROUPS into a group definition suitable for `bufler-groups'.
See documentation for details.

\(fn &rest GROUPS)" nil t)

(function-put 'bufler-defgroups 'lisp-indent-function 'defun)

(register-definition-prefixes "bufler" '("bufler-"))

;;;***

;;;### (autoloads nil "bufler-group-tree" "bufler-group-tree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bufler-group-tree.el

(register-definition-prefixes "bufler-group-tree" '("bufler-group-tree"))

;;;***

;;;### (autoloads nil "bufler-workspace" "bufler-workspace.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from bufler-workspace.el

(autoload 'bufler-workspace-frame-set "bufler-workspace" "\
Set workspace for the current frame to the one at PATH.
Interactively, choose workspace path with completion.  If PATH is
nil (interactively, with prefix), unset the frame's workspace.
Return the workspace path.

\(fn &optional PATH)" t nil)

(autoload 'bufler-workspace-focus-buffer "bufler-workspace" "\
Set current frame's workspace to BUFFER's workspace.
Interactively, use current buffer.

\(fn BUFFER)" t nil)

(autoload 'bufler-workspace-switch-buffer "bufler-workspace" "\
Switch to another buffer in the current group.
Without any input, switch to the previous buffer, like
`switch-to-buffer'.  If ALL-P (interactively, with universal
prefix) or if the frame has no workspace, select from all
buffers.  If SET-WORKSPACE-P (with two universal prefixes),
select from all buffers and set the frame's workspace.  If
NO-FILTER (with three universal prefixes), include buffers that
would otherwise be filtered by
`bufler-workspace-switch-buffer-filter-fns'.

If `bufler-workspace-switch-buffer-sets-workspace' is non-nil,
act as if SET-WORKSPACE-P is non-nil.

\(fn &optional ALL-P SET-WORKSPACE-P NO-FILTER)" t nil)

(autoload 'bufler-workspace-buffer-name-workspace "bufler-workspace" "\
Set current buffer's workspace to NAME.
If NAME is nil (interactively, with prefix), unset the buffer's
workspace name.  This sets the buffer-local variable
`bufler-workspace-name'.  Note that, in order for a buffer to
appear in a named workspace, the buffer must be matched by an
`auto-workspace' group before any other group.

\(fn &optional NAME)" t nil)

(defvar bufler-workspace-mode nil "\
Non-nil if Bufler-Workspace mode is enabled.
See the `bufler-workspace-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bufler-workspace-mode'.")

(custom-autoload 'bufler-workspace-mode "bufler-workspace" nil)

(autoload 'bufler-workspace-mode "bufler-workspace" "\
When active, set the frame title according to current Bufler group.

This is a minor mode.  If called interactively, toggle the
`Bufler-Workspace mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='bufler-workspace-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "bufler-workspace" '("bufler-workspace-"))

;;;***

;;;### (autoloads nil "bufler-workspace-tabs" "bufler-workspace-tabs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bufler-workspace-tabs.el

(defvar bufler-workspace-tabs-mode nil "\
Non-nil if Bufler-Workspace-Tabs mode is enabled.
See the `bufler-workspace-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bufler-workspace-tabs-mode'.")

(custom-autoload 'bufler-workspace-tabs-mode "bufler-workspace-tabs" nil)

(autoload 'bufler-workspace-tabs-mode "bufler-workspace-tabs" "\
Use Bufler workspaces for `tab-bar-mode' and `tab-line-mode'.

This is a minor mode.  If called interactively, toggle the
`Bufler-Workspace-Tabs mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='bufler-workspace-tabs-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(defalias 'bufler-tabs-mode #'bufler-workspace-tabs-mode)

(register-definition-prefixes "bufler-workspace-tabs" '("bufler-workspace-tabs"))

;;;***

;;;### (autoloads nil nil ("bufler-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bufler-autoloads.el ends here
