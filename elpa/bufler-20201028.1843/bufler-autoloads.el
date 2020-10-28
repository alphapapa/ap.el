;;; bufler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bufler" "bufler.el" (0 0 0 0))
;;; Generated autoloads from bufler.el

(autoload 'bufler-list "bufler" "\
Show Bufler's list.
With prefix argument FORCE-REFRESH, force refreshing of buffers'
VC state, and clear `bufler-cache' and regenerate buffer
groups (which can be useful after changing `bufler-groups' if the
buffer list has not yet changed).

\(fn &optional FORCE-REFRESH)" t nil)

(defalias 'bufler #'bufler-list)

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
select from all buffers and set the frame's workspace.

If `bufler-workspace-switch-buffer-sets-workspace' is non-nil,
act as if SET-WORKSPACE-P is non-nil.

\(fn &optional ALL-P SET-WORKSPACE-P)" t nil)

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
When active, set the frame title according to current Mr. Buffer group.

If called interactively, enable Bufler-Workspace mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "bufler-workspace" '("bufler-workspace-"))

;;;***

;;;### (autoloads nil "bufler-workspace-tabs" "bufler-workspace-tabs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bufler-workspace-tabs.el

(when (require 'tab-bar nil t) (require 'tab-line) (defvar bufler-workspace-tabs-mode-saved-settings '((tab-bar-separator) (tab-bar-close-button-show)) "Settings saved from before `bufler-workspace-tabs-mode' was activated.\nUsed to restore them when the mode is disabled.") (defcustom bufler-workspace-tabs-tab-separator " | " "String displayed between tabs.\nSince there is no built-in separator between tabs, it can be\nunclear where one tab ends and the next begins, depending on face\nsettings.  Normally the tab-close button would indicate where a\ntab ends, but our tabs are dynamic, rule-generated workspaces and\naren't closable manually, so we repurpose the\n`tab-bar-close-button' as a separator.\n\nThis string can be anything, including an image using display\nproperties.  See the default value of `tab-bar-close-button'." :type 'string :group 'bufler-workspace) (define-minor-mode bufler-workspace-tabs-mode "Use Bufler workspaces for `tab-bar-mode' and `tab-line-mode'." :global t (if bufler-workspace-tabs-mode (progn (cl-loop for (symbol . _value) in bufler-workspace-tabs-mode-saved-settings do (setf (map-elt bufler-workspace-tabs-mode-saved-settings symbol) (symbol-value symbol))) (advice-add 'tab-bar-select-tab :override #'bufler-workspace-tabs--tab-bar-select-tab) (advice-add 'tab-bar-switch-to-tab :override #'bufler-workspace-frame-set) (setf tab-bar-tabs-function #'bufler-workspace-tabs tab-line-tabs-function #'bufler-workspace-buffers) (tab-bar-mode 1) (global-tab-line-mode 1) (setf tab-bar-separator bufler-workspace-tabs-tab-separator tab-bar-close-button-show nil)) (advice-remove 'tab-bar-select-tab #'bufler-workspace-tabs--tab-bar-select-tab) (advice-remove 'tab-bar-switch-to-tab #'bufler-workspace-frame-set) (setf tab-bar-tabs-function #'tab-bar-tabs tab-line-tabs-function #'tab-line-tabs-window-buffers) (cl-loop for (symbol . value) in bufler-workspace-tabs-mode-saved-settings do (set symbol value) do (setf (map-elt bufler-workspace-tabs-mode-saved-settings symbol) nil)) (tab-bar-mode -1) (global-tab-line-mode -1)) (force-mode-line-update 'all)) (defalias 'bufler-tabs-mode #'bufler-workspace-tabs-mode) (defun bufler-workspace-tabs--tab-bar-select-tab (&optional arg) "Set the frame's workspace to the selected tab's workspace.\nARG is the position of the tab in the tab bar." (interactive "P") (unless (integerp arg) (let ((key (event-basic-type last-command-event))) (setq arg (if (and (characterp key) (>= key 49) (<= key 57)) (- key 48) 1)))) (let* ((tabs (funcall tab-bar-tabs-function)) (from-index (tab-bar--current-tab-index tabs)) (to-index (1- (max 1 (min arg (length tabs)))))) (unless (eq from-index to-index) (let* ((_from-tab (tab-bar--tab)) (to-tab (nth to-index tabs)) (workspace-path (alist-get 'path to-tab))) (bufler-workspace-frame-set workspace-path) (force-mode-line-update 'all))))) (defun bufler-workspace-tabs (&optional frame) "Return a list of workspace tabs from FRAME's perspective.\nFRAME defaults to the selected frame.  Works as\n`tab-bar-tabs-function'." (with-selected-frame (or frame (selected-frame)) (cl-labels ((tab-type (path) (if (equal path (frame-parameter nil 'bufler-workspace-path)) 'current-tab 'tab)) (path-first (path) (cl-typecase path (string (list path)) (list (if (car path) (list (car path)) (list (cadr path)))))) (workspace-to-tab (workspace &optional type) (-let* (((&plist :name :path) workspace)) (list (or type (tab-type path)) (cons 'name (car name)) (cons 'path path)))) (path-top-level (path) (pcase-exhaustive path (`(,(and first (guard (not first))) ,(and second (guard second)) \, _rest) (ignore first second) (cl-subseq path 0 2)) (`(,first \, _rest) (list first)))) (path-to-workspace (path) (list :name (path-first path) :path path))) (let* ((bufler-vc-refresh nil) (buffer-paths (bufler-group-tree-paths (bufler-buffers))) (group-paths (mapcar #'butlast buffer-paths)) (top-level-paths (mapcar #'path-top-level group-paths)) (top-level-workspaces (mapcar #'path-to-workspace top-level-paths)) (unique-top-level-workspaces (seq-uniq top-level-workspaces #'equal)) (tabs (mapcar #'workspace-to-tab unique-top-level-workspaces))) (unless (cl-loop with current-path = (frame-parameter nil 'bufler-workspace-path) for tab in tabs for tab-path = (alist-get 'path tab) thereis (equal tab-path current-path)) (push (list 'current-tab (cons 'name (bufler-format-path (frame-parameter nil 'bufler-workspace-path))) (cons 'path (frame-parameter nil 'bufler-workspace-path))) tabs)) tabs)))))

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
