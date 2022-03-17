;;; org-sidebar-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-sidebar" "org-sidebar.el" (0 0 0 0))
;;; Generated autoloads from org-sidebar.el

(autoload 'org-sidebar "org-sidebar" "\
Display the Org Sidebar.

Interactively, display the sidebars configured in
`org-sidebar-default-fns'.

FNS should be one or a list of functions which return a buffer to
be displayed in the sidebar.  Each one is called with the current
buffer as its argument.

\(fn FNS)" t nil)

(autoload 'org-sidebar-toggle "org-sidebar" "\
Toggle default sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer." t nil)

(autoload 'org-sidebar-ql "org-sidebar" "\
Display a sidebar for `org-ql' QUERY.
Interactively, with prefix, prompt for these variables:

BUFFERS-FILES: A list of buffers and/or files to search.

NARROW: When non-nil, don't widen buffers before searching.

SUPER-GROUPS: An `org-super-agenda' grouping form.

SORT: One or a list of `org-ql' sorting functions, like `date' or
`priority'.

TITLE: Title for sidebar buffer.

\(fn BUFFERS-FILES QUERY &key NARROW SUPER-GROUPS SORT TITLE)" t nil)

(function-put 'org-sidebar-ql 'lisp-indent-function 'defun)

(autoload 'org-sidebar-backlinks "org-sidebar" "\
Show sidebar with entries that link to the current entry.
The entry must have an ID or CUSTOM_ID property; links to the
heading text are not found.  Note that searching for links to
entries that have both ID and CUSTOM_ID properties set is much
slower than searching for links to entries with just one of those
properties." t nil)

(autoload 'org-sidebar-tree "org-sidebar" "\
Show tree-view sidebar." t nil)

(autoload 'org-sidebar-tree-toggle "org-sidebar" "\
Toggle tree-view sidebar window.
If it is open and shows the view for the current buffer, delete
it.  Otherwise, show it for current buffer." t nil)

(autoload 'org-sidebar-tree-view-buffer "org-sidebar" "\
Return a tree-view buffer for BUFFER.

\(fn &key (BUFFER (current-buffer)) &allow-other-keys)" nil nil)

(register-definition-prefixes "org-sidebar" '("org-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-sidebar-autoloads.el ends here
