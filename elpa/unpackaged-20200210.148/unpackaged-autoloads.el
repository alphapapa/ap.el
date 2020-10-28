;;; unpackaged-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "unpackaged" "unpackaged.el" (0 0 0 0))
;;; Generated autoloads from unpackaged.el

(autoload 'unpackaged/font-compare "unpackaged" "\
Compare TEXT displayed in FONTS.
If TEXT is nil, use `lorem-ipsum' text.  FONTS is a list of font
family strings and/or font specs.

Interactively, prompt for TEXT, using `lorem-ipsum' if left
empty, and select FONTS with `x-select-font', pressing Cancel to
stop selecting fonts.

\(fn TEXT FONTS)" t nil)

(autoload 'unpackaged/ibuffer-toggle-all-filter-groups "unpackaged" "\
Toggle all filter groups.
With prefix, toggle `ibuffer-show-empty-filter-groups'.

\(fn TOGGLE-EMPTY)" t nil)

(autoload 'unpackaged/ibuffer-filter-group-move-down "unpackaged" "\
Move filter group at point down." t nil)

(autoload 'unpackaged/ibuffer-filter-group-move-up "unpackaged" "\
Move filter group at point up." t nil)

(autoload 'unpackaged/lorem-ipsum-overlay "unpackaged" "\
Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays.  Useful for taking
screenshots without revealing buffer contents.

Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid.  Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid.  For example, in an Org buffer, a line that starts
with:

  #+TITLE: unpackaged.el

could be matched against the exclude regexp (in `rx' syntax):

  (rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))

And the line would be overlaid like:

  #+TITLE: parturient.et" t nil)

(autoload 'unpackaged/org-agenda-current-subtree-or-region "unpackaged" "\
Display an agenda view for the current subtree or region.
 With prefix, display only TODO-keyword items.

\(fn ONLY-TODOS)" t nil)

(autoload 'unpackaged/org-agenda-toggle-preview "unpackaged" "\
Toggle overlay of current item in agenda." t nil)

(autoload 'unpackaged/package-org-docs "unpackaged" "\
Return documentation about PACKAGE as an Org string.
Interactively, place on kill ring.

\(fn &optional (PACKAGE (unpackaged/buffer-provides)))" t nil)

(autoload 'unpackaged/elisp-to-org "unpackaged" "\
Convert elisp code in region to Org syntax and put in kill-ring.
Extracts and converts docstring to Org text, and places code in
source block." t nil)

(autoload 'unpackaged/docstring-to-org "unpackaged" "\
Return DOCSTRING as formatted Org text.

Interactively, get text from region, and kill formatted Org text
to kill-ring.

\(fn DOCSTRING)" t nil)

(autoload 'unpackaged/caps-to-code "unpackaged" "\
Convert all-caps words in region to Org code emphasis.

\(fn BEG END)" t nil)

(autoload 'unpackaged/symbol-quotes-to-org-code "unpackaged" "\
Change Emacs `symbol' quotes to Org =symbol= quotes in region.

\(fn BEG END)" t nil)

(autoload 'unpackaged/org-attach-download "unpackaged" "\
Download file at URL and attach with `org-attach'.
Interactively, look for URL at point, in X clipboard, and in
kill-ring, prompting if not found.  With prefix, prompt for URL.

\(fn URL)" t nil)

(autoload 'unpackaged/org-fix-blank-lines "unpackaged" "\
Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers.

\(fn PREFIX)" t nil)

(autoload 'unpackaged/org-table-face-mode "unpackaged" "\
Apply `org-table' face family to all text in Org tables.
Useful for forcibly applying the face to portions of table data
that might have a different face, which could affect alignment.

If called interactively, enable Unpackaged/Org-Table-Face mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'unpackaged/def-org-maybe-surround "unpackaged" "\
Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'.

\(fn &rest KEYS)" nil t)

(autoload 'unpackaged/org-refile-to-datetree-using-ts-in-entry "unpackaged" "\
Refile current entry to datetree in FILE using timestamp found in entry.
WHICH should be `earliest' or `latest'. If SUBTREE-P is non-nil,
search whole subtree.

\(fn WHICH-TS FILE &optional SUBTREE-P)" t nil)

(autoload 'unpackaged/org-timestamps-in-entry "unpackaged" "\
Return timestamp objects for all Org timestamps in entry.
 If SUBTREE-P is non-nil (interactively, with prefix), search
 whole subtree.

\(fn &optional SUBTREE-P)" t nil)

(autoload 'unpackaged/org-refile-to-datetree "unpackaged" "\
Refile ENTRY or current node to entry for DATE in datetree in FILE.

\(fn FILE &key (DATE (calendar-current-date)) ENTRY)" t nil)

(autoload 'unpackaged/org-return-dwim "unpackaged" "\
A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table.

\(fn &optional DEFAULT)" t nil)

(autoload 'unpackaged/org-mark-read-only "unpackaged" "\
Mark all entries in the buffer tagged \"read_only\" with read-only text properties." t nil)

(autoload 'unpackaged/org-sort-multi "unpackaged" "\
Call `org-sort' until \\[keyboard-quit] is pressed." t nil)

(autoload 'unpackaged/quelpa-use-package-upgrade "unpackaged" "\
Eval the current `use-package' form with `quelpa-upgrade-p' true.
Deletes the package first to remove obsolete versions." t nil)

(autoload 'unpackaged/flex-fill-paragraph "unpackaged" "\
Fill paragraph, incrementing fill column to cause a change when repeated.
The global value of `fill-column' is not modified; it is only
bound around calls to `fill-paragraph'.

When called for the first time in a sequence, unfill to the
default `fill-column'.

When called repeatedly, increase `fill-column' until filling
changes.

With one universal prefix, increase `fill-column' until the
number of lines is reduced.  With two, unfill completely.

\(fn &optional FEWER-LINES UNFILL)" t nil)

(autoload 'unpackaged/iedit-scoped "unpackaged" "\
Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix.

\(fn ORIG-FN)" t nil)

(autoload 'unpackaged/iedit-or-flyspell "unpackaged" "\
Toggle `iedit-mode' or correct previous misspelling with `flyspell', depending on context.

With point in code or when `iedit-mode' is already active, toggle
`iedit-mode'.  With point in a comment or string, and when
`iedit-mode' is not already active, auto-correct previous
misspelled word with `flyspell'.  Call this command a second time
to choose a different correction." t nil)

(autoload 'unpackaged/sort-sexps "unpackaged" "\
Sort sexps in region.
Comments stay with the code below.

\(fn BEG END)" t nil)

(autoload 'unpackaged/query-replace-rx "unpackaged" "\
Call `query-replace-regexp', reading regexp in `rx' syntax.
Automatically wraps in parens and adds `seq' to the beginning of
the form.

\(fn &rest _)" t nil)

(autoload 'unpackaged/magit-status "unpackaged" "\
Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section." t nil)

(autoload 'unpackaged/magit-save-buffer-show-status "unpackaged" "\
Save buffer and show its changes in `magit-status'." t nil)

(autoload 'unpackaged/feed-for-url "unpackaged" "\
Return feed URL for web page at URL.
Interactively, insert the URL at point.  PREFER may be
`atom' (the default) or `rss'.  When ALL is non-nil, return all
feed URLs of all types; otherwise, return only one feed URL,
preferring the preferred type.

\(fn URL &key (PREFER \\='atom) (ALL nil))" t nil)

(register-definition-prefixes "unpackaged" '("unpackaged/"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unpackaged-autoloads.el ends here
