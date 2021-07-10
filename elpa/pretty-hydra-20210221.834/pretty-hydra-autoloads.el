;;; pretty-hydra-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pretty-hydra" "pretty-hydra.el" (0 0 0 0))
;;; Generated autoloads from pretty-hydra.el

(autoload 'pretty-hydra-define "pretty-hydra" "\
Define a pretty hydra with given NAME, BODY options and HEADS-PLIST.
The generated hydra has a nice-looking docstring which is a table
with columns of command keys and hints.

NAME should be a symbol and is passed to `defhydra' as is.

BODY is the same as that in `defhydra', withe the following
pretty hydra specific ones:

  - `:separator' a single char used to generate the separator
    line.

  - `:title' a string that's added to the beginning of the
    docstring as a title of the hydra.

  - `:formatter' a function that takes the generated docstring
    and return a decorated one.  It can be used to further
    customize the hydra docstring.

  - `:quit-key' a key of list of keys for quitting the hydra.
    When specified, invisible head(s) are created with the
    specified keys for quitting the hydra.

HEADS-PLIST is a plist of columns of hydra heads.  The keys of
the plist should be column names.  The values should be lists of
hydra heads.  Each head has exactly the same syntax as that of
`defhydra', except hint is required for the head to appear in the
docstring.  The following additional options are supported:

  - `:width' the max width of a dynamic hint, used to calculate
    the final width of the entire column.  It is ignored when the
    hint is a string.

  - `:toggle' when specified, it makes the head a toggle and adds
    an indicator to the end of the hint for the status of the
    toggle.  The value of this option can be a symbol, an s-exp
    or t.  The toggle status is read from the given variable, by
    evaluating the given expression or checking the `cmd' as if
    it's a variable.  The latter is especially useful for minior
    modes, e.g.

       (\"n\" `linum-mode' \"line number\" :toggle t)

\(fn NAME BODY HEADS-PLIST)" nil t)

(function-put 'pretty-hydra-define 'lisp-indent-function 'defun)

(autoload 'pretty-hydra-define+ "pretty-hydra" "\
Redefine an existing pretty-hydra by adding new HEADS-PLIST.
If heads are added to a column already in NAME, the heads are
appended to that column.  Existing BODY is replaced with the new
one if specified.  Arguments are the same as `pretty-hydra-define'.

\(fn NAME BODY HEADS-PLIST)" nil t)

(function-put 'pretty-hydra-define+ 'lisp-indent-function 'defun)

(autoload 'pretty-hydra-toggle "pretty-hydra" "\
Create a dynamic hint that look like a radio button with given NAME.
Radio is considered on when STATUS is non-nil, otherwise off.

\(fn NAME STATUS)" nil nil)

(register-definition-prefixes "pretty-hydra" '("pretty-hydra-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pretty-hydra-autoloads.el ends here
