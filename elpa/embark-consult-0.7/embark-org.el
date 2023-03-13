;;; embark-org.el --- Embark targets and actions for Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package configures the Embark package for use in Org Mode
;; buffers.  It teaches Embark a number of Org related targets and
;; appropriate actions.  Currently it has table cells, whole tables,
;; source blocks and links.  Targets to add: headings (Embark already
;; has generic support for outlines, so we just nee to add Org
;; specific actions), timestamps, etc.

;;; Code:

(require 'embark)
(require 'org)
(require 'org-element)

;;; Basic target finder for Org

;; There are very many org element and objects types, we'll only
;; recognize those for which there are specific actions we can put in
;; a keymap, or for even if there aren't any specific actions, if it's
;; import to be able to kill, delete or duplicate (embark-insert) them
;; conveniently.  I'll start conservatively and we can add more later

(defconst embark-org--types
  '(
    babel-call
    ;; bold
    ;; center-block
    ;; citation
    ;; citation-reference
    ;; clock
    ;; code
    ;; comment
    ;; comment-block
    ;; diary-sexp
    ;; drawer
    ;; dynamic-block
    ;; entity
    ;; example-block
    ;; export-block
    ;; export-snippet
    ;; fixed-width
    footnote-definition
    footnote-reference
    ;; headline ; the bounds include the entire subtree!
    ;; horizontal-rule
    ;; inline-babel-call
    ;; inline-src-block
    ;; inlinetask
    ;; italic
    item
    ;; keyword
    ;; latex-environment
    ;; latex-fragment
    ;; line-break
    link
    ;; macro
    ;; node-property
    ;; paragraph ; the existing general support seems fine
    plain-list
    ;; planning
    ;; property-drawer
    ;; quote-block
    ;; radio-target
    ;; section
    ;; special-block
    src-block
    ;; statistics-cookie
    ;; strike-through
    ;; subscript
    ;; superscript
    table ; supported via a specific target finder
    table-cell
    ;; table-row ; we'll put row & column actions in the cell map
    ;; target ; I think there are no useful actions for radio targets
    timestamp
    ;; underline
    ;; verbatim
    ;; verse-block
    )
  "Supported Org object and element types.")

(defun embark-org-target-element-context ()
  "Target all Org elements or objects around point."
  (when (derived-mode-p 'org-mode 'org-agenda-mode)
    (cl-loop
     for elt = (org-element-lineage (org-element-context) embark-org--types t)
     then (org-element-lineage elt embark-org--types)
     while elt
     for begin = (org-element-property :begin elt)
     for end = (org-element-property :end elt)
     for target = (buffer-substring begin end)
      ;; Adjust table-cell to exclude final |. (Why is that there?)
      ;; Note: We are not doing this is an embark transformer because we
      ;; want to adjust the bounds too.
      ;; TODO? If more adjustments like this become necessary, add a
      ;; nice mechanism for doing them.
      when (and (eq (car elt) 'table-cell) (string-suffix-p "|" target))
      do (setq target (string-trim (string-remove-suffix "|" target))
               end (1- end))
      collect `(,(intern (format "org-%s" (car elt))) ,target ,begin . ,end))))

(add-to-list 'embark-target-finders 'embark-org-target-element-context)

;;; Custom Org actions

(defvar org-export-with-toc)

(defun embark-org-copy-as-markdown (start end)
  "Export the region from START to END to markdown and save on the `kill-ring'."
  (interactive "r")
  (require 'ox)
  (kill-new
   (let (org-export-with-toc)
     (string-trim
      (org-export-string-as (buffer-substring-no-properties start end) 'md t))))
  (deactivate-mark))

(add-to-list 'embark-pre-action-hooks
             '(embark-org-copy-as-markdown embark--mark-target))

(keymap-set embark-region-map "M" #'embark-org-copy-as-markdown) ; good idea?

;;; Tables

(dolist (motion '(org-table-move-cell-up org-table-move-cell-down
                  org-table-move-cell-left org-table-move-cell-right))
  (add-to-list 'embark-repeat-actions motion))

(push 'embark--ignore-target
      (alist-get 'org-table-edit-field embark-target-injection-hooks))

(defvar-keymap embark-org-table-cell-map
  :doc "Keymap for actions the current cells, column or row of an Org table."
  :parent embark-general-map
  ;; TODO: default action?
  "<up>"    #'org-table-move-cell-up
  "<down>"  #'org-table-move-cell-down
  "<left>"  #'org-table-move-cell-left
  "<right>" #'org-table-move-cell-right
  "=" #'org-table-eval-formula
  "e" #'org-table-edit-field
  "g" #'org-table-recalculate)

(defvar-keymap embark-org-table-map
  :doc "Keymap for actions on entire Org table."
  :parent embark-general-map
  ;; TODO: default action?
  "=" #'org-table-edit-formulas
  "s" #'org-table-sort-lines
  "t" #'org-table-transpose-table-at-point
  "c" #'org-table-convert
  "f" #'org-table-follow-field-mode
  "y" #'org-table-paste-rectangle
  "d" #'org-table-toggle-formula-debugger
  "i" #'org-table-iterate
  "e" #'org-table-export)

(push 'embark--ignore-target            ; prompts for file name
      (alist-get 'org-table-export embark-target-injection-hooks))

(add-to-list 'embark-keymap-alist '(org-table . embark-org-table-map))

(add-to-list 'embark-keymap-alist '(org-table-cell . embark-org-table-cell-map))

;;; Links

;; The link support has a slightly complicated design in order to
;; achieve the following goals:

;; 1. RET should simply be org-open-at-point

;; 2. When the link is to a file, URL, email address or elisp
;;    expression or command, we want to offer the user actions for
;;    that underlying type.

;; 3. Even in those cases, we still want some actions to apply to the
;;    entire link including description: actions to copy the link as
;;    markdown, or just the link description or target.

;; So the strategy is as follows (illustrated with file links):

;; - The target will be just the file, without the description and
;;   also without the "file:" prefix nor the "::line-number or search"
;;   suffix.  That way, file actions will correctly apply to it.

;; - The type will not be 'file, but 'org-file-link that way we can
;;   register a keymap for 'org-file-link that inherits from both
;;   embark-org-link-map (with RET bound to org-open-at-point and a
;;   few other generic link actions) and embark-file-map.

;; - The commands to copy the link at point in some format will be
;;   written as commands that act on the Org link at point.  This way
;;   they are independently (plausibly) useful, and we circumvent the
;;   problem that the whole Org link is not actually the target (just
;;   the inner file is!).

;; Alternative design I considered: separate each target into two, a
;; whole link target which includes the description and brackets and
;; what not; and an "inner target" which is just the file or URL or
;; whatever.  Cons of this approach: much target cycling is required!
;; First of all, an unadorned embark-dwim definitely should be
;; org-open-at-point, which means the whole link target would need
;; priority. That means that any file, URL, etc. actions would require
;; you to cycle first.  This sounds very inconvenient, the above
;; slightly more complex design allows both whole-link and inner
;; target actions to work without cycling.

(autoload 'org-attach-dir "org-attach")

(defun embark-org--refine-link-type (_type target)
  "Refine type of link TARGET if we have more specific actions available."
  (when (string-match org-link-any-re target)
    (let ((target (or (match-string-no-properties 2 target)
                      (match-string-no-properties 0 target))))
      (cond
       ((string-prefix-p "http" target)
        (cons 'org-url-link target))
       ((string-prefix-p "mailto:" target)
        (cons 'org-email-link (string-remove-prefix "mailto:" target)))
       ((string-prefix-p "file:" target)
        (cons 'org-file-link
              (replace-regexp-in-string
               "::.*" "" (string-remove-prefix "file:" target))))
       ((string-prefix-p "attachment:" target)
        (cons 'org-file-link
              (expand-file-name
               (replace-regexp-in-string
                "::.*" "" (string-remove-prefix "attachment:" target))
               (org-attach-dir))))
       ((string-match-p "^[./]" target)
        (cons 'org-file-link (abbreviate-file-name (expand-file-name target))))
       ((string-prefix-p "elisp:(" target)
        (cons 'org-expression-link (string-remove-prefix "elisp:" target)))
       ((string-prefix-p "elisp:" target)
        (cons 'command (string-remove-prefix "elisp:" target)))
       (t (cons 'org-link target))))))

(add-to-list 'embark-transformer-alist
             '(org-link . embark-org--refine-link-type))

(defmacro embark-org-define-link-copier (name formula description)
  "Define a command that copies the Org link at point according to FORMULA.
The command's name is formed by appending NAME to
embark-org-copy-link.  The docstring includes the DESCRIPTION of
what part or in what format the link is copied."
  `(defun ,(intern (format "embark-org-copy-link-%s" name)) ()
     ,(format "Copy to the kill-ring the Org link at point%s." description)
     (interactive)
     (when (org-in-regexp org-link-any-re)
       (let* ((full (match-string-no-properties 0))
              (target (or (match-string-no-properties 2)
                          (match-string-no-properties 0)))
              (description (match-string-no-properties 3))
              (kill ,formula))
         (ignore full target description)
         (when kill
           (message "Saved '%s'" kill)
           (kill-new kill))))))

(embark-org-define-link-copier in-full full " in full")
(embark-org-define-link-copier description description "'s description")
(embark-org-define-link-copier target target "'s target")

(defalias 'embark-org-copy-link-inner-target #'kill-new
  "Copy inner part of the Org link at point's target.
For mailto and elisp links, the inner part is the portion of the
target after `mailto:' or `elisp:'.

For file links the inner part is the file name, without the
`file:' prefix and without `::' suffix (used for line numbers,
IDs or search terms).

For URLs the inner part is the whole target including the `http:'
or `https:' prefix.  For any other type of link the inner part is
also the whole target.")

(defvar-keymap embark-org-link-copy-map
  :doc "Keymap for different ways to copy Org links to the kill-ring.

You should bind w in this map to your most frequently used link
copying function.  The default is for w to copy the \"inner
target\" (see `embark-org-copy-link-inner-target'); which is also
bound to i."
  :parent nil
  "w" #'embark-org-copy-link-inner-target
  "f" #'embark-org-copy-link-in-full
  "d" #'embark-org-copy-link-description
  "t" #'embark-org-copy-link-target
  "i" #'embark-org-copy-link-inner-target
  "m" #'embark-org-copy-as-markdown)

(fset 'embark-org-link-copy-map embark-org-link-copy-map)

(defvar-keymap embark-org-link-map
  :doc "Keymap for actions on Org links."
  :parent embark-general-map
  "RET" #'org-open-at-point
  "'" #'org-insert-link
  "w" #'embark-org-link-copy-map)

;; The reason for this is left as an exercise to the reader.
;; Solution: Na ryvfc gnetrg znl cebzcg gur hfre sbe fbzrguvat!
(push 'embark--ignore-target
      (alist-get 'org-open-at-point embark-target-injection-hooks))

(push 'embark--ignore-target
      (alist-get 'org-insert-link embark-target-injection-hooks))

(add-to-list 'embark-keymap-alist
             '(org-link embark-org-link-map))
(add-to-list 'embark-keymap-alist
             '(org-url-link embark-org-link-map embark-url-map))
(add-to-list 'embark-keymap-alist
             '(org-email-link embark-org-link-map embark-email-map))
(add-to-list 'embark-keymap-alist
             '(org-file-link embark-org-link-map embark-file-map))
(add-to-list 'embark-keymap-alist
             '(org-expression-link embark-org-link-map embark-expression-map))

;;; Source blocks and babel calls

(defvar-keymap embark-org-src-block-map
  :doc "Keymap for actions on Org source blocks."
  :parent embark-general-map
  "RET" #'org-babel-execute-src-block
  "c" #'org-babel-check-src-block
  "k" #'org-babel-remove-result-one-or-many
  "p" #'org-babel-previous-src-block
  "n" #'org-babel-next-src-block
  "t" #'org-babel-tangle
  "s" #'org-babel-switch-to-session
  "l" #'org-babel-load-in-session
  "'" #'org-edit-special)

(dolist (motion '(org-babel-next-src-blockorg-babel-previous-src-block))
  (add-to-list 'embark-repeat-actions motion))

(add-to-list 'embark-keymap-alist '(org-src-block . embark-org-src-block-map))

;;; List items

(defvar-keymap embark-org-item-map
  :doc "Keymap for actions on Org list items."
  :parent embark-general-map
  "RET" #'org-toggle-checkbox
  "c" #'org-toggle-checkbox
  "t" #'org-toggle-item
  "n" #'org-next-item
  "p" #'org-previous-item
  "<left>" #'org-outdent-item
  "<right>" #'org-indent-item
  "<up>" #'org-move-item-up
  "<down>" #'org-move-item-down
  ">" #'org-indent-item-tree
  "<" #'org-outdent-item-tree)

(dolist (cmd '(org-toggle-checkbox
               org-toggle-item
               org-next-item
               org-previous-item
               org-outdent-item
               org-indent-item
               org-move-item-up
               org-move-item-down
               org-indent-item-tree
               org-outdent-item-tree))
  (add-to-list 'embark-repeat-actions cmd))

(add-to-list 'embark-keymap-alist '(org-item . embark-org-item-map))

;;; Org plain lists

(defvar-keymap embark-org-plain-list-map
  :doc "Keymap for actions on plain Org lists."
  :parent embark-general-map
  "RET" #'org-list-repair
  "r" #'org-list-repair
  "s" #'org-sort-list
  "b" #'org-cycle-list-bullet
  "t" #'org-list-make-subtree
  "c" #'org-toggle-checkbox)

(add-to-list 'embark-repeat-actions 'org-cycle-list-bullet)

(add-to-list 'embark-keymap-alist '(org-plain-list . embark-org-plain-list-map))

(cl-defun embark-org--toggle-checkboxes
    (&rest rest &key run type &allow-other-keys)
  "Around action hook for `org-toggle-checkbox'.
See `embark-around-action-hooks' for the keyword arguments RUN and TYPE.
REST are the remaining arguments."
  (apply (if (eq type 'org-plain-list) #'embark--mark-target run)
         :type type
         rest))

(cl-pushnew #'embark-org--toggle-checkboxes
            (alist-get 'org-toggle-checkbox embark-around-action-hooks))

;;; "Encode" region using Org export in place

(defvar-keymap embark-org-export-in-place-map
  :doc "Keymap for actions which replace the region by an exported version."
  :parent embark-general-map
  "m" #'org-md-convert-region-to-md
  "h" #'org-html-convert-region-to-html
  "a" #'org-ascii-convert-region-to-ascii
  "l" #'org-latex-convert-region-to-latex)

(fset 'embark-org-export-in-place-map embark-org-export-in-place-map)

(keymap-set embark-encode-map "o" 'embark-org-export-in-place-map)

(provide 'embark-org)
;;; embark-org.el ends here
