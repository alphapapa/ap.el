;;; debbugs-org.el --- Org-mode interface for the GNU bug tracker  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hypermedia, maint, outlines
;; Package: debbugs

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to bug reports which are located
;; on the GNU bug tracker debbugs.gnu.org.  Its main purpose is to
;; show and manipulate bug reports as org-mode TODO items.

;; The bug tracker is called interactively by
;;
;;   M-x debbugs-org

;; It asks for the severities, for which bugs shall be shown. This can
;; be either just one severity, or a list of severities, separated by
;; comma.  Valid severities are "serious", "important", "normal",
;; "minor" or "wishlist".  Severities "critical" and "grave" are not
;; used, although configured on the GNU bug tracker.  If no severity
;; is given, all bugs are selected.

;; There is also the pseudo severity "tagged".  When it is used, the
;; function will ask for user tags (a comma separated list), and shows
;; just the bugs which are tagged with them.  In general, user tags
;; shall be strings denoting to subprojects of the package, like
;; "cedet" or "tramp" of the package "emacs".  If no user tag is
;; given, locally tagged bugs are shown.

;; If a prefix is given to the command, more search parameters are
;; asked for, like packages (also a comma separated list, "emacs" is
;; the default), whether archived bugs shall be shown, and whether
;; closed bugs shall be suppressed from being retrieved.

;; Another command is
;;
;;   M-x debbugs-org-search

;; It behaves like `debbugs-org', but asks at the beginning for a
;; search phrase to be used for full text search.  Additionally, it
;; asks for key-value pairs to filter bugs.  Keys are as described in
;; `debbugs-get-status', the corresponding value must be a regular
;; expression to match for.  The other parameters are as described in
;; `debbugs-org'.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, several downloads will
;; be performed, until all bugs are retrieved.

;; These default values could be changed also by customer options
;; `debbugs-gnu-default-severities' and `debbugs-gnu-default-packages'.

;; The commands create a TODO list.  Besides the usual handling of
;; TODO items, you could apply the following actions by the following
;; keystrokes:

;;   "C-c # C": Send a debbugs control message
;;   "C-c # E": Make (but don't yet send) a debbugs control message
;;   "C-c # t": Tag the bug locally
;;   "C-c # m": Mark the bug locally
;;   "C-c # d": Show bug attributes

;; The last entry in a TODO record is the link [[Messages]].  If you
;; follow this link, a Gnus ephemeral group or an Rmail buffer is
;; opened presenting all related messages for this bug.  Here you
;; could also send debbugs control messages by keystroke "C".

;; A special command to show bugs containing patches is
;;
;;   M-x debbugs-org-patches

;; This command shows all unarchived bugs of the packages declared in
;; `debbugs-gnu-default-packages', and tagged with "patch".  This is
;; useful for bug triages.

;; Another special command is
;;
;;    M-x debbugs-org-tagged

;; This command shows just the locally tagged bugs.

;; For the Emacs package, there is a special command, which shows
;; release critical bugs
;;
;;    M-x debbugs-org-emacs-release-blocking-reports

;; Finally, if you simply want to list some bugs with known bug
;; numbers, call the command
;;
;;   M-x debbugs-org-bugs

;; The bug numbers to be shown shall be entered as comma separated
;; list.  A bug number can also be a range of bugs like "123-456" or
;; "-10".  In the former case, all bugs from 123 until 456 are
;; presented, and in the latter case the last 10 bugs are shown,
;; counting from the highest bug number in the repository.

;;; Code:

(require 'debbugs-gnu)
(require 'org)
(eval-when-compile (require 'cl-lib))

;; Buffer-local variables.
(defvar debbugs-gnu-local-query)
(defvar debbugs-gnu-local-filter)

(defconst debbugs-org-severity-priority
  (let ((priority ?A))
    (mapcar
     (lambda (x) (prog1 (cons x (char-to-string priority)) (cl-incf priority)))
     debbugs-gnu-all-severities))
  "Mapping of debbugs severities to TODO priorities.")

(defun debbugs-org-get-severity-priority (state)
  "Returns the TODO priority of STATE."
  (or (cdr (assoc (alist-get 'severity state) debbugs-org-severity-priority))
      (cdr (assoc "minor" debbugs-org-severity-priority))))

(defconst debbugs-org-priority-faces
  '(("A" . org-warning)
    ("B" . org-warning))
  "Highlighting of prioritized TODO items.")

(defvar debbugs-org-buffer-name "*Org Bugs*"
  "The buffer name we present the bug reports.
This could be a temporary buffer, or a buffer linked with a file.")

;;;###autoload
(defun debbugs-org-search ()
  "Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\"."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively #'debbugs-gnu-search)))

;;;###autoload
(defun debbugs-org-patches ()
  "List the bug reports that have been marked as containing a patch."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively #'debbugs-gnu-patches)))

;;;###autoload
(defun debbugs-org-tagged ()
  "List the bug reports that have been tagged locally."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively 'debbugs-gnu-tagged)))

;;;###autoload
(defun debbugs-org ()
  "List all outstanding bugs."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively #'debbugs-gnu)))

(defun debbugs-org-show-reports ()
  "Show bug reports as retrieved via `debbugs-gnu-current-query'."
  (let ((inhibit-read-only t)
	(org-startup-folded t))
    (when (get-buffer debbugs-org-buffer-name)
      (kill-buffer debbugs-org-buffer-name))
    (switch-to-buffer (get-buffer-create debbugs-org-buffer-name))
    (org-mode)
    (debbugs-org-mode 1)

    (dolist (status
	     ;; `debbugs-get-status' returns in random order, so we must sort.
	     (sort
	      (apply #'debbugs-get-status
		     (debbugs-gnu-get-bugs debbugs-gnu-local-query))
	       (lambda (a b) (> (alist-get 'id a) (alist-get 'id b)))))
      (let* ((beg (point))
	     (id (alist-get 'id status))
	     (done (string-equal (alist-get 'pending status) "done"))
	     (priority (debbugs-org-get-severity-priority status))
	     (archived (alist-get 'archived status))
	     (tags (append (alist-get 'found_versions status)
			   (alist-get 'tags status)))
	     (subject (when (alist-get 'subject status)
			(decode-coding-string
			 (alist-get 'subject status) 'utf-8)))
	     (date (alist-get 'date status))
	     (last-modified (alist-get 'last_modified status))
	     (originator (when (alist-get 'originator status)
			   (decode-coding-string
			    (alist-get 'originator status) 'utf-8)))
	     (owner (when (alist-get 'owner status)
		      (decode-coding-string (alist-get 'owner status) 'utf-8)))
	     (closed-by (when (alist-get 'done status)
			  (decode-coding-string
			   (alist-get 'done status) 'utf-8)))
	     (merged (alist-get 'mergedwith status)))

	;; Handle tags.
	(when (string-match "^\\([0-9.]+\\); \\(.+\\)$" subject)
	  (let ((x (match-string 1 subject))) (cl-pushnew x tags :test #'equal))
	  (setq subject (match-string 2 subject)))
	(when archived
          (cl-pushnew "ARCHIVE" tags :test #'equal))
	(setq tags
	      (mapcar
	       ;; Replace all invalid TAG characters by "_".
	       (lambda (x) (replace-regexp-in-string "[^A-Za-z0-9_@]" "_" x))
	       tags))

	;; Headline.
	(insert
	 (format
	  "* %s [#%s] %s %s\n"
	  (if done "DONE" "TODO")
	  priority subject
	  (if tags (string-join (append '("") tags '("")) ":") "")))

	;; Submitted.
	(when date
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Submitted\n" (seconds-to-time date))))

	;; Properties.
	(insert "  :PROPERTIES:\n")
	(insert (format "  :DEBBUGS_ID: %s\n" id))
	(when merged
	  (insert
	   (format
	    "  :MERGED_WITH: %s\n"
	    (if (numberp merged)
		merged (mapconcat #'number-to-string merged " ")))))
	(insert (format "  :CREATOR: %s\n" originator))
	(when owner (insert (format "  :OWNER: %s\n" owner)))
	(when closed-by (insert (format "  :CLOSED_BY: %s\n" closed-by)))
	(insert "  :END:\n")

	;; Messages.
	(insert
	 "  [[elisp:(debbugs-gnu-select-report)][Messages]]\n")

	;; Last modified.
	(when last-modified
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Last modified\n"
	    (seconds-to-time last-modified))))

	;; Add text properties.
	(add-text-properties beg (point) `(tabulated-list-id ,status))))

    ;; The end.
    (insert "* COMMENT Local " "Variables\n"
	    "# Local " "Variables:\n"
	    "# mode: org\n"
	    "# eval: (debbugs-org-mode 1)\n"
	    "# End:\n")
    (goto-char (point-min))
    (org-overview)
    (set-buffer-modified-p nil)))

(defun debbugs-org-regenerate-status ()
  "Regenerate the `tabulated-list-id' text property.
This property is used when following the [Messages] link, so you
need to regenerate it when opening an .org file after you killed
the corresponding buffer (e.g. by closing Emacs)."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":DEBBUGS_ID:[ \t]*\\([0-9]+\\)" nil t)
      (let* ((bugnum (string-to-number (match-string 1)))
	     (mw (org-entry-get (point) "MERGEDWIDTH"))
	     (tli (list (cons 'id bugnum)
			(cons 'bug_num bugnum)
			(cons 'mergedwidth (if mw (string-to-number mw)))))
	    (beg (org-back-to-heading t))
	    (end (org-end-of-subtree t)))
	(add-text-properties beg end `(tabulated-list-id ,tli))))))

(defconst debbugs-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c # t") #'debbugs-gnu-toggle-tag)
    (define-key map (kbd "C-c # m") #'debbugs-gnu-toggle-mark)
    (define-key map (kbd "C-c # C") #'debbugs-gnu-send-control-message)
    (define-key map (kbd "C-c # E") #'debbugs-gnu-make-control-message)
    (define-key map (kbd "C-c # d") #'debbugs-gnu-display-status)
    map)
  "Keymap for the `debbugs-org-mode' minor mode.")

;; Make byte-compiler quiet.
(defvar gnus-posting-styles)

;;;###autoload
(define-minor-mode debbugs-org-mode
  "Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}"
  :lighter " Debbugs" :keymap debbugs-org-mode-map
  (set (make-local-variable 'debbugs-gnu-local-query) debbugs-gnu-current-query)
  (set (make-local-variable 'debbugs-gnu-local-filter)
       debbugs-gnu-current-filter)
  ;; FIXME: Does not show any effect.
  (set (make-local-variable 'org-priority-faces) debbugs-org-priority-faces)
  (set (make-local-variable 'gnus-posting-styles)
       `((".*"
	  (eval
	   (when (buffer-live-p gnus-article-copy)
	     (with-current-buffer gnus-article-copy
	       (set (make-local-variable 'message-prune-recipient-rules)
		    '((".*@debbugs.*" "emacs-pretest-bug")
		      (".*@debbugs.*" "bug-gnu-emacs")
		      ("[0-9]+@debbugs.*" "submit@debbugs.gnu.org")
		      ("[0-9]+@debbugs.*" "quiet@debbugs.gnu.org")))
	       ;; `gnus-posting-styles' is eval'ed after
	       ;; `message-simplify-subject'.  So we cannot use m-s-s.
	       (setq subject ,debbugs-gnu-subject)))))))
  (debbugs-org-regenerate-status))

;;;###autoload
(defun debbugs-org-emacs-release-blocking-reports ()
  "Show the reports that are blocking an Emacs release."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively #'debbugs-gnu-emacs-release-blocking-reports)))

;;;###autoload
(defun debbugs-org-bugs ()
  "List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (call-interactively #'debbugs-gnu-bugs)))

;; TODO

;; - Make headline customizable.
;; - Sort according to different TODO properties.

(provide 'debbugs-org)
