;;; debbugs-gnu.el --- interface for the GNU bug tracker  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2022 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hypermedia, maint
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
;; show and manipulate bug reports from Emacs, but it could be used
;; also for other GNU projects which use the same bug tracker.

;; The bug tracker is called interactively by
;;
;;   M-x debbugs-gnu

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

;; If you want to see all bugs for a given package, sorted by severity
;; and whether already resolved, call
;;
;;   M-x debbugs-gnu-package

;; Per default it shows the bugs for package "emacs", but with a
;; prefix given to the command, different package names can be
;; specified (comma-separated).

;; Another command is
;;
;;   M-x debbugs-gnu-search

;; It behaves like `debbugs-gnu', but asks at the beginning for a
;; search phrase to be used for full text search.  Additionally, it
;; asks for key-value pairs to filter bugs.  Keys are as described in
;; `debbugs-get-status', the corresponding value must be a regular
;; expression to match for.  The other parameters are as described in
;; `debbugs-gnu'.  Usually, there is just one value except for the
;; attribute "date", which needs two arguments specifying a period in
;; which the bug has been submitted or modified.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, several downloads will
;; be performed, until all bugs are retrieved.

;; These default values could be changed also by customer options
;; `debbugs-gnu-default-severities', `debbugs-gnu-default-packages'
;; and `debbugs-gnu-default-suppress-bugs'.

;; The commands create a page of bug lists.  Every bug is shown in one
;; line, including the bug number, the status (combining merged bug
;; numbers, keywords and severities), the name of the submitter, and
;; the title of the bug.  On every bug line you could apply the
;; following actions by the following keystrokes:

;;   RET: Show corresponding messages in Gnus/Rmail
;;   "C": Send a control message
;;   "E": Make (but don't yet send) a control message
;;   "t": Tag the bug locally
;;   "m": Mark the bug locally
;;   "b": Show bugs this bug is blocked by
;;   "B": Show bugs this bug is blocking
;;   "d": Show bug attributes

;; Furthermore, you could apply the global actions

;;   "g": Rescan bugs
;;   "q": Quit the buffer
;;   "s": Toggle bug sorting for age or for state
;;   "x": Toggle suppressing of bugs
;;   "/": Display only bugs matching a string
;;   "R": Display only bugs blocking the current release
;;   "w": Display all the currently selected bug reports
;;   "A": Show all messages from the currently shown bugs

;; When you visit the related bug messages in Gnus or Rmail, you could
;; also send or make control messages by keystroke "C" or "E" in the
;; message summary buffer.

;; In the header line of every bug list page, you can toggle sorting
;; per column by selecting a column with the mouse.  The sorting
;; happens as expected for the respective column; sorting in the Title
;; column is depending on whether you are the owner of a bug.

;; Another approach for listing bugs is calling the command
;;
;;   M-x debbugs-gnu-usertags

;; This command shows you all existing user tags for the packages
;; defined in `debbugs-gnu-default-packages'.  A prefix for the
;; command allows you to use other package names, or an arbitrary
;; string for a user who has tagged bugs.  The command returns the
;; list of existing user tags for the given user(s) or package
;; name(s), respectively.  Applying RET on a user tag, all bugs tagged
;; with this user tag are shown.

;; Unfortunately, it is not possible with the SOAP interface to show
;; all users who have tagged bugs.  This list can be retrieved via
;; <https://debbugs.gnu.org/cgi/pkgindex.cgi?indexon=users>.

;; A special command to show bugs containing patches is
;;
;;   M-x debbugs-gnu-patches

;; This command shows all unarchived bugs of the packages declared in
;; `debbugs-gnu-default-packages', and tagged with "patch".  This is
;; useful for bug triages.

;; Another special command is
;;
;;    M-x debbugs-gnu-tagged

;; This command shows just the locally tagged bugs.

;; For the Emacs package, there is a special command, which shows
;; release critical bugs
;;
;;    M-x debbugs-gnu-emacs-release-blocking-reports

;; Finally, if you simply want to list some bugs with known bug
;; numbers, call the command
;;
;;   M-x debbugs-gnu-bugs

;; The bug numbers to be shown shall be entered as comma separated
;; list.  A bug number can also be a range of bugs like "123-456" or
;; "-10".  In the former case, all bugs from 123 until 456 are
;; presented, and in the latter case the last 10 bugs are shown,
;; counting from the highest bug number in the repository.

;; For posting commit to bugs, or constructing a bug closing message
;; based on a pushed commit, use the command
;;
;;   M-x debbugs-gnu-pick-commits
;;
;; (bound to "c" in *vc-change-log* buffers).  Then follow the prompts.

;;; Code:

(require 'debbugs)
(require 'debbugs-compat)
(require 'tabulated-list)
(require 'add-log)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

(autoload 'article-decode-charset "gnus-art")
(autoload 'diff-goto-source "diff-mode")
(autoload 'diff-hunk-file-names "diff-mode")
(autoload 'gnus-article-mime-handles "gnus-art")
(autoload 'gnus-fetch-field "gnus-util")
(autoload 'gnus-read-ephemeral-bug-group "gnus-group")
(autoload 'gnus-read-ephemeral-emacs-bug-group "gnus-group")
(autoload 'gnus-summary-article-header "gnus-sum")
(autoload 'gnus-summary-select-article "gnus-sum")
(autoload 'gnus-summary-show-article "gnus-sum")
(autoload 'log-edit-done "log-edit")
(autoload 'log-edit-insert-changelog "log-edit")
(autoload 'mail-header-subject "nnheader")
(autoload 'message-add-header "message")
(autoload 'message-goto-body "message")
(autoload 'message-make-from "message")
(autoload 'message-narrow-to-headers "message")
(autoload 'rmail-get-new-mail "rmail")
(autoload 'rmail-show-message "rmail")
(autoload 'rmail-summary "rmailsum")
(autoload 'vc-dir-hide-up-to-date "vc-dir")
(autoload 'vc-dir-mark "vc-dir")
(autoload 'vc-git--call "vc-git")

(declare-function log-view-current-entry "log-view" (&optional pos move))
(declare-function log-view-current-tag "log-view" (&optional pos))

(defvar compilation-in-progress)
(defvar diff-file-header-re)
(defvar gnus-article-buffer)
(defvar gnus-bug-group-download-format-alist)
(defvar gnus-posting-styles)
(defvar gnus-save-duplicate-list)
(defvar gnus-suppress-duplicates)
(defvar mail-extr-ignore-realname-equals-mailbox-name)
(defvar mail-extr-ignore-single-names)
(defvar message-sent-message-via)
(defvar rmail-current-message)
(defvar rmail-mode-map)
(defvar rmail-summary-mode-map)
(defvar rmail-total-messages)

;; Buffer-local variables.
(defvar debbugs-gnu-local-query)
(defvar debbugs-gnu-local-filter)
(defvar debbugs-gnu-local-suppress)
(defvar debbugs-gnu-local-print-function)
(defvar debbugs-gnu-sort-state)
(defvar debbugs-gnu-limit)

(defgroup debbugs-gnu ()
  "UI for the debbugs.gnu.org bug tracker."
  :group 'debbugs
  :version "24.1")

(defcustom debbugs-gnu-default-severities '("serious" "important" "normal")
  "The list severities bugs are searched for.
\"tagged\" is not a severity but marks locally tagged bugs."
  ;; <https://debbugs.gnu.org/Developer.html#severities>
  ;; /ssh:debbugs:/etc/debbugs/config @gSeverityList
  ;; We don't use "critical" and "grave".
  :type '(set (const "serious")
	      (const "important")
	      (const "normal")
	      (const "minor")
	      (const "wishlist")
	      (const "tagged"))
  :version "24.1")

(defcustom debbugs-gnu-send-mail-function nil
  "A function to send control messages from debbugs.
If nil, the value of `send-mail-function' is used instead."
  :type '(radio (const :tag "Use `send-mail-function'" nil)
		(function-item message-send-mail-with-sendmail)
		(function-item message-smtpmail-send-it)
		(function-item mailclient-send-it)
		(function-item smtpmail-send-it)
		(function-item feedmail-send-it)
		(function-item :tag "Use Mailclient package"
			       message-send-mail-with-mailclient)
 		(function :tag "Other function"))
  :version "25.1")

(defcustom debbugs-gnu-compile-command "make -k"
  "Command to run to compile Emacs."
  :type 'string
  :version "28.1")

(defcustom debbugs-gnu-suppress-closed t
  "If non-nil, don't show closed bugs."
  :type 'boolean
  :version "25.1")

(defconst debbugs-gnu-all-severities
  (mapcar #'cadr (cdr (get 'debbugs-gnu-default-severities 'custom-type)))
  "List of all possible severities.")

(defconst debbugs-gnu-applicable-severities
  (remove "tagged" debbugs-gnu-all-severities)
  "List of all applicable severities.")

(defcustom debbugs-gnu-default-packages '("emacs")
  "The list of packages to be searched for."
  ;; <https://debbugs.gnu.org/Packages.html>
  ;; <https://debbugs.gnu.org/cgi/pkgindex.cgi>
  :type `(set (const "ada-mode")
	      (const "adns")
	      (const "anubis")
	      (const "auctex")
	      ;(const "autoconf")
	      (const "automake")
	      (const "automake-patches")
	      (const "cc-mode")
	      (const "coreutils")
	      (const "cppi")
	      (const "debbugs.gnu.org")
	      (const "dejagnu")
	      (const "diffutils")
	      (const "emacs")
	      (const "emacs-xwidgets")
	      (const "fm")
	      (const "gnus")
	      (const "gnuzilla")
	      (const "grep")
	      (const "guile")
	      (const "guix")
	      (const "guix-patches")
	      (const "gzip")
	      (const "hyperbole")
	      (const "idutils")
	      (const "libtool")
	      (const "mh-e")
	      (const "oo-browser")
	      (const "org-mode")
	      (const "parted")
	      (const "sed")
	      (const "skribilo")
	      (const ,(propertize
		      "spam"
		      'face 'debbugs-gnu-done
		      'help-echo "This is a pseudo package for spam."))
	      (const ,(propertize
		      "test"
		      'face 'debbugs-gnu-done
		      'help-echo "This is a pseudo package for test."))
	      (const "vc-dwim")
	      (const "woodchuck"))
  :version "28.1")

(defconst debbugs-gnu-all-packages
  (mapcar #'cadr (cdr (get 'debbugs-gnu-default-packages 'custom-type)))
  "List of all possible package names.")

(defconst debbugs-gnu-applicable-packages
  (remove "spam" debbugs-gnu-all-packages)
  "List of all applicable package names.")

(defcustom debbugs-gnu-default-suppress-bugs
  '((pending . "done"))
  "A list of specs for bugs to be suppressed.
An element of this list is a cons cell (KEY . REGEXP), with key
being returned by `debbugs-get-status', and REGEXP a regular
expression matching the corresponding value, a string.  Showing
suppressed bugs is toggled by `debbugs-gnu-toggle-suppress'."
  :type '(alist :key-type symbol :value-type regexp)
  :version "24.1")

(defcustom debbugs-gnu-mail-backend 'gnus
  "The email backend to use for reading bug report email exchange.
If this is `gnus', the default, use Gnus.
If this is `rmail', use Rmail instead."
  :type '(radio (function-item :tag "Use Gnus" gnus)
		(function-item :tag "Use Rmail" rmail))
  :version "25.1")

(defface debbugs-gnu-archived '((t (:inverse-video t)))
  "Face for archived bug reports.")

(defface debbugs-gnu-new '((t (:foreground "red")))
  "Face for new reports that nobody has answered.")

(defface debbugs-gnu-handled '((t (:foreground "ForestGreen")))
  "Face for reports that have been modified recently.")

(defface debbugs-gnu-stale-1 '((t (:foreground "#b0b000")))
  "Face for reports that have been touched two to four weeks ago.")

(defface debbugs-gnu-stale-2 '((t (:foreground "#c0c000")))
  "Face for reports that have been touched 4 weeks to 12 weeks ago.")

(defface debbugs-gnu-stale-3 '((t (:foreground "#d0d000")))
  "Face for reports that have been touched 12 weeks to 26 weeks ago.")

(defface debbugs-gnu-stale-4 '((t (:foreground "#e0e000")))
  "Face for reports that have been touched 26 weeks to 52 weeks ago.")

(defface debbugs-gnu-stale-5 '((t (:foreground "#ffff00")))
  "Face for reports that have been touched more than 52 weeks ago.")

(defface debbugs-gnu-pending '((t (:foreground "MidnightBlue")))
  "Face for reports that are pending.")

(defface debbugs-gnu-done '((t (:foreground "DarkGrey")))
  "Face for closed bug reports.")

(defface debbugs-gnu-forwarded '((t (:foreground "yellow")))
  "Face for forwarded bug reports.")

(defface debbugs-gnu-tagged '((t (:foreground "red")))
  "Face for reports that have been tagged locally.")

(defface debbugs-gnu-marked '((t (:background "DarkGrey")))
  "Face for reports that have been marked locally.")

(defface debbugs-gnu-title '((t (:height 1.2 :bold t)))
  "Face for titles.")

(defvar debbugs-gnu-local-tags nil
  "List of bug numbers tagged locally, and kept persistent.")

(defvar debbugs-gnu-local-marks nil
  "List of bug numbers marked locally, and kept persistent.")

(defvar debbugs-gnu-persistency-file
  (expand-file-name (locate-user-emacs-file "debbugs"))
  "File name of a persistency store for debbugs variables")

(defun debbugs-gnu-dump-persistency-file ()
  "Function to store debbugs variables persistently."
  (with-temp-file debbugs-gnu-persistency-file
    (insert
     ;; This could be `lisp-data' once we depend on Emacs 28+.
     ";; -*- emacs-lisp -*-\n"
     ";; Debbugs tags and marks history.  Don't change this file.\n\n"
     (format
      "(setq debbugs-gnu-local-tags '%S\n      debbugs-gnu-local-marks '%S)\n"
      (sort (copy-sequence debbugs-gnu-local-tags) #'>)
      (sort (copy-sequence debbugs-gnu-local-marks) #'>)))))

(defvar debbugs-gnu-current-query nil
  "The query object of the current search.
It will be applied server-side, when calling `debbugs-get-bugs'.
It has the same format as `debbugs-gnu-default-suppress-bugs'.")

(defvar debbugs-gnu-current-filter nil
  "The filter object for the current search.
It will be applied client-side, when parsing the results of
`debbugs-get-status'.  It has a similar format as
`debbugs-gnu-default-suppress-bugs'.  In case of keys representing
a date, value is the cons cell (BEFORE . AFTER).")

(defvar debbugs-gnu-current-suppress nil
  "Whether bugs shall be suppressed.
The specification which bugs shall be suppressed is taken from
  `debbugs-gnu-default-suppress-bugs'.")

(defvar debbugs-gnu-current-print-function #'tabulated-list-print
  "Which function to apply printing the tabulated list..
See `debbugs-gnu-package' for an alternative.")

(defcustom debbugs-gnu-emacs-current-release "28.1"
  "The current Emacs relase developped for."
  :type '(choice (const "24.5")
		 (const "25.1") (const "25.2")
		 (const "26.1") (const "26.3")
		 (const "27.1") (const "27.2")
		 (const "28.1"))
  :version "28.1")

(defconst debbugs-gnu-emacs-blocking-reports
  '(("24.5" . 19758)
    ("25.1" . 19759)
    ("25.2" . 21966)
    ("26.1" . 24655)
    ("26.3" . 35968)
    ("27.1" . 39200)
    ("27.2" . 43018)
    ("28.1" . 39202))
  "The IDs of the Emacs report used to track blocking bug reports.
It is a list of cons cells, each one containing the Emacs
version (a string) and the bug report number (a number).")

(defun debbugs-gnu-calendar-read (prompt acceptable &optional initial-contents)
  "Return a string read from the minibuffer.
Derived from `calendar-read'."
  (let ((value (read-string prompt initial-contents)))
    (while (not (funcall acceptable value))
      (setq value (read-string prompt initial-contents)))
    value))

(defconst debbugs-gnu-phrase-prompt
  (propertize
   "Enter search phrase: "
   'help-echo "\
The search phrase contains words to be searched for, combined by
operators like AND, ANDNOT and OR.  The phrase can also be empty,
in this case only the following attributes are used for search."))

;;;###autoload
(defun debbugs-gnu-search (phrase &optional query severities packages archivedp)
  "Search for Emacs bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned.  If a key cannot be queried by a SOAP request, it is
marked as \"client-side filter\".

When using interactively, use \\[repeat-complex-command] after
this command for reusing the argument list.  Be careful in
editing the arguments, because the allowed attributes for QUERY
depend on PHRASE being a string, or nil.  See Info node
`(debbugs-ug) Searching Bugs'."
  (interactive
   (let ((date-format
	  (eval-when-compile
	    (concat"\\([[:digit:]]\\{4\\}\\)-"
		   "\\([[:digit:]]\\{1,2\\}\\)-"
		   "\\([[:digit:]]\\{1,2\\}\\)")))
	 key val1 val2 phrase query severities packages archivedp)

     ;; Check for the phrase.
     (setq phrase (read-string debbugs-gnu-phrase-prompt))
     (when (zerop (length phrase))
       (setq phrase nil))

     ;; The other queries.
     (catch :finished
       (while t
	 (setq key (completing-read
		    "Enter attribute: "
		    (if phrase
			(append
			 '("severity" "package" "tags"
			   "author" "date" "subject")
			 ;; Client-side filters.
			 (mapcar
			  (lambda (key)
			    (propertize
			     key 'face 'debbugs-gnu-done
			     'help-echo "Client-side filter"))
			  '("status")))
		      (append
		       '("severity" "package" "archive" "src" "status" "tag"
			 "owner" "submitter" "maint" "correspondent")
		       ;; Client-side filters.
		       (mapcar
			(lambda (key)
			  (propertize
			   key 'face 'debbugs-gnu-done
			   'help-echo "Client-side filter"))
			'("date" "log_modified" "last_modified"
			  "found_date" "fixed_date" "unarchived"
			  "subject" "done" "forwarded" "msgid" "summary"))))
		    nil t))
	 (cond
	  ;; Server-side queries.
	  ((equal key "severity")
	   (setq
	    severities
	    (completing-read-multiple
	     "Enter severities: " debbugs-gnu-all-severities nil t
	     (string-join debbugs-gnu-default-severities ","))))

	  ((equal key "package")
	   (setq
	    packages
	    (completing-read-multiple
	     "Enter packages: " debbugs-gnu-applicable-packages nil t
	     (string-join debbugs-gnu-default-packages ","))))

	  ((equal key "archive")
	   ;; We simplify, by assuming just archived bugs are requested.
	   (setq archivedp t))

	  ((member key '("src" "tag" "tags"))
	   (setq val1 (read-string (format "Enter %s: " key)))
	   (when (not (zerop (length val1)))
	     (push (cons (intern key) val1) query)))

	  ((member
	    key '("author" "owner" "submitter" "maint" "correspondent"))
	   (setq val1 (read-string "Enter email address: "))
	   (when (not (zerop (length val1)))
	     (push
	      (cons (intern (if (equal key "author") "@author" key)) val1)
	      query)))

	  ;; Client-side filters.
	  ((equal key "status")
	   (setq
	    val1
	    (completing-read
	     (format "Enter status%s: "
		     (if (null phrase) "" " (client-side filter)"))
             (if (null phrase)
	         '("open" "forwarded" "done")
               '("pending" "forwarded" "fixed" "done"))
             nil t))
	   (when (not (zerop (length val1)))
	     (push (cons (if (null phrase) (intern key) 'pending) val1) query)))

	  ((member key '("date" "log_modified" "last_modified"
			 "found_date" "fixed_date" "unarchived"))
	   (setq val1
		 (debbugs-gnu-calendar-read
		  (format "Enter %s before YYYY-MM-DD%s: "
			  key (if phrase "" " (client-side filter)"))
		  (lambda (x)
		    (string-match (concat "^\\(" date-format "\\|\\)$") x))))
	   (if (string-match date-format val1)
	       (setq val1 (floor
			   (float-time
			    (encode-time
			     0 0 0
			     (string-to-number (match-string 3 val1))
			     (string-to-number (match-string 2 val1))
			     (string-to-number (match-string 1 val1))))))
	     (setq val1 nil))
	   (setq val2
		 (debbugs-gnu-calendar-read
		  (format "Enter %s after YYYY-MM-DD%s: "
			  key (if phrase "" " (client-side filter)"))
		  (lambda (x)
		    (string-match (concat "^\\(" date-format "\\|\\)$") x))))
	   (if (string-match date-format val2)
	       (setq val2 (floor
			   (float-time
			    (encode-time
			     0 0 0
			     (string-to-number (match-string 3 val2))
			     (string-to-number (match-string 2 val2))
			     (string-to-number (match-string 1 val2))))))
	     (setq val2 nil))
	   (when (or val1 val2)
	     (push
	      (cons (intern (if (and phrase (equal key "date")) "@cdate" key))
		    (cons val1 val2))
	      query)))

	  ;; "subject", "done", "forwarded", "msgid", "summary".
	  ((not (zerop (length key)))
	   (setq val1
		 (funcall
		  (if phrase 'read-string 'read-regexp)
		  (format "Enter %s%s: "
			  key (if phrase "" " (client-side filter)"))))
	   (when (not (zerop (length val1)))
	     (push (cons (intern key) val1) query)))

	  ;; The End.
	  (t (throw :finished nil)))))

     ;; The arguments.
     (list phrase query severities packages archivedp)))

  ;; We suppress closed bugs if there is no phrase.
  (setq debbugs-gnu-current-suppress
	(if (not debbugs-gnu-suppress-closed)
	    nil
	  (null phrase)))

  ;; Set phrase, query and filter.
  (when phrase
    (setq archivedp nil
          debbugs-gnu-current-query (list (cons 'phrase phrase))))
  (dolist (elt query)
    (add-to-list
     (if (memq
	  (car elt)
	  (if phrase
	      ;; Filters with phrase.
	      '(pending)
	    ;; Filters without phrase.
	    '(date log_modified last_modified found_date fixed_date unarchived
	      subject done forwarded msgid summary)))
	 'debbugs-gnu-current-filter 'debbugs-gnu-current-query)
     elt))

  ;; Do the search.
  (debbugs-gnu severities packages archivedp)
  (message "Search finished"))

;;;###autoload
(defun debbugs-gnu-patches ()
  "List the bug reports that have been marked as containing a patch."
  (interactive)
  (setq debbugs-gnu-current-suppress t)
  (debbugs-gnu nil debbugs-gnu-default-packages nil nil "patch"))

;;;###autoload
(defun debbugs-gnu-tagged ()
  "List the bug reports that have been tagged locally."
  (interactive)
  (debbugs-gnu '("tagged")))

;;;###autoload
(defun debbugs-gnu-package (&optional packages)
  "List the bug reports of default packages, divided by severity."
  (interactive
     (list
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: " debbugs-gnu-applicable-packages nil t
	   (string-join debbugs-gnu-default-packages ","))
	debbugs-gnu-default-packages)))

  (let ((debbugs-gnu-current-print-function
	 #'debbugs-gnu-package-tabulated-list-print))
    (debbugs-gnu debbugs-gnu-applicable-severities packages)))

(defun debbugs-gnu-package-tabulated-list-print ()
  "Print the tabulated list for `tramp-gnu-package'."
  (let ((inhibit-read-only t)
	(entries tabulated-list-entries)
	(packages
	 (delq nil
	       (mapcar
		(lambda (x) (when (eq (car x) 'package) (cdr x)))
		debbugs-gnu-local-query)))
	tabulated-list-entries)
    (kill-region (point-min) (point-max))
    (dolist (done '(t nil))
      (dolist (severity (reverse debbugs-gnu-applicable-severities))
	(setq tabulated-list-entries
	      (delq nil
		    (mapcar
		     (lambda (x)
		       (and (equal severity (alist-get 'severity (car x)))
			    (equal (if done "done" "pending")
				   (alist-get 'pending (car x)))
			    x))
		     entries)))
	(when tabulated-list-entries
	  (narrow-to-region (point-min) (point-max))
	  (tabulated-list-print nil 'update)
	  (goto-char (point-min))
	  (insert
	   (propertize
	    (format
	     "\n%s bugs - %s:\n\n"
	     (capitalize severity) (if done "resolved" "outstanding"))
	    'face 'debbugs-gnu-title))
	  (widen))))

    (when (member "emacs" packages)
      (when-let ((blockers
		  (alist-get
		   'blockedby
		   (car
		    (debbugs-get-status
		     (alist-get
		      debbugs-gnu-emacs-current-release
		      debbugs-gnu-emacs-blocking-reports nil nil #'equal))))))
	(setq tabulated-list-entries
	      (delq nil
		    (mapcar
		     (lambda (x)
		       (and (memq (alist-get 'id (car x)) blockers) x))
		     entries)))
	(when tabulated-list-entries
	  (narrow-to-region (point-min) (point-max))
	  (tabulated-list-print nil 'update)
	  (goto-char (point-min))
	  (insert
	   (propertize
	    (format "\nBugs blocking Emacs %s release\n\n"
		    debbugs-gnu-emacs-current-release)
	    'face 'debbugs-gnu-title))
	  (widen))))

    (goto-char (point-min))
    (insert
     (propertize
      (format "GNU bug reports: package(s) %s\n" (string-join packages ","))
      'face 'debbugs-gnu-title))))

(defvar debbugs-gnu-show-reports-function #'debbugs-gnu-show-reports
  "Which function to apply showing bug reports.
Shall be bound in `debbugs-org-*' functions.")

;;;###autoload
(defun debbugs-gnu (severities &optional packages archivedp suppress tags)
  "List all outstanding bugs."
  (interactive
   (let (severities archivedp)
     (list
      (setq severities
	    (completing-read-multiple
	     "Severities: " debbugs-gnu-all-severities nil t
	     (string-join debbugs-gnu-default-severities ",")))
      ;; The next parameters are asked only when there is a prefix.
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: " debbugs-gnu-applicable-packages nil t
	   (string-join debbugs-gnu-default-packages ","))
	debbugs-gnu-default-packages)
      (when current-prefix-arg
	(setq archivedp (y-or-n-p "Show archived bugs?")))
      (when (and current-prefix-arg (not archivedp))
	(y-or-n-p "Suppress unwanted bugs?"))
      ;; This one must be asked for severity "tagged".
      (when (member "tagged" severities)
	(split-string (read-string "User tag(s): ") "," t)))))

  (unwind-protect
      (progn
	;; Initialize variables.
	(when (and (file-exists-p debbugs-gnu-persistency-file)
		   (not debbugs-gnu-local-tags))
	  (with-temp-buffer
	    (insert-file-contents debbugs-gnu-persistency-file)
	    (eval (read (current-buffer)) t)))
	;; Per default, we suppress retrieved unwanted bugs.
	(when (and (called-interactively-p 'any)
		   debbugs-gnu-suppress-closed)
	  (setq debbugs-gnu-current-suppress t))

	;; Add queries.
	(dolist (severity (if (consp severities) severities (list severities)))
	  (when (not (zerop (length severity)))
	    (when (string-equal severity "tagged")
	      (setq debbugs-gnu-current-suppress nil))
	    (add-to-list 'debbugs-gnu-current-query (cons 'severity severity))))
	(dolist (package (if (consp packages) packages (list packages)))
	  (when (not (zerop (length package)))
	    (add-to-list 'debbugs-gnu-current-query (cons 'package package))))
	(when archivedp
	  (setq debbugs-gnu-current-suppress nil)
	  (add-to-list 'debbugs-gnu-current-query '(archive . "1")))
	(when suppress
	  (setq debbugs-gnu-current-suppress t)
	  (add-to-list 'debbugs-gnu-current-query '(status . "open"))
	  (add-to-list 'debbugs-gnu-current-query '(status . "forwarded")))
	(dolist (tag (if (consp tags) tags (list tags)))
	  (when (not (zerop (length tag)))
	    (add-to-list 'debbugs-gnu-current-query (cons 'tag tag))))

	;; Show result.
	(funcall debbugs-gnu-show-reports-function))

    ;; Reset query, filter and suppress.
    (setq debbugs-gnu-current-query nil
	  debbugs-gnu-current-filter nil
	  debbugs-gnu-current-suppress nil)
    (when (called-interactively-p 'interactive)
      (message "Query finished"))))

(defun debbugs-gnu-get-bugs (query)
  "Retrieve bug numbers from debbugs.gnu.org according search criteria."
  (let* ((debbugs-port "gnu.org")
	 (bugs (assq 'bugs query))
	 (tags (and (member '(severity . "tagged") query) (assq 'tag query)))
	 (local-tags (and (member '(severity . "tagged") query) (not tags)))
	 (phrase (assq 'phrase query))
	 args)
    ;; Compile query arguments.
    (unless (or query tags)
      (dolist (elt debbugs-gnu-default-packages)
	(setq args (append args (list :package elt)))))
    (dolist (elt query)
      (unless (equal elt '(severity . "tagged"))
	(setq args
	      (append
	       args
	       (if phrase
		   (cond
		    ((eq (car elt) 'phrase)
		     (list (list :phrase (cdr elt))))
		    ((memq (car elt) '(date @cdate))
		     (list (list (intern (concat ":" (symbol-name (car elt))))
				 (cddr elt) (cadr elt)
				 :operator "NUMBT")))
		    (t
		     (list (list (intern (concat ":" (symbol-name (car elt))))
				 (cdr elt) :operator "ISTRINC"))))
		 (list (intern (concat ":" (symbol-name (car elt))))
		       (cdr elt)))))))

    (cond
     ;; If the query is just a list of bug numbers, we return them.
     (bugs (cdr bugs))
     ;; If the query contains the pseudo severity "tagged", we return
     ;; just the local tagged bugs.
     (local-tags (copy-sequence debbugs-gnu-local-tags))
     ;; A full text query.
     (phrase
      (mapcar
       (lambda (x) (cdr (assoc "id" x)))
       (apply #'debbugs-search-est args)))
     ;; User tags.
     (tags
      (setq args (mapcar (lambda (x) (if (eq x :package) :user x)) args))
      (apply #'debbugs-get-usertag args))
     ;; Otherwise, we retrieve the bugs from the server.
     (t (apply #'debbugs-get-bugs args)))))

(defun debbugs-gnu--split-address (string)
  "Split mail-like STRING into a name/email address pair."
  (if (string-match "\\`\\(.*\\) <\\([^>]+\\)>\\'" string)
      (let ((name (match-string 1 string))
	    (email (match-string 2 string)))
	;; Remove leading/trailing quote chars.
	(cons email
	      (replace-regexp-in-string "\\`\"\\|\"\\'" "" name)))
    (cons string string)))

(defun debbugs-gnu-show-reports (&optional offline)
  "Show bug reports.
If OFFLINE is non-nil, the query is not sent to the server.  Bugs
are taken from the cache instead."
  (let* ((inhibit-read-only t)
	 string
	 (buffer-name
	  (cond
	   ((setq string (alist-get 'phrase debbugs-gnu-current-query))
	    (format "*%S Bugs*" string))
	   ((setq string (alist-get 'package debbugs-gnu-current-query))
	    (format "*%s Bugs*" (capitalize string)))
	   (t "*Bugs*"))))
    ;; The tabulated mode sets several local variables.  We must get
    ;; rid of them.
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (pop-to-buffer-same-window (get-buffer-create buffer-name))
    (debbugs-gnu-mode)

    ;; Print bug reports.
    (dolist (status
	     (sort
	      (let ((debbugs-cache-expiry (if offline nil debbugs-cache-expiry))
		    ids)
		(apply #'debbugs-get-status
		       (if offline
			   (progn
			     (maphash (lambda (key _elem)
					(push key ids))
				      debbugs-cache-data)
			     ids)
			 (debbugs-gnu-get-bugs debbugs-gnu-local-query))))
	      ;; Sort so that if a new report gets merged with an old
	      ;; report, it shows up under the new report.
	      (lambda (s1 s2)
		(> (alist-get 'id s1) (alist-get 'id s2)))))
      (let* ((id (alist-get 'id status))
	     (words (cons (alist-get 'severity status)
			  (alist-get 'keywords status)))
	     (address (if (alist-get 'originator status)
			  (debbugs-gnu--split-address
			   (decode-coding-string
			    (alist-get 'originator status) 'utf-8))))
	     (owner (if (alist-get 'owner status)
			(car (debbugs-gnu--split-address
			      (decode-coding-string
			       (alist-get 'owner status) 'utf-8)))))
	     (subject (if (alist-get 'subject status)
			  (decode-coding-string
			   (alist-get 'subject status) 'utf-8)))
	     (age (- (float-time) (or (alist-get 'log_modified status) 0)))
	     (week (* 60 60 24 7))
	     merged)
	(unless (equal (alist-get 'pending status) "pending")
	  (setq words (append words (list (alist-get 'pending status)))))
	(when (alist-get 'fixed status)
	  (setq words (append words '("fixed"))))
	(let ((packages (alist-get 'package status)))
	  (dolist (elt packages)
	    (when (member elt debbugs-gnu-default-packages)
	      (setq packages (delete elt packages))))
	  (setq words (append words packages)))
	(when (setq merged (alist-get 'mergedwith status))
	  (setq words (append (mapcar #'number-to-string merged) words)))
	(setq merged (sort merged #'>))
	;; `words' could contain the same word twice, for example
	;; "fixed" from `keywords' and `pending'.
	(setq words
	      (string-join (cl-delete-duplicates words :test #'equal) ","))
	(when (or (not merged)
		  (not (let (found)
			 (dolist (id (if (listp merged)
					 merged
				       (list merged)))
			   (dolist (entry tabulated-list-entries)
			     (when (equal id (alist-get 'id (car entry)))
			       (setq found t))))
			 found)))
	  (add-to-list
	   'tabulated-list-entries
	   (list
	    status
	    (vector
	     (propertize
	      (format "%5d" id)
	      'face
	      ;; Mark tagged bugs.
	      (if (memq id debbugs-gnu-local-tags)
		  'debbugs-gnu-tagged
		'default))
	     (propertize
	      ;; Mark status and age.
	      (or words "")
	      'face
	      (cond
	       ((alist-get 'archived status)
		'debbugs-gnu-archived)
	       ((equal (alist-get 'pending status) "done")
		'debbugs-gnu-done)
	       ((equal (alist-get 'pending status) "forwarded")
		'debbugs-gnu-forwarded)
	       ((member "pending" (alist-get 'keywords status))
		'debbugs-gnu-pending)
	       ;; For some new bugs `date' and `log_modified' may
	       ;; differ in 1 second.
	       ((< (abs (- (alist-get 'date status)
			   (alist-get 'log_modified status)))
		   3)
		'debbugs-gnu-new)
	       ((< age (* week 2)) 'debbugs-gnu-handled)
	       ((< age (* week 4)) 'debbugs-gnu-stale-1)
	       ((< age (* week 12)) 'debbugs-gnu-stale-2)
	       ((< age (* week 26)) 'debbugs-gnu-stale-3)
	       ((< age (* week 52)) 'debbugs-gnu-stale-4)
	       (t
		'debbugs-gnu-stale-5)))
	     (propertize
	      ;; Prefer the name over the address.
	      (or (cdr address)
		  (car address)
		  "")
	      'face
	      ;; Mark own submitted bugs.
	      (if (and (stringp (car address))
		       (string-equal (car address) user-mail-address))
		  'debbugs-gnu-tagged
		'default))
	     (propertize
	      (or subject "")
	      'face
	      (cond
	       ;; Marked bugs.
	       ((memq id debbugs-gnu-local-marks)
		'debbugs-gnu-marked)
	       ;; Mark owned bugs.
	       ((and (stringp owner)
		     (string-equal owner user-mail-address))
		'debbugs-gnu-tagged)
	       (t 'default)))))
	   'append))))

    (tabulated-list-init-header)
    (funcall debbugs-gnu-local-print-function)

    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun debbugs-gnu-print-entry (list-id cols)
  "Insert a debbugs entry at point.
Used instead of `tabulated-list-print-entry'."
  (let ((beg (point))
	(pos 0)
	(case-fold-search t)
	(id               (aref cols 0))
	(id-length        (nth 1 (aref tabulated-list-format 0)))
	(state            (aref cols 1))
	(state-length     (nth 1 (aref tabulated-list-format 1)))
	(submitter        (aref cols 2))
	(submitter-length (nth 1 (aref tabulated-list-format 2)))
	(title            (aref cols 3))
	;; (title-length     (nth 1 (aref tabulated-list-format 3)))
        )
    (when (and
	   ;; We may have a narrowing in effect.
	   (or (not debbugs-gnu-limit)
	       (memq (alist-get 'id list-id) debbugs-gnu-limit))
	   ;; Filter suppressed bugs.
	   (or (not debbugs-gnu-local-suppress)
	       (not (catch :suppress
		      (dolist (check debbugs-gnu-default-suppress-bugs)
			(when (string-match
			       (cdr check) (alist-get (car check) list-id ""))
			  (throw :suppress t))))))
	   ;; Filter search list.
	   (not (catch :suppress
		  (dolist (check debbugs-gnu-local-filter)
		    (let ((val (alist-get (car check) list-id)))
		      (if (stringp (cdr check))
			  ;; Regular expression.
			  (when (not (string-match (cdr check) (or val "")))
			    (throw :suppress t))
			;; Time value.
			(when (or (and (numberp (cadr check))
				       (< (cadr check) val))
				  (and (numberp (cddr check))
				       (> (cddr check) val)))
			  (throw :suppress t))))))))

      ;; Insert id.
      (indent-to (- id-length (length id)))
      (insert id)
      ;; Insert state.
      (indent-to (setq pos (+ pos id-length 1)) 1)
      (insert (if (> (length state) state-length)
		  (propertize (substring state 0 state-length)
			      'help-echo state)
		state))
      ;; Insert submitter.
      (indent-to (setq pos (+ pos state-length 1)) 1)
      (insert (if (> (length submitter) submitter-length)
		  (propertize (substring submitter 0 submitter-length)
			      'help-echo submitter)
		submitter))
      (indent-to (+ pos (1- submitter-length)))
      ;; Insert title.
      (indent-to (setq pos (+ pos submitter-length 1)) 1)
      (insert (propertize title 'help-echo title))
      ;; Add properties.
      (add-text-properties
       beg (point)
       `(tabulated-list-id ,list-id
	 tabulated-list-entry ,cols
	 mouse-face highlight))
      (insert ?\n))))

(defun debbugs-gnu-menu-map-emacs-enabled ()
  "Whether \"Show Release Blocking Bugs\" is enabled in the menu."
  (or ;; No package discriminator has been used.
      (not (assq 'package debbugs-gnu-local-query))
      ;; Package "emacs" has been selected.
      (member '(package . "emacs") debbugs-gnu-local-query)))

(defun debbugs-gnu-manual ()
  "Display the Debbugs manual in Info mode."
  (interactive)
  (info "debbugs-ug"))

(defconst debbugs-gnu-bug-triage-file
  (expand-file-name "../admin/notes/bug-triage" data-directory)
  "The \"bug-triage\" file.")

(defun debbugs-gnu-menu-map-bug-triage-enabled ()
  "Whether \"Describe Bug Triage Procedure\" is enabled in the menu."
  (and (debbugs-gnu-menu-map-emacs-enabled)
       (stringp debbugs-gnu-bug-triage-file)
       (file-readable-p debbugs-gnu-bug-triage-file)))

(defun debbugs-gnu-view-bug-triage ()
  "Show \"bug-triage\" file."
  (interactive)
  (view-file debbugs-gnu-bug-triage-file))

(defvar debbugs-gnu-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'debbugs-gnu-select-report)
    (define-key map [mouse-2] #'debbugs-gnu-select-report)
    (define-key map "A" #'debbugs-gnu-select-current-bugs)
    (define-key map "g" #'debbugs-gnu-rescan)
    (define-key map "R" #'debbugs-gnu-show-all-blocking-reports)
    (define-key map "C" #'debbugs-gnu-send-control-message)
    (define-key map "E" #'debbugs-gnu-make-control-message)

    (define-key map "s" #'debbugs-gnu-toggle-sort)
    (define-key map "t" #'debbugs-gnu-toggle-tag)
    (define-key map "m" #'debbugs-gnu-toggle-mark)
    (define-key map "x" #'debbugs-gnu-toggle-suppress)
    (define-key map "/" #'debbugs-gnu-narrow-to-status)
    (define-key map "w" #'debbugs-gnu-widen)

    (define-key map "b" #'debbugs-gnu-show-blocked-by-reports)
    (define-key map "B" #'debbugs-gnu-show-blocking-reports)
    (define-key map "d" #'debbugs-gnu-display-status)

    (define-key map [menu-bar debbugs] (cons "Debbugs" menu-map))
    (define-key menu-map [debbugs-gnu-select-report]
      '(menu-item "Show Reports" debbugs-gnu-select-report
		  :help "Show all reports belonging to this bug"))
    (define-key-after menu-map [debbugs-gnu-select-current]
      '(menu-item "Show Reports For All" debbugs-gnu-select-current-bugs
		  :help "Show reports for all currently shown bugs")
      'debbugs-gnu-select-report)
    (define-key-after menu-map [debbugs-gnu-rescan]
      '(menu-item "Refresh Bugs" debbugs-gnu-rescan
		  :help "Refresh bug list")
      'debbugs-gnu-select-current)
    (define-key-after menu-map [debbugs-gnu-show-all-blocking-reports]
      '(menu-item "Show Release Blocking Bugs"
		  debbugs-gnu-show-all-blocking-reports
		  :enable (debbugs-gnu-menu-map-emacs-enabled)
		  :help "Show all bugs blocking next Emacs release")
      'debbugs-gnu-rescan)
    (define-key-after menu-map [debbugs-gnu-send-control-message]
      '(menu-item "Send Control Message"
		  debbugs-gnu-send-control-message
		  :help "Send control message to debbugs.gnu.org")
      'debbugs-gnu-show-all-blocking-reports)
    (define-key-after menu-map [debbugs-gnu-make-control-message]
      '(menu-item "Make Control Message"
		  debbugs-gnu-make-control-message
		  :help (concat "Make (but don't yet send) "
				"a control message to debbugs.gnu.org"))
      'debbugs-gnu-send-control-message)

    (define-key-after menu-map [debbugs-gnu-separator1]
      '(menu-item "--") 'debbugs-gnu-make-control-message)
    (define-key-after menu-map [debbugs-gnu-search]
      '(menu-item "Search Bugs" debbugs-gnu-search
		  :help "Search bugs on debbugs.gnu.org")
      'debbugs-gnu-separator1)
    (define-key-after menu-map [debbugs-gnu]
      '(menu-item "Retrieve Bugs" debbugs-gnu
		  :help "Retrieve bugs from debbugs.gnu.org")
      'debbugs-gnu-search)
    (define-key-after menu-map [debbugs-gnu-bugs]
      '(menu-item "Retrieve Bugs by Number" debbugs-gnu-bugs
		  :help "Retrieve selected bugs from debbugs.gnu.org")
      'debbugs-gnu)

    (define-key-after menu-map [debbugs-gnu-separator2]
      '(menu-item "--") 'debbugs-gnu-bugs)
    (define-key-after menu-map [debbugs-gnu-manual]
      '(menu-item "Debbugs Manual" debbugs-gnu-manual
		  :help "Show Debbugs Manual")
      'debbugs-gnu-separator2)
    (define-key-after menu-map [debbugs-gnu-view-bug-triage]
      '(menu-item "Describe Bug Triage Procedure"
		  debbugs-gnu-view-bug-triage
		  :enable (debbugs-gnu-menu-map-bug-triage-enabled)
		  :help "Show procedure of triaging bugs")
      'debbugs-gnu-manual)
    map))

(defun debbugs-gnu-rescan (&optional nocache)
  "Rescan the current set of bug reports.
If NOCACHE is non-nil, bug information is retrieved from the debbugs server.
Interactively, it is non-nil with the prefix argument."
  (interactive
   (list current-prefix-arg))
  (let ((id (debbugs-gnu-current-id t))
	(debbugs-gnu-current-query debbugs-gnu-local-query)
	(debbugs-gnu-current-filter debbugs-gnu-local-filter)
	(debbugs-gnu-current-suppress debbugs-gnu-local-suppress)
	(debbugs-gnu-current-print-function debbugs-gnu-local-print-function)
	(debbugs-cache-expiry (if nocache t debbugs-cache-expiry)))
    (funcall debbugs-gnu-show-reports-function)
    (when id
      (debbugs-gnu-goto id))))

(define-derived-mode debbugs-gnu-mode tabulated-list-mode "Debbugs"
  "Major mode for listing bug reports.

\\{debbugs-gnu-mode-map}"
  (set (make-local-variable 'debbugs-gnu-sort-state) 'number)
  (set (make-local-variable 'debbugs-gnu-limit) nil)
  (set (make-local-variable 'debbugs-gnu-local-query)
       debbugs-gnu-current-query)
  (set (make-local-variable 'debbugs-gnu-local-filter)
       debbugs-gnu-current-filter)
  (set (make-local-variable 'debbugs-gnu-local-suppress)
       debbugs-gnu-current-suppress)
  (set (make-local-variable 'debbugs-gnu-local-print-function)
       debbugs-gnu-current-print-function)
  (setq tabulated-list-format [("Id"         5 debbugs-gnu-sort-id)
			       ("State"     10 debbugs-gnu-sort-state)
			       ("Submitter" 18 debbugs-gnu-sort-submitter)
			       ("Title"     10 debbugs-gnu-sort-title)])
  (setq tabulated-list-sort-key (cons "Id" nil))
  (setq tabulated-list-printer #'debbugs-gnu-print-entry)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun debbugs-gnu-sort-id (s1 s2)
  (> (alist-get 'id (car s1)) (alist-get 'id (car s2))))

(defconst debbugs-gnu-state-preference
  '((debbugs-gnu-new . 1)
    (debbugs-gnu-stale-5 . 2)
    (debbugs-gnu-stale-4 . 3)
    (debbugs-gnu-stale-3 . 4)
    (debbugs-gnu-stale-2 . 5)
    (debbugs-gnu-stale-1 . 6)
    (debbugs-gnu-handled . 7)
    (debbugs-gnu-pending . 8)
    (debbugs-gnu-forwarded . 9)
    (debbugs-gnu-done . 10)))

(defun debbugs-gnu-get-state-preference (face-string)
  (or (alist-get (get-text-property 0 'face face-string)
		 debbugs-gnu-state-preference)
      10))

(defconst debbugs-gnu-severity-preference
  '(("serious" . 1)
    ("important" . 2)
    ("normal" . 3)
    ("minor" . 4)
    ("wishlist" . 5)))

(defun debbugs-gnu-get-severity-preference (state)
  (or (cdr (assoc (alist-get 'severity state) debbugs-gnu-severity-preference))
      10))

(defun debbugs-gnu-sort-state (s1 s2)
  (let ((id1 (alist-get 'id (car s1)))
	(age1 (debbugs-gnu-get-state-preference (aref (nth 1 s1) 1)))
	(id2 (alist-get 'id (car s2)))
	(age2 (debbugs-gnu-get-state-preference (aref (nth 1 s2) 1))))
    (cond
     ;; Tagged bugs go to the beginning.
     ((and (memq id1 debbugs-gnu-local-tags)
	   (not (memq id2 debbugs-gnu-local-tags)))
      t)
     ((and (not (memq id1 debbugs-gnu-local-tags))
	   (memq id2 debbugs-gnu-local-tags))
      nil)
     ;; Then, we check the age of the bugs.
     ((< age1 age2)
      t)
     ((> age1 age2)
      nil)
     ;; If they have the same age, we check for severity.
     ((< (debbugs-gnu-get-severity-preference (car s1))
	 (debbugs-gnu-get-severity-preference (car s2)))
      t)
     (t nil))))

(defun debbugs-gnu-sort-submitter (s1 s2)
  (let ((address1
	 (debbugs-gnu--split-address
	  (decode-coding-string (alist-get 'originator (car s1) "") 'utf-8)))
	(address2
	 (debbugs-gnu--split-address
	  (decode-coding-string (alist-get 'originator (car s2) "") 'utf-8))))
    (cond
     ;; Bugs I'm the originator of go to the beginning.
     ((and (string-equal user-mail-address (car address1))
	   (not (string-equal (car address1) (car address2))))
      t)
     ((and (string-equal user-mail-address (car address2))
	   (not (string-equal (car address1) (car address2))))
      nil)
     ;; Then, we check the originator.  Prefer the name over the address.
     (t (string-collate-lessp
	 (or (cdr address1) (car address1) "")
	 (or (cdr address2) (car address2) "")
	 nil t)))))

(defun debbugs-gnu-sort-title (s1 s2)
  (let ((owner1
	 (car (debbugs-gnu--split-address
	       (decode-coding-string (alist-get 'owner (car s1) "") 'utf-8))))
	(subject1
	 (decode-coding-string (alist-get 'subject (car s1) "") 'utf-8))
	(owner2
	 (car (debbugs-gnu--split-address
	       (decode-coding-string (alist-get 'owner (car s2) "") 'utf-8))))
	(subject2
	 (decode-coding-string (alist-get 'subject (car s2) "") 'utf-8)))
    (cond
     ;; Bugs I'm the owner of go to the beginning.
     ((and (string-equal user-mail-address owner1)
	   (not (string-equal owner1 owner2)))
      t)
     ((and (string-equal user-mail-address owner2)
	   (not (string-equal owner1 owner2)))
      nil)
     ;; Then, we check the title.
     (t (string-collate-lessp subject1 subject2 nil t)))))

(defun debbugs-gnu-toggle-sort ()
  "Toggle sorting by age and by state."
  (interactive)
  (if (eq debbugs-gnu-sort-state 'number)
      (progn
	(setq debbugs-gnu-sort-state 'state)
	(setq tabulated-list-sort-key (cons "Id" nil)))
    (setq debbugs-gnu-sort-state 'number)
    (setq tabulated-list-sort-key (cons "State" nil)))
  (tabulated-list-init-header)
  (funcall debbugs-gnu-local-print-function))

(defun debbugs-gnu-widen ()
  "Display all the currently selected bug reports."
  (interactive)
  (let ((id (debbugs-gnu-current-id t))
	(inhibit-read-only t))
    (setq debbugs-gnu-limit nil)
    (tabulated-list-init-header)
    (funcall debbugs-gnu-local-print-function)
    (when id
      (debbugs-gnu-goto id))))

(defun debbugs-gnu-show-blocked-by-reports ()
  "Display all bug reports this report is blocked by."
  (interactive)
  (let ((id (debbugs-gnu-current-id))
	(status (debbugs-gnu-current-status)))
    (if (null (alist-get 'blockedby status))
	(message "Bug %d is not blocked by any other bug" id)
      (apply #'debbugs-gnu-bugs (alist-get 'blockedby status)))))

(defun debbugs-gnu-show-blocking-reports ()
  "Display all bug reports this report is blocking."
  (interactive)
  (let ((id (debbugs-gnu-current-id))
	(status (debbugs-gnu-current-status)))
    (if (null (alist-get 'blocks status))
	(message "Bug %d is not blocking any other bug" id)
      (apply #'debbugs-gnu-bugs (alist-get 'blocks status)))))

(defun debbugs-gnu-show-all-blocking-reports (&optional release)
  "Narrow the display to just the reports that are blocking an Emacs release."
  (interactive
   (list
    (if current-prefix-arg
	(completing-read
	 "Emacs release: "
	 (mapcar #'identity debbugs-gnu-emacs-blocking-reports)
	 nil t debbugs-gnu-emacs-current-release)
      debbugs-gnu-emacs-current-release)))

  (let ((blockers
	 (alist-get
	  'blockedby
	  (car
	   (debbugs-get-status
	    (alist-get
	     release debbugs-gnu-emacs-blocking-reports nil nil #'equal)))))
	(id (debbugs-gnu-current-id t))
	(inhibit-read-only t)
	status)
    (setq debbugs-gnu-limit nil)
    (goto-char (point-min))
    (while (not (eobp))
      (setq status (debbugs-gnu-current-status))
      (if (not (memq (alist-get 'id status) blockers))
	  (delete-region (point) (progn (forward-line 1) (point)))
	(push (alist-get 'id status) debbugs-gnu-limit)
	(forward-line 1)))
    (when id
      (debbugs-gnu-goto id))))

(defun debbugs-gnu-emacs-release-blocking-reports (&optional release)
  "Show the reports that are blocking an Emacs release."
  (interactive
   (list
    (if current-prefix-arg
	(completing-read
	 "Emacs release: "
	 (mapcar #'identity debbugs-gnu-emacs-blocking-reports)
	 nil t debbugs-gnu-emacs-current-release)
      debbugs-gnu-emacs-current-release)))

  (if-let* ((id (alist-get
		 release debbugs-gnu-emacs-blocking-reports nil nil #'equal))
	    (blockers (alist-get 'blockedby (car (debbugs-get-status id)))))
      (apply #'debbugs-gnu-bugs blockers)
    (message "There are no release blocking bugs for Emacs %s" release)))

(defun debbugs-gnu-narrow-to-status (string &optional status-only)
  "Only display the bugs matching STRING.
If STATUS-ONLY (the prefix), ignore matches in the From and
Subject fields."
  (interactive "sNarrow to: \nP")
  (let ((id (debbugs-gnu-current-id t))
	(inhibit-read-only t)
	status)
    (setq debbugs-gnu-limit nil)
    (if (equal string "")
	(debbugs-gnu-toggle-suppress)
      (goto-char (point-min))
      (while (not (eobp))
	(setq status (debbugs-gnu-current-status))
	(if (and (not (member string (alist-get 'keywords status)))
		 (not (equal string (alist-get 'severity status)))
		 (or status-only
		     (not (string-match
			   string (alist-get 'originator status))))
		 (or status-only
		     (not (alist-get 'subject status))
		     (not (string-match string (alist-get 'subject status)))))
	    (delete-region (point) (progn (forward-line 1) (point)))
	  (push (alist-get 'id status) debbugs-gnu-limit)
	  (forward-line 1)))
      (when id
	(debbugs-gnu-goto id)))))

(defun debbugs-gnu-goto (id)
  "Go to the line displaying bug ID."
  (goto-char (point-min))
  (while (and (not (eobp))
	      (not (equal (debbugs-gnu-current-id t) id)))
    (forward-line 1)))

(defun debbugs-gnu-toggle-tag ()
  "Toggle the local tag of the report in the current line.
If a report is tagged locally, it is presumed to be of little
interest to you."
  (interactive)
  (let ((id (debbugs-gnu-current-id)))
    (if (memq id debbugs-gnu-local-tags)
	(setq debbugs-gnu-local-tags (delq id debbugs-gnu-local-tags))
      (add-to-list 'debbugs-gnu-local-tags id))
    (when-let ((entry (debbugs-gnu--update-tag-mark-face id))
	       (inhibit-read-only t))
      (delete-region (line-beginning-position) (progn (forward-line 1) (point)))
      (apply #'debbugs-gnu-print-entry entry))
    (when id
      (debbugs-gnu-goto id)))
  (debbugs-gnu-dump-persistency-file))

(defun debbugs-gnu--update-tag-mark-face (id)
  (catch 'entry
    (dolist (entry tabulated-list-entries)
      (when (equal (alist-get 'id (car entry)) id)
	(let ((owner (if (alist-get 'owner (car entry))
			 (car (debbugs-gnu--split-address
			       (decode-coding-string
				(alist-get 'owner (car entry)) 'utf-8))))))
	  (aset (cadr entry) 0
		(propertize
		 (format "%5d" id)
		 'face
		 ;; Mark tagged bugs.
		 (if (memq id debbugs-gnu-local-tags)
		     'debbugs-gnu-tagged
		   'default)))
	  (aset (cadr entry) 3
		(propertize
		 (or (alist-get 'subject (car entry)) "")
		 'face
		 (cond
		  ;; Marked bugs.
		  ((memq id debbugs-gnu-local-marks)
		   'debbugs-gnu-marked)
		  ;; Mark owned bugs.
		  ((and (stringp owner) (string-equal owner user-mail-address))
		   'debbugs-gnu-tagged)
		  (t 'default))))
	  (throw 'entry entry))))))

(defun debbugs-gnu-toggle-mark ()
  "Toggle the local mark of the report in the current line.
If a report is marked locally, it is presumed to be very
interesting to you."
  (interactive)
  (let ((id (debbugs-gnu-current-id)))
    (if (memq id debbugs-gnu-local-marks)
	(setq debbugs-gnu-local-marks (delq id debbugs-gnu-local-marks))
      (add-to-list 'debbugs-gnu-local-marks id))
    (when-let ((entry (debbugs-gnu--update-tag-mark-face id))
	       (inhibit-read-only t))
      (delete-region (line-beginning-position) (progn (forward-line 1) (point)))
      (apply #'debbugs-gnu-print-entry entry))
    (when id
      (debbugs-gnu-goto id)))
  (debbugs-gnu-dump-persistency-file))

(defun debbugs-gnu-toggle-suppress ()
  "Suppress bugs marked in `debbugs-gnu-suppress-bugs'."
  (interactive)
  (setq debbugs-gnu-local-suppress (not debbugs-gnu-local-suppress))
  (tabulated-list-init-header)
  (funcall debbugs-gnu-local-print-function))

(defvar debbugs-gnu-bug-number nil)
(defvar debbugs-gnu-subject nil)

(defun debbugs-gnu-current-id (&optional noerror)
  (or (alist-get 'id (debbugs-gnu-current-status))
      (and (not noerror)
	   (error "No bug on the current line"))))

(defun debbugs-gnu-current-status ()
  ;; FIXME: `debbugs-org-mode' shouldn't be mentioned here.
  (when (or (derived-mode-p 'debbugs-gnu-mode)
	    (derived-mode-p 'debbugs-gnu-usertags-mode)
	    (bound-and-true-p debbugs-org-mode))
    (get-text-property (line-beginning-position) 'tabulated-list-id)))

(defun debbugs-gnu-display-status (query filter status)
  "Display the query, filter and status of the report on the current line."
  (interactive (list debbugs-gnu-local-query
		     debbugs-gnu-local-filter
		     (debbugs-gnu-current-status)))
  (switch-to-buffer "*Bug Status*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when query
      (insert ";; Query\n")
      (pp query (current-buffer))
      (insert "\n"))
    (when filter
      (insert ";; Filter\n")
      (pp filter (current-buffer))
      (insert "\n"))
    (when status
      (insert ";; Status\n")
      (pp status (current-buffer)))
    (goto-char (point-min)))
  (set-buffer-modified-p nil)
  (special-mode))

(defconst debbugs-gnu-select-bugs-limit-max 50
  "Absolute maximum for `debbugs-gnu-select-bugs-limit'.")

(defcustom debbugs-gnu-select-bugs-limit 10
  "Maximum number of bugs to retrieve for multi-bug mailbox group.
This applies for `debbugs-gnu-select-current-bugs'.
Maximum allowed value is `debbugs-gnu-select-bugs-limit-max' to
avoid overloading the server."
  :type '(integer
          :validate
          (lambda (widget)
            (unless (<= 1
                        (widget-value widget)
                        debbugs-gnu-select-bugs-limit-max)
              (widget-put
	       widget :error
               (format "Invalid value: range is 1..%d"
		       debbugs-gnu-select-bugs-limit-max))
	      widget)))
  :version "27.1")

(defun debbugs-gnu-select-current-bugs ()
  "Retrieve the mailboxes for all currently shown bugs.
Limited by `debbugs-gnu-select-bugs-limit'."
  (interactive)
  (save-excursion
    (let (ids)
      (goto-char (point-min))
      (dotimes (_ debbugs-gnu-select-bugs-limit)
	(push (debbugs-gnu-current-id t) ids)
	(setq ids
	      (append (alist-get 'mergedwith (debbugs-gnu-current-status)) ids))
	(forward-line 1))
      (setq ids (delq nil (nreverse ids)))
      (cond
       ((not ids)
	(message "No bug reports in the current buffer"))
       ((eq debbugs-gnu-mail-backend 'rmail)
	(debbugs-gnu-select-current-bugs-with-rmail ids))
       ((eq debbugs-gnu-mail-backend 'gnus)
	(debbugs-gnu-select-current-bugs-with-gnus ids))
       (t (error "No valid mail backend specified"))))))

(defun debbugs-gnu-select-current-bugs-with-rmail (ids)
  "Read email exchange for debbugs IDS.
IDS is the list of bug IDs."
  (let* ((mbox-dir (make-temp-file "debbugs" t))
	 (mbox-fname
	  (format
	   "%s/bug_%s.mbox" mbox-dir (mapconcat #'number-to-string ids ","))))
    (debbugs-get-mbox (car ids) 'mboxmaint mbox-fname)
    (rmail mbox-fname)
    (dolist (bugno (cdr ids))
      (let ((fn (make-temp-file "url")))
	(debbugs-get-mbox bugno 'mboxmaint fn)
	(rmail-get-new-mail fn)
	(delete-file fn)
	;; Remove the 'unseen' attribute from all the messages we've
	;; just read, so that all of them appear in the summary with
	;; the same face.
	(while (< rmail-current-message rmail-total-messages)
	  (rmail-show-message (1+ rmail-current-message)))))
    ;; (set (make-local-variable 'debbugs-gnu-bug-number) id)
    ;; (set (make-local-variable 'debbugs-gnu-subject)
    ;; 	 (format "Re: bug#%d: %s" id (alist-get 'subject status)))
    (rmail-summary)
    (define-key rmail-summary-mode-map "C" #'debbugs-gnu-send-control-message)
    (define-key rmail-summary-mode-map "E" #'debbugs-gnu-make-control-message)
    (set-window-text-height nil 10)
    (other-window 1)
    (define-key rmail-mode-map "C" #'debbugs-gnu-send-control-message)
    (define-key rmail-mode-map "E" #'debbugs-gnu-make-control-message)
    (rmail-show-message 1)))

(defcustom debbugs-gnu-lars-workflow nil
  "If non-nil, set some Gnus vars as preferred by Lars."
  :type 'boolean
  :version "27.1")

(defun debbugs-gnu-select-current-bugs-with-gnus (ids)
  "Create a Gnus group of the messages from the currently shown bugs.
IDS is the list of bug IDs."
  (require 'gnus-group)
  (when debbugs-gnu-lars-workflow
    (setq gnus-suppress-duplicates t
	  gnus-save-duplicate-list t))
  (let ((mbox-url
         (replace-regexp-in-string
          ";mboxstat=yes" ""
          (alist-get 'emacs gnus-bug-group-download-format-alist)
          nil t)))
    (gnus-read-ephemeral-bug-group ids mbox-url)
    (debbugs-gnu-summary-mode 1)))

(defun debbugs-gnu-select-report ()
  "Select the report on the current line."
  (interactive)
  (when (mouse-event-p last-input-event) (mouse-set-point last-input-event))
  ;; We open the report messages.
  (let* ((status (debbugs-gnu-current-status))
	 (id (alist-get 'id status))
	 (merged (alist-get 'mergedwith status)))
    (setq merged (if (listp merged) merged (list merged)))
    (cond
     ((not id)
      (message "No bug report on the current line"))
     ((eq debbugs-gnu-mail-backend 'rmail)
      (debbugs-gnu-read-emacs-bug-with-rmail id status merged))
     ((eq debbugs-gnu-mail-backend 'gnus)
      (debbugs-gnu-read-emacs-bug-with-gnus id status merged))
     (t (error "No valid mail backend specified")))))

(defun debbugs-gnu-read-emacs-bug-with-rmail (id status merged)
  "Read email exchange for debbugs bug ID.
STATUS is the bug's status list.
MERGED is the list of bugs merged with this one."
  (let* ((mbox-dir (make-temp-file "debbugs" t))
	 (mbox-fname (format "%s/bug_%d.mbox" mbox-dir id)))
    (debbugs-get-mbox id 'mboxmaint mbox-fname)
    (rmail mbox-fname)
    ;; Download messages of all the merged bug reports and append them
    ;; to the mailbox of the requested bug.
    (when merged
      (dolist (bugno merged)
	(let ((fn (make-temp-file "url")))
	  (debbugs-get-mbox bugno 'mboxmaint fn)
	  (rmail-get-new-mail fn)
	  (delete-file fn)
	  ;; Remove the 'unseen' attribute from all the messages we've
	  ;; just read, so that all of them appear in the summary with
	  ;; the same face.
	  (while (< rmail-current-message rmail-total-messages)
	    (rmail-show-message (1+ rmail-current-message))))))
    (set (make-local-variable 'debbugs-gnu-bug-number) id)
    (set (make-local-variable 'debbugs-gnu-subject)
	 (format "Re: bug#%d: %s" id (alist-get 'subject status)))
    (rmail-summary)
    (define-key rmail-summary-mode-map "C" #'debbugs-gnu-send-control-message)
    (define-key rmail-summary-mode-map "E" #'debbugs-gnu-make-control-message)
    (set-window-text-height nil 10)
    (other-window 1)
    (define-key rmail-mode-map "C" #'debbugs-gnu-send-control-message)
    (define-key rmail-mode-map "E" #'debbugs-gnu-make-control-message)
    (rmail-show-message 1)))

(defun debbugs-gnu-read-emacs-bug-with-gnus (id status merged)
  "Read email exchange for debbugs bug ID.
STATUS is the bug's status list.
MERGED is the list of bugs merged with this one."
  (require 'gnus-dup)
  (when debbugs-gnu-lars-workflow
    (setq gnus-suppress-duplicates t
	  gnus-save-duplicate-list t))
  ;; Use Gnus.
  (gnus-read-ephemeral-emacs-bug-group
   (cons id (if (listp merged) merged (list merged)))
   (cons (current-buffer)
	 (current-window-configuration)))
  (with-current-buffer (window-buffer (selected-window))
    (set (make-local-variable 'debbugs-gnu-bug-number) id)
    (set (make-local-variable 'debbugs-gnu-subject)
	 (format "Re: bug#%d: %s" id (alist-get 'subject status)))
    (debbugs-gnu-summary-mode 1)))

(defvar debbugs-gnu-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" #'debbugs-gnu-send-control-message)
    (define-key map "E" #'debbugs-gnu-make-control-message)
    (define-key map [(meta m)] #'debbugs-gnu-apply-patch)
    map))

(define-minor-mode debbugs-gnu-summary-mode
  "Minor mode for providing a debbugs interface in Gnus summary buffers.

\\{debbugs-gnu-summary-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-summary-mode-map
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
	       (set (make-local-variable 'message-alter-recipients-function)
		    (lambda (address)
		      (if (string-match "\\([0-9]+\\)@donarmstrong"
					(car address))
			  (let ((new (format "%s@debbugs.gnu.org"
					     (match-string 1 (car address)))))
			    (cons new new))
			address))))))
	  ,@(and debbugs-gnu-subject
		 `((subject ,debbugs-gnu-subject)))))))

(defun debbugs-gnu-guess-current-id ()
  "Guess the ID based on \"#23\".
Looks at current line and then backwards from point."
  (save-excursion
    (beginning-of-line)
    (and
     (or (re-search-forward "#\\([0-9]+\\)" (line-end-position) t)
	 (re-search-backward "#\\([0-9]+\\)" nil t))
     (string-to-number (match-string 1)))))

(defun debbugs-gnu-proper-bug-number (id)
  "Check that ID is a number string and in the range of existing bugs."
  (and (string-match "^[1-9][0-9]*$" id)
       (<= (string-to-number id) (car (debbugs-newest-bugs 1)))))

(defvar debbugs-gnu-completion-table
  (completion-table-dynamic
   (lambda (string)
     (let* ((split (split-string string "-"))
	    (from (and (cdr split) (car split)))
	    (to (or (car (cdr split)) (car split))))
       (cond
	((> (length split) 2) nil)
	((and (or (zerop (length from)) (debbugs-gnu-proper-bug-number from))
	      (string-equal to ""))
	 (mapcar
	  (lambda (x) (concat string x))
	  (cons (unless from "-") '("1" "2" "3" "4" "5" "6" "7" "8" "9"))))
	((and (or (zerop (length from)) (debbugs-gnu-proper-bug-number from))
	      (debbugs-gnu-proper-bug-number to))
	 (mapcar
	  (lambda (x)
	    (and (debbugs-gnu-proper-bug-number (concat to x))
		 (concat string x)))
	  '("" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))))))
  "Dynamic completion table for reading bug numbers.")

(defun debbugs-gnu-expand-bug-number-list (bug-number-list)
  "Expand BUG-NUMBER-LIST to a list of single bug numbers.
BUG-NUMBER-LIST is a list of bug numbers or bug number ranges, as
returned by `debbugs-gnu-bugs'."
  (let (result)
    (dolist (elt bug-number-list result)
      (let* ((split (split-string elt "-"))
	     (from (and (cdr split) (car split)))
	     (to (or (car (cdr split)) (car split))))
	(setq
	 result
	 (cond
	  ((or (> (length split) 2)
	       (zerop (length to)))
	   (user-error "Wrong bug number or range %s" elt))
	  ((null from)
	   (cons to result))
	  ((string-equal from "")
	   (append
	    (mapcar
	     #'number-to-string
	     (debbugs-newest-bugs (string-to-number to)))
	    result))
	  (t (append
	      (mapcar
	       #'number-to-string
	       (number-sequence (string-to-number from) (string-to-number to)))
	      result))))))))


(defconst debbugs-gnu-control-message-keywords
  '("serious" "important" "normal" "minor" "wishlist"
    "done" "donenotabug" "donewontfix" "doneunreproducible"
    "invalid" ; done+notabug+wontfix
    "unarchive" "unmerge" "reopen" "close"
    "merge" "forcemerge"
    "block" "unblock"
    "owner" "noowner"
    "reassign"
    "retitle"
    "forwarded" "notforwarded"
    ;; 'notfixed <bugnum> <version>' works, even though it's
    ;; undocumented at debbugs.gnu.org.
    "fixed" "found" "notfound" "notfixed"
    "patch" "wontfix" "moreinfo" "unreproducible" "notabug"
    "pending" "help" "security" "confirmed" "easy"
    "usertag"
    "documentation" ;; usertag:emacs.documentation
    ))

(defconst debbugs-gnu-control-message-commands-regexp
  (concat "^" (regexp-opt (cl-list* "#" "tags" "severity" "user"
                                    debbugs-gnu-control-message-keywords))
          " .*$"))

(defconst debbugs-gnu-control-message-end-regexp
  (concat "^" (regexp-opt '("--" "quit" "stop"
                            "thank" "thanks" "thankyou" "thank you"))
          "$"))

(defun debbugs-gnu-send-control-message (message &optional reverse)
  "Send a control message for the current bug report.
You can set the severity or add a tag, or close the report.

If given a prefix, and given a tag to set, the tag will be
removed instead."
  (interactive
   (list (completing-read
          "Control message: " debbugs-gnu-control-message-keywords nil t)
	 current-prefix-arg))
  (let ((id (or (debbugs-gnu-current-id t)
                debbugs-gnu-bug-number       ; Set on group entry.
                (debbugs-gnu-guess-current-id))))
    (with-temp-buffer
      (debbugs-gnu-make-control-message
       message id reverse (current-buffer))
      (funcall (or debbugs-gnu-send-mail-function send-mail-function))
      (message-goto-body)
      (message "Control message sent:\n%s"
               (buffer-substring-no-properties (point) (1- (point-max)))))))

(defun debbugs-gnu-implicit-ids ()
  "Return a list of bug IDs guessed from the current buffer."
  (delq nil (delete-dups
             (list (debbugs-gnu-current-id t)
                   debbugs-gnu-bug-number ; Set on group entry.
                   (debbugs-gnu-guess-current-id)
                   (let ((bugnum-re
			  "\\([0-9]+\\)\\(?:-done\\)?@debbugs.gnu.org"))
                     (when (derived-mode-p 'message-mode)
                       (save-excursion
                         (save-restriction
                           (message-narrow-to-headers)
                           (or (when-let ((addr (message-fetch-field "to")))
                                 (and (string-match bugnum-re addr)
				      (string-to-number (match-string 1 addr))))
                               (when-let ((addr (message-fetch-field "cc")))
                                 (and (string-match bugnum-re addr)
                                      (string-to-number
				       (match-string 1 addr)))))))))))))

(defun debbugs-gnu-make-control-message
    (message bugid &optional reverse buffer noversion)
  "Make a control message for the current bug report.
The message is inserted into BUFFER, and mail headers are adjust
so that it will be sent to control@debbugs.gnu.org (via Bcc if
there is already a To address).  If BUFFER omitted, create and
display a new buffer.  If optional NOVERSION is non-nil, suppress
query for version number on \"close\", \"fixed\", etc messages.
Otherwise, the version is queried for bugs whose package is
\"emacs\".

When called interactively, choose the current buffer if it is in
`message-mode', or create a new buffer otherwise.

You can set the severity or add a tag, or close the report.

If given a prefix, and given a tag to set, the tag will be
removed instead."
  (interactive
   (save-excursion                 ; Point can change while prompting!
     (list (completing-read
            "Control message: " debbugs-gnu-control-message-keywords nil t)
           (let* ((implicit-ids (mapcar #'prin1-to-string
                                        (debbugs-gnu-implicit-ids)))
                  (default-id (car implicit-ids)))
             (string-to-number
              (completing-read (if default-id
                                   (format "Bug # (default %s): " default-id)
                                 "Bug #: ")
                               implicit-ids
                               (lambda (s) (string-match-p "\\`[0-9]+\\'" s))
                               nil nil nil (car implicit-ids))))
           current-prefix-arg
           (when (derived-mode-p 'message-mode)
             (current-buffer)))))
  (let* ((status (or (debbugs-gnu-current-status)
                     (car (debbugs-get-status bugid))))
         (version
          (if (and
               (not noversion)
               (member message '("close" "done"
                                 "fixed" "notfixed" "found" "notfound"))
               (member "emacs" (alist-get 'package status)))
              (save-excursion
                (read-string
                 "Version: "
                 (pcase (nbutlast (version-to-list emacs-version)
                                  ;; Chop off build number, if needed.
                                  (if (boundp 'emacs-build-number)
                                      0 1))
                   (`(,major ,minor ,_micro) ; Development version.
                    (format "%d.%d" major
                            (if (member
				 message '("notfixed" "found" "notfound"))
                                minor
                              (1+ minor))))
                   (`(,major ,minor)    ; Release version.
                    (format "%d.%d" major minor))
                   ;; Unexpected version format?
                   (_ emacs-version))))
            ;; Don't put a version.
            "")))
    (unless buffer
      (setq buffer
            (pop-to-buffer
             (get-buffer-create
              (format "*Debbugs Control Message for #%d*" bugid)))))
    (set-buffer buffer)
    (when (= (buffer-size) 0)
      (insert "To: control@debbugs.gnu.org\n"
              "From: " (message-make-from) "\n"
              (format "Subject: control message for bug #%d\n" bugid)
              mail-header-separator
              "\n"))
    (unless (or (derived-mode-p 'message-mode)
                ;; `message-mode' associates buffer with file, we
                ;; don't want to do that for temp buffers.
                (eq (aref (buffer-name) 0) ?\s))
      (message-mode))
    (save-restriction
      (message-narrow-to-headers)
      (let* ((ctrl-addr "control@debbugs.gnu.org")
             (ctrl-re (regexp-quote ctrl-addr))
             (to-addr (message-fetch-field "to"))
             (bcc-addr (message-fetch-field "bcc")))
        (unless (or (and  to-addr (string-match-p ctrl-re to-addr))
                    (and bcc-addr (string-match-p ctrl-re bcc-addr)))
          (message-add-header
           (format "%s: %s" (if to-addr "Bcc" "To") ctrl-addr)))))
    (message-goto-body)
    (while (looking-at-p debbugs-gnu-control-message-commands-regexp)
      (forward-line))
    (insert
     (save-excursion             ; Point can change while prompting!
       (cond
        ((member message '("unarchive" "unmerge" "noowner" "notforwarded"))
         (format "%s %d\n" message bugid))
        ((equal message "reopen")
         (format "reopen %d\ntags %d - fixed patch\n" bugid bugid))
        ((member message '("merge" "forcemerge"))
         (format
          "%s %d %s\n" message bugid
          (string-join
           (debbugs-gnu-expand-bug-number-list
            (completing-read-multiple
             (format "%s with bug(s) #: " (capitalize message))
             debbugs-gnu-completion-table))
           " ")))
        ((member message '("block" "unblock"))
         (format
          "%s %d by %s\n" message bugid
          (string-join
           (debbugs-gnu-expand-bug-number-list
            (completing-read-multiple
             (format "%s with bug(s) #: " (capitalize message))
             (if (equal message "unblock")
                 (mapcar #'number-to-string (alist-get 'blockedby status))
               debbugs-gnu-completion-table)
             nil (and (equal message "unblock") status)))
           " ")))
        ((equal message "owner")
         (format "owner %d !\n" bugid))
        ((equal message "retitle")
         (format "retitle %d %s\n" bugid (read-string "New title: ")))
        ((equal message "forwarded")
         (format "forwarded %d %s\n" bugid (read-string "Forward to: ")))
        ((equal message "reassign")
         (format
	  "reassign %d %s\n" bugid
          (string-join
	   (completing-read-multiple
	    "Package(s): " debbugs-gnu-all-packages nil nil
	    (string-join (alist-get 'package status) ","))
	   ",")))
        ((member message '("close" "done"))
         (format "close %d %s\n" bugid version))
        ((member message '("found" "notfound" "fixed" "notfixed"))
         (format "%s %d %s\n" message bugid version))
        ((member message '("donenotabug" "donewontfix"
                           "doneunreproducible"))
         (format "tags %d %s\nclose %d\n" bugid (substring message 4) bugid))
        ((member message '("serious" "important" "normal"
                           "minor" "wishlist"))
         (format "severity %d %s\n" bugid message))
        ((equal message "invalid")
         (format "tags %d notabug wontfix\nclose %d\n"
                 bugid bugid))
        ((equal message "documentation")
         (format "user emacs\nusertag %d %s\n" bugid "documentation"))
        ((equal message "usertag")
         (format "user %s\nusertag %d %s\n"
                 (completing-read
                  "Package name or email address: "
                  (append
		   debbugs-gnu-applicable-packages (list user-mail-address))
                  nil nil (car debbugs-gnu-default-packages))
                 bugid (read-string "User tag: ")))
	;; "patch", "wontfix", "moreinfo", "unreproducible", "notabug",
	;; "pending", "help", "security", "confirmed", "easy"
        (t
         (format "tags %d %c %s\n"
                 bugid (if reverse ?- ?+)
                 message)))))
    (unless (looking-at-p debbugs-gnu-control-message-end-regexp)
      (insert "quit\n\n"))
    (add-hook 'message-send-actions
              (lambda () (remhash bugid debbugs-cache-data))
              nil t)))

(defun debbugs-gnu-jump-to-bug (bugid)
  "Display buffer associated with BUGID with `pop-to-buffer'.
Use `gnus-read-ephemeral-emacs-bug-group' instead if there is no such buffer."
  (let (bug-buf
        ;; By reverse order of preference.  FIXME: `rmail' buffers?
        (preferred-modes '(gnus-summary-mode gnus-article-mode message-mode)))
    (save-current-buffer
      (cl-loop
       for buf in (buffer-list)
       while preferred-modes do
       (set-buffer buf)
       (when-let (((memql bugid (debbugs-gnu-implicit-ids)))
                  (mode (cl-loop
                         for mode in preferred-modes
                         thereis (and (derived-mode-p mode)
                                      ;; Don't choose sent message buffers.
                                      (or (not (eq mode 'message-mode))
                                          (not message-sent-message-via))
                                      mode))))
         (setq preferred-modes (cdr (memq mode preferred-modes)))
         (setq bug-buf buf))))
    (if bug-buf
        (pop-to-buffer bug-buf '(display-buffer-reuse-window
                                 . ((reusable-frames . visible))))
      (gnus-read-ephemeral-emacs-bug-group
       bugid (cons (current-buffer) (current-window-configuration))))))

(defcustom debbugs-gnu-git-remote-info-alist
  '(("git.sv.gnu.org\\(?::/srv/git\\)/emacs.git" .
     ((commit-url
       . "https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=%H")
      (ref-globs . ("/emacs-*" "/master"))))
    ("git.sv.gnu.org\\(?::/srv/git\\)/emacs/elpa" .
     ((commit-url
       . "https://git.savannah.gnu.org/cgit/emacs/elpa.git/commit/?id=%H"))))
  "Nest alist for repository-specific information.
Each element has the form (REMOTE-REGEXP . INFO-ALIST), where
INFO-ALIST is an alist containing the repository attributes.

Supported keys of INFO-ALIST are

* `commit-url': Format of a URL for a given commit hash, using
  format specifiers supported by `git show'.  Used by
  `debbugs-gnu-announce-commit' as a supplement to
  `debbugs-gnu-commit-description-format'.

* `ref-globs': List of glob patterns matching branches of
  interest, used by `debbugs-gnu-announce-commit' to make the
  \"Pushed to X\" message."
  :version "27.1"
  :type '(alist :key-type string :value-type (alist :key-type symbol)))

(defcustom debbugs-gnu-commit-description-format
  "%h %cI \"%s\""
  "Format used for describing commits in `debbugs-gnu-announce-commit'.
It is passed as --format argument to `git show', see its manual
page for formatting specifier meanings."
  :version "27.1"
  :type 'string)

(defun debbugs-gnu--git-insert (&rest args)
  "Insert output of running git with ARGS.
Throws error if git returns non-zero."
  (unless (eql 0 (apply #'vc-git--call '(t t) args))
    (error "git %s failed: %s" (car args) (buffer-string))))

(defun debbugs-gnu--git-remote-info ()
  "Return (REMOTE . INFO-ALIST).
Where REMOTE is a string naming a git remote which matches the
REMOTE-REGEXP key of a `debbugs-gnu-git-remote-info-alist' entry.
INFO-ALIST is the correponding value of the entry.  If no entry
matches, return nil."
  (with-temp-buffer
    (debbugs-gnu--git-insert "remote" "-v")
    (catch 'found-remote
      (dolist (remote-info debbugs-gnu-git-remote-info-alist)
        (goto-char (point-min))
        (and (re-search-forward (car remote-info) nil t)
             (progn (beginning-of-line)
                    (looking-at "[^ \t]+"))
             (throw 'found-remote
                    (cons (match-string 0) (cdr remote-info))))))))

(defun debbugs-gnu--git-get-pushed-to (commit-range remote-info)
  "Return the branch name which COMMIT-RANGE was pushed to.
REMOTE-INFO is return value of `debbugs-gnu--git-remote-info'."
  (let* ((last-commit
          (with-temp-buffer
            (debbugs-gnu--git-insert
             ;; %H: commit hash.
             "log" "-1" "--format=%H" commit-range)
            (goto-char (point-min))
            (buffer-substring (point-min) (line-end-position))))
         (remote (pop remote-info))
	 (ref-globs (alist-get 'ref-globs remote-info)))
    (with-temp-buffer
      (apply
       #'debbugs-gnu--git-insert
       "branch" "--remote" "--contains" last-commit
       (mapcar (lambda (glob) (concat remote glob))
               ref-globs))
      ;; First 2 characters are current branch indicator.
      (goto-char (+ (point-min) 2))
      (and (looking-at (concat (regexp-quote remote) "/\\(.+\\)$"))
           (match-string 1)))))

(defun debbugs-gnu-announce-commit (commit-range bugnum &optional _args)
  "Insert info about COMMIT-RANGE into message.
Optionally call `debbugs-gnu-make-control-message' to close BUGNUM."
  (let* ((status (car (debbugs-get-status bugnum)))
         (packages (alist-get 'package status))
         (remote-info (debbugs-gnu--git-remote-info)))
    (insert "\nPushed to "
            (or (debbugs-gnu--git-get-pushed-to commit-range remote-info) "")
            ".\n\n")
    (debbugs-gnu--git-insert
     "show" "--no-patch"
     (concat "--format=" debbugs-gnu-commit-description-format
             "\n" (alist-get 'commit-url remote-info) "\n")
     commit-range)
    (when (y-or-n-p "Close bug? ")
      (let ((emacs-version
             (and (member "emacs" packages)
                  (file-exists-p "configure.ac")
                  (with-temp-buffer
                    (insert-file-contents "configure.ac")
                    (and (re-search-forward "\
^ *AC_INIT(GNU Emacs, *\\([0-9.]+\\), *bug-gnu-emacs@gnu.org"
                                            nil t)
                         (match-string 1))))))
        (debbugs-gnu-make-control-message
         "done" bugnum nil (current-buffer) (not emacs-version))))))

(defun debbugs-gnu-post-patch (commit-range bugnum &optional format-patch-args)
  "Attach COMMIT-RANGE as patches into current message.
Optionally call `debbugs-gnu-make-control-message'' to tag BUGNUM
with `patch'."
  (letrec ((disposition
	    (completing-read "disposition: " '("inline" "attachment")))
           ;; Make attachments text/plain for better compatibility
           ;; (e.g., opening in browser instead of downloading).
           (type (if (equal disposition "inline") "text/x-diff" "text/plain"))
           (dir (make-temp-file (format "patches-for-bug%d" bugnum) t))
           (deldir (lambda ()
                     (delete-directory dir t)
                     (remove-hook 'message-exit-actions deldir t)
                     (remove-hook 'kill-buffer-hook deldir t))))
    (add-hook 'message-send-actions deldir nil t)
    (add-hook 'kill-buffer-hook deldir nil t)
    (with-temp-buffer
      (apply #'debbugs-gnu--git-insert
             "format-patch" (concat "--output-directory=" dir)
             (append format-patch-args
                     (list commit-range))))
    (dolist (patch (directory-files dir t "\\`[^.]"))
      (mml-attach-file patch type "patch" disposition))
    (when (and (not (member
                     "patch"
		     (alist-get 'tags (car (debbugs-get-status bugnum)))))
               (y-or-n-p "Tag + patch? "))
      (debbugs-gnu-make-control-message
       "patch" bugnum nil (current-buffer)))))

(defvar debbugs-gnu-read-commit-range-hook nil
  "Used by `debbugs-gnu-pick-commits'.
Each function receives no arguments, and should return an
argument compatible with `debbugs-gnu-pick-commits'.  If the
function can't function in the current buffer, it should return
nil to let the next function try.")

(defun debbugs-gnu-read-commit-range-from-vc-log ()
  "Read commit range from a VC log buffer.
Return commit at point, or commit range in region if it is
active.  This function is suitable for use in
`debbugs-gnu-read-commit-range-hook'."
  (when (derived-mode-p 'vc-git-log-view-mode)
    (list (if (use-region-p)
              (let ((beg (log-view-current-entry (region-beginning)))
                    (end (log-view-current-entry (region-end))))
                (if (= (car beg) (car end))
                    ;; Region spans only a single entry.
                    (cadr beg)
                  ;; Later revs are at the top of buffer.
                  (format "%s~1..%s" (cadr end) (cadr beg))))
            (log-view-current-tag)))))
(add-hook 'debbugs-gnu-read-commit-range-hook
          #'debbugs-gnu-read-commit-range-from-vc-log)

(defvar debbugs-gnu-picked-commits nil
  "List of commits selected in `debbugs-gnu-pick-commits'.
Format of each element is (BUGNUMBERS REPO-DIR COMMIT-RANGE).")

(defun debbugs-gnu-pick-commits (commit-range)
  "Select COMMIT-RANGE to post as patches or announce as pushed.
COMMIT-RANGE is read using `debbugs-gnu-read-commit-range-hook',
or `read-string' if none of its functions apply.  Add entry to
`debbugs-gnu-picked-commits' and jump to read bug in preparation
for user to call `debbugs-gnu-maybe-use-picked-commits'."
  (interactive
   (or (run-hook-with-args-until-success
        'debbugs-gnu-read-commit-range-hook)
       (list (read-string "Commit (or range): "))))
  (let ((bugnum nil)
        (repo-dir default-directory))
    (with-temp-buffer
      (debbugs-gnu--git-insert
       ;; %B: raw body (unwrapped subject and body).
       "show" "--no-patch" "--format=%B" commit-range)
      (goto-char (point-min))
      (while (re-search-forward "[bB]ug ?#\\([0-9]+\\)" nil t)
        (push (match-string 1) bugnum)))
    (let ((read-bugnum
           (string-to-number
            (completing-read
             (if bugnum
                 (format "Bug # (default %s): " (car bugnum))
               "Bug #: ")
             debbugs-gnu-completion-table nil t nil nil bugnum))))
      (debbugs-gnu-jump-to-bug read-bugnum)
      (cl-callf2 mapcar #'string-to-number bugnum)
      (unless (memql read-bugnum bugnum)
        (push read-bugnum bugnum)))
    (push (list bugnum repo-dir commit-range)
          debbugs-gnu-picked-commits)
    (if (derived-mode-p 'message-mode)
        (debbugs-gnu-maybe-use-picked-commits)
      (message "Reply to a message to continue"))))

(defun debbugs-gnu-maybe-use-picked-commits ()
  "Add commit corresponding to current message's bug number.
Calls `debbugs-gnu-announce-commit' or `debbugs-gnu-post-patch'
on an entry with a matching bug number from
`debbugs-gnu-picked-commits'.  Remove entry after message is
successfully sent."
  (interactive)
  (when (derived-mode-p 'message-mode)
    (cl-loop with id = (car (debbugs-gnu-implicit-ids))
             for pcomm-entry in debbugs-gnu-picked-commits
             for (bugnum repo-dir commit-range) = pcomm-entry
             when (memql id bugnum)
             do
             (goto-char (point-max))
             (let ((default-directory repo-dir))
               (pcase (read-char-choice
                       (format "[a]nnounce commit, or [p]ost patch? (%s)"
                               commit-range)
                       '(?a ?p))
                 (?a (debbugs-gnu-announce-commit commit-range id) )
                 (?p (debbugs-gnu-post-patch commit-range id))))
             (add-hook 'message-send-actions
                       (lambda ()
                         (cl-callf2 delq pcomm-entry
                                    debbugs-gnu-picked-commits))
                       nil t)
             (remove-hook 'post-command-hook
                          #'debbugs-gnu-maybe-use-picked-commits)
             (cl-return))))

;; We need to daisy chain the hooks because `message-setup-hook' runs
;; too early (before `message-yank-original').
(defun debbugs-gnu--prepare-to-use-picked-commits ()
  (add-hook 'post-command-hook #'debbugs-gnu-maybe-use-picked-commits))
(add-hook 'message-setup-hook #'debbugs-gnu--prepare-to-use-picked-commits)

(defvar debbugs-gnu-pick-vc-log-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'debbugs-gnu-pick-commits)
    map))

(define-minor-mode debbugs-gnu-pick-vc-log-commit-mode
  "Minor mode for sending commits from *vc-change-log* buffers to debbugs.

\\{debbugs-gnu-pick-vc-log-commit-mode}"
  :lighter " Debbugs")

(add-hook 'vc-git-log-view-mode-hook #'debbugs-gnu-pick-vc-log-commit-mode)

(defvar debbugs-gnu-usertags-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'debbugs-gnu-select-usertag)
    (define-key map [mouse-2] #'debbugs-gnu-select-usertag)
    map))

(define-derived-mode debbugs-gnu-usertags-mode tabulated-list-mode "Usertags"
  "Major mode for listing user tags.

\\{debbugs-gnu-usertags-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun debbugs-gnu-usertags (&rest users)
  "List all user tags for USERS, which is \(\"emacs\"\) by default."
  (interactive
   (if current-prefix-arg
       (completing-read-multiple
	"Package name(s) or email address: "
	(append debbugs-gnu-applicable-packages (list user-mail-address)) nil nil
	(string-join debbugs-gnu-default-packages ","))
     debbugs-gnu-default-packages))

  (unwind-protect
      (let ((inhibit-read-only t)
	    (debbugs-port "gnu.org")
	    (buffer-name "*Emacs User Tags*")
	    (user-tab-length
	     (1+ (apply #'max (length "User") (mapcar #'length users)))))

	;; Initialize variables.
	(when (and (file-exists-p debbugs-gnu-persistency-file)
		   (not debbugs-gnu-local-tags))
	  (with-temp-buffer
	    (insert-file-contents debbugs-gnu-persistency-file)
	    (eval (read (current-buffer)) t)))

	;; Create buffer.
	(when (get-buffer buffer-name)
	  (kill-buffer buffer-name))
	(switch-to-buffer (get-buffer-create buffer-name))
	(debbugs-gnu-usertags-mode)
	(setq tabulated-list-format `[("User" ,user-tab-length t)
				      ("Tag"  10 t)])
	(setq tabulated-list-sort-key (cons "User" nil))
	;(setq tabulated-list-printer #'debbugs-gnu-print-entry)

	;; Retrieve user tags.
	(dolist (user users)
	  (dolist (tag (sort (debbugs-get-usertag :user user) #'string<))
	    (add-to-list
	     'tabulated-list-entries
	     ;; `tabulated-list-id' is the parameter list for `debbugs-gnu'.
	     `((("tagged") (,user) nil nil (,tag))
	       ,(vector (propertize user 'mouse-face 'highlight)
			(propertize tag  'mouse-face 'highlight)))
	     'append)))

	;; Add local tags.
	(when debbugs-gnu-local-tags
	  (add-to-list
	     'tabulated-list-entries
	     `((("tagged"))
	       ,(vector
		 "" (propertize "(local tags)" 'mouse-face 'highlight)))))

	;; Show them.
	(tabulated-list-init-header)
	(tabulated-list-print)

	(set-buffer-modified-p nil)
	(goto-char (point-min)))))

(defun debbugs-gnu-select-usertag ()
  "Select the user tag on the current line."
  (interactive)
  (when (mouse-event-p last-input-event) (mouse-set-point last-input-event))
  ;; We open the bug reports.
  (when-let ((args (debbugs-gnu-current-status)))
    (apply #'debbugs-gnu args)))

(defcustom debbugs-gnu-default-bug-number-list
  (propertize "-10" 'help-echo "The 10 most recent bugs.")
  "The default value used in interactive call of `debbugs-gnu-bugs'.
It must be a string, containing a comma separated list of bugs or bug ranges.
A negative value, -N, means the newest N bugs."
  :type 'string
  :version "25.2")

;;;###autoload
(defun debbugs-gnu-bugs (&rest bugs)
  "List all BUGS, a list of bug numbers.
In interactive calls, prompt for a comma separated list of bugs
or bug ranges, with default to `debbugs-gnu-default-bug-number-list'."
  (interactive
   (mapcar
    #'string-to-number
    (debbugs-gnu-expand-bug-number-list
     (or
      (completing-read-multiple
       (format "Bug numbers (default %s): " debbugs-gnu-default-bug-number-list)
       debbugs-gnu-completion-table)
      (split-string debbugs-gnu-default-bug-number-list "," t)))))
  (dolist (elt bugs)
    (unless (natnump elt) (signal 'wrong-type-argument (list 'natnump elt))))
  (add-to-list 'debbugs-gnu-current-query (cons 'bugs bugs))
  ;; We do not suppress bugs requested explicitely.
  (setq debbugs-gnu-current-suppress nil)
  (debbugs-gnu nil)
  (when (called-interactively-p 'interactive)
    (message "Retrieving bugs finished")))

(defcustom debbugs-gnu-trunk-directory "~/src/emacs/trunk/"
  "The directory where the main source tree lives."
  :type 'directory
  :version "25.2")

(defcustom debbugs-gnu-branch-directory "~/src/emacs/emacs-28/"
  "The directory where the previous source tree lives."
  :type 'directory
  :version "29.1")

(defvar debbugs-gnu-current-directory nil
  "The current source tree directory.")

(defun debbugs-gnu-init-current-directory (&optional branch)
"Initialize `debbugs-gnu-current-directory'."
  (setq debbugs-gnu-current-directory
	(if branch
	    debbugs-gnu-branch-directory
	  debbugs-gnu-trunk-directory))
  (unless (file-directory-p debbugs-gnu-current-directory)
    (setq debbugs-gnu-current-directory
	  (read-file-name
	   "Emacs repository location: "
	   debbugs-gnu-current-directory nil t nil #'file-directory-p))))

(defun debbugs-gnu-apply-patch (&optional branch selectively)
  "Apply the patch from the current message.
If given a prefix, patch in the branch directory instead.

If SELECTIVELY, query the user before applying the patch."
  (interactive "P")
  (unless (eq debbugs-gnu-mail-backend 'gnus)
    (error "This function only works with Gnus."))
  (add-hook 'diff-mode-hook #'debbugs-gnu-diff-mode)
  (debbugs-gnu-init-current-directory branch)
  (let ((rej (expand-file-name "debbugs-gnu.rej" temporary-file-directory))
	(output-buffer (get-buffer-create "*debbugs patch*"))
	(patch-buffers nil))
    (when (file-exists-p rej)
      (delete-file rej))
    (with-current-buffer output-buffer
      (erase-buffer))
    (gnus-summary-select-article nil t)
    ;; The patches are either in MIME attachements or the main article
    ;; buffer.  Determine which.
    (with-current-buffer gnus-article-buffer
      (dolist (handle (mapcar #'cdr (gnus-article-mime-handles)))
	(when (string-match "diff\\|patch\\|plain\\|octet\\|verbatim"
			    (mm-handle-media-type handle))
	  (push (cons (mm-handle-encoding handle)
		      (mm-handle-buffer handle))
		patch-buffers))))
    (unless patch-buffers
      (gnus-summary-show-article 'raw)
      (with-current-buffer gnus-article-buffer
	(article-decode-charset))
      (push (cons nil gnus-article-buffer) patch-buffers))
    (dolist (elem (nreverse patch-buffers))
      (with-current-buffer (generate-new-buffer "*debbugs input patch*")
	(insert-buffer-substring (cdr elem))
	(cond ((eq (car elem) 'base64)
	       (base64-decode-region (point-min) (point-max)))
	      ((eq (car elem) 'quoted-printable)
	       (quoted-printable-decode-region (point-min) (point-max))))
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" t t))
	(debbugs-gnu-fix-patch debbugs-gnu-current-directory)
	(when (or (not selectively)
		  (y-or-n-p (format "%s\nApply?"
				    (buffer-substring (point-min)
						      (min 200 (point-max))))))
	  (let (old-rej)
	    (when (file-exists-p rej)
	      (with-temp-buffer
		(insert-file-contents rej)
		(setq old-rej (buffer-string)))
	      (delete-file rej))
	    (call-process-region (point-min) (point-max)
				 "patch" nil output-buffer nil
				 "-r" rej "--no-backup-if-mismatch"
				 "-l" "-f"
				 "-d" (expand-file-name
				       debbugs-gnu-current-directory)
				 "-p1")
	    (when old-rej
	      (with-temp-buffer
		(insert old-rej)
		(when (file-exists-p rej)
		  (insert-file-contents rej))
		(write-region (point-min) (point-max) rej nil 'silent)))))))
    (set-buffer output-buffer)
    (when (file-exists-p rej)
      (goto-char (point-max))
      (insert-file-contents-literally rej))
    (goto-char (point-max))
    (save-some-buffers t)
    (require 'compile)
    (mapc #'kill-process compilation-in-progress)
    (compile
     (format "cd %s; %s"
	     debbugs-gnu-current-directory
	     debbugs-gnu-compile-command))
    (vc-dir debbugs-gnu-current-directory)
    (vc-dir-hide-up-to-date)
    (goto-char (point-min))
    (sit-for 1)
    (vc-diff)
    ;; All these commands are asynchronous, so just wait a bit.  This
    ;; should be done properly a different way.
    (sit-for 2)
    ;; We've now done everything, so arrange the windows we need to see.
    (delete-other-windows)
    (switch-to-buffer output-buffer)
    (split-window)
    (split-window)
    (other-window 1)
    (switch-to-buffer "*compilation*")
    (goto-char (point-max))
    (other-window 1)
    (switch-to-buffer "*vc-diff*")
    (goto-char (point-min))))

(defun debbugs-gnu-diff-hunk-target-name (dir)
  (let ((names nil))
    (dolist (name (diff-hunk-file-names))
      ;; The function above may return names like
      ;; "lisp/custom.el 2013-06-14 12:10:30 +0000"
      (setq name (car (split-string name " ")))
      (unless (string-match "[ #<>]" name)
	(when (string-match "\\`/" name)
	  ;; This is an absolute path, so try to find the target.
	  (while (and (not (file-exists-p (expand-file-name name dir)))
		      (string-match "\\`[^/]*/" name))
	    (setq name (replace-match "" t t name))))
	;; See whether we can find the file.
	(when (or (not (string-match "/" name))
		  (and (string-match "^[ab]/" name)
		       (not (file-exists-p
			     (expand-file-name (substring name 2)
					       dir))))
		  (file-exists-p (expand-file-name name dir)))
	  ;; We have a simple patch that refers to a file somewhere in the
	  ;; tree.  Find it.
	  (setq name (car (sort (directory-files-recursively
				 dir
				 (concat "^" (regexp-quote
					      (file-name-nondirectory name))
					 "$"))
				#'string>))))
	(when name
	  (push name names))))
    ;; Return any of the guessed names.
    (car names)))

(defun debbugs-gnu-fix-patch (dir)
  (require 'diff-mode)
  (setq dir (directory-file-name (expand-file-name dir)))
  (goto-char (point-min))
  (while (re-search-forward diff-file-header-re nil t)
    (goto-char (match-beginning 0))
    (when-let ((target-name (debbugs-gnu-diff-hunk-target-name dir)))
      (when (and (string-match "^/" target-name)
		 (re-search-forward "^\\([+]+\\|-+\\) .*" nil t))
	(replace-match (concat (match-string 1)
			       " a"
			       (substring target-name (length dir)))
		       nil t)))
    (forward-line 2)))

(defun debbugs-gnu-find-contributor (string)
  "Search through ChangeLogs to find contributors."
  (interactive "sContributor match: ")
  (debbugs-gnu-init-current-directory)
  (let ((found 0)
	(match (concat "^[0-9].*" string)))
    (dolist (file (directory-files-recursively
		   debbugs-gnu-current-directory "ChangeLog\\(.[0-9]+\\)?$"))
      (with-temp-buffer
	(when (file-exists-p file)
	  (insert-file-contents file))
	(goto-char (point-min))
	(while (and (re-search-forward match nil t)
		    (not (looking-at ".*tiny change")))
	  (cl-incf found))))
    (message "%s is a contributor %d times" string found)
    found))

(defvar debbugs-gnu-patch-subject nil)

(defun debbugs-gnu-insert-changelog ()
  "Add a ChangeLog from a recently applied patch from a third party."
  (interactive)
  (unless (eq debbugs-gnu-mail-backend 'gnus)
    (error "This function only works with Gnus."))
  (let (from subject patch-subject changelog
	     patch-from)
    (with-current-buffer gnus-article-buffer
      (widen)
      (goto-char (point-min))
      (setq from (gnus-fetch-field "from")
	    subject (gnus-fetch-field "subject"))
      ;; If it's a patch formatted the right way, extract that data.
      (dolist (handle (mapcar #'cdr (gnus-article-mime-handles)))
	(when (string-match "diff\\|patch\\|plain"
			    (mm-handle-media-type handle))
	  (with-temp-buffer
	    (insert-buffer-substring (mm-handle-buffer handle))
	    (cond ((eq (mm-handle-encoding handle) 'base64)
		   (base64-decode-region (point-min) (point-max)))
		  ((eq (mm-handle-encoding handle) 'quoted-printable)
		   (quoted-printable-decode-region (point-min) (point-max))))
	    (setq patch-subject
		  (or (gnus-fetch-field "subject") patch-subject))
	    (when-let ((pf (gnus-fetch-field "from")))
	      (setq patch-from (mail-decode-encoded-address-string pf)))
	    (goto-char (point-min))
	    (when (re-search-forward "^[*] " nil t)
	      (let ((start (match-beginning 0)))
		(while (and (not (eobp))
			    (not (looking-at "---")))
		  (forward-line 1))
		(setq changelog (buffer-substring
				 start (line-end-position 0)))))))))
    (setq from (debbugs-gnu--parse-mail (or patch-from from)))
    (let ((add-log-full-name (car from))
	  (add-log-mailing-address (cadr from)))
      (add-change-log-entry-other-window)
      (debbugs-gnu-change-mode)
      (setq-local debbugs-gnu-patch-subject patch-subject)
      (when changelog
	(delete-region (line-beginning-position) (point-max))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert (debbugs-compat-string-replace "\r" "" changelog))
	  (indent-region (point-min) (point-max))))
      (let ((point (point)))
	(when (string-match "\\(bug#[0-9]+\\)" subject)
	  (insert " (" (match-string 1 subject) ")."))
	(goto-char point)))))

(defvar debbugs-gnu-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta m)] #'debbugs-gnu-insert-changelog)
    map))

(define-minor-mode debbugs-gnu-lisp-mode
  "Minor mode for providing a debbugs interface in Lisp buffers.

\\{debbugs-gnu-lisp-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-lisp-mode-map)

(defvar debbugs-gnu-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta m)] #'debbugs-gnu-diff-select)
    map))

(define-minor-mode debbugs-gnu-diff-mode
  "Minor mode for providing a debbugs interface in diff buffers.

\\{debbugs-gnu-diff-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-diff-mode-map)

(defun debbugs-gnu-diff-select ()
  "Select the diff under point."
  (interactive)
  (delete-other-windows)
  (diff-goto-source)
  (debbugs-gnu-lisp-mode))

(defvar debbugs-gnu-change-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta m)] #'debbugs-gnu-change-checkin)
    map))

(define-minor-mode debbugs-gnu-change-mode
  "Minor mode for providing a debbugs interface in ChangeLog buffers.

\\{debbugs-gnu-change-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-change-mode-map)

(defun debbugs-gnu-change-checkin ()
  "Prepare checking in the current changes."
  (interactive)
  (debbugs-gnu-init-current-directory)
  (save-some-buffers t)
  (when (get-buffer "*vc-dir*")
    (kill-buffer (get-buffer "*vc-dir*")))
  (let ((patch-subject debbugs-gnu-patch-subject))
    (vc-dir debbugs-gnu-current-directory)
    (goto-char (point-min))
    (while (not (search-forward "edited" nil t))
      (sit-for 0.01))
    (beginning-of-line)
    (while (search-forward "edited" nil t)
      (vc-dir-mark)
      (beginning-of-line))
    (vc-diff nil)
    (vc-next-action nil)
    (delete-region (point-min) (point-max))
    (log-edit-insert-changelog t)
    (debbugs-gnu-log-edit-mode)
    (delete-other-windows)
    (split-window)
    (other-window 1)
    (switch-to-buffer "*vc-diff*")
    (other-window 1)
    (when patch-subject
      (goto-char (point-min))
      (unless (re-search-forward "^Summary: " nil t)
	(insert "Summary: \n")
	(forward-line -1)
	(end-of-line))
      (insert (replace-regexp-in-string "^ *\\[PATCH\\] *" "" patch-subject))
      (beginning-of-line)
      (search-forward ": " nil t))))

(defvar debbugs-gnu-log-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta m)] #'debbugs-gnu-log-edit-done)
    map))

(define-minor-mode debbugs-gnu-log-edit-mode
  "Minor mode for providing a debbugs interface in log-edit buffers.

\\{debbugs-gnu-log-edit-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-log-edit-mode-map)

(defun debbugs-gnu--parse-mail (string)
  (let* ((mail-extr-ignore-single-names nil)
	 (mail-extr-ignore-realname-equals-mailbox-name nil))
    (mail-extract-address-components string)))

(defun debbugs-gnu-log-edit-done ()
  "Finish editing the log edit and commit the files."
  (interactive)
  (let ((author (mail-fetch-field "Author")))
    (when (> (length author) 0)
      (let ((from (debbugs-gnu--parse-mail author)))
	(when (and (zerop (debbugs-gnu-find-contributor
			   (let ((bits (split-string (car from))))
			     (cond
			      ((>= (length bits) 2)
			       (format "%s.*%s" (car bits) (car (last bits))))
			      ((= (length bits) 1)
			       (car bits))
			      ;; Fall back on the email address.
			      (t
			       (cadr from))))))
		   (y-or-n-p "Add paperwork exempt line?"))
	  (goto-char (point-max))
	  (end-of-line)
	  (terpri)
	  (insert "\nCopyright-paperwork-exempt: yes\n")))))
  ;; Commit.
  (log-edit-done))

(defun debbugs-gnu-save-cache ()
  "Save the bugs cache to a file."
  (interactive)
  (unless debbugs-cache-data
    (error "No data to cache"))
  (unless (file-exists-p "~/.emacs.d/debbugs-cache")
    (make-directory "~/.emacs.d/debbugs-cache" t))
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file "~/.emacs.d/debbugs-cache/list"
      (prin1 debbugs-cache-data (current-buffer)))))

(provide 'debbugs-gnu)

;;; TODO:

;; * Extend SOAP interface to get all bugs modified in a given timeframe.

;; * Extend SOAP interface to get existing package names on the
;;  server, in order not to hardcode them.

;; * The bug tracker should be aware of repositories, branches,
;;   commits, contributors, and ticket links or mentions in commit
;;   messages.
;;
;;   For me personally, if I can *see* the specific code that fixes a
;;   ticket inside the ticket as a commit, and click my way to the
;;   wider commit and then diff from before that commit against
;;   today's state of that code, I've built a mental map of the code
;;   that would otherwise take me a lot of work. That's one common
;;   workflow. Another is to view several commits that fix a single
;;   ticket in one place. So it's not revolutionary, just simpler and
;;   more straightforward for the user.
;;
;;   Being able to close a bug just by mentioning it in a certain way
;;   in the commit message and pushing that commit is also handy. You
;;   don't have to switch to the bug discussion and duplicate that
;;   info there manually.

;; * Contributors should be able to tag and notify each other.
;;
;;   You mean to (re)assign bugs to particular persons and things like that?

;;   Yes, plus ping someone or a team specifically: "hey, maybe the
;;   @gnus team should look at this" in a comment.

;; * Markdown should be well supported.

;; * Inline code comments should be easy, and linked to a commit (so
;;   an updated commit can resolve the comment). This is just the
;;   essential stuff.

;;   Rebase or amend+force push would update a branch destructively,
;;   which in a pull request context should show you that a comment
;;   was for a commit that's no longer in the branch. Furthermore some
;;   trackers allow you to mark a comment as resolved (e.g. Github
;;   recently added reactions, which can be used as ad-hoc markup).
;;
;;   Even if you don't rebase, but just push a new commit to the
;;   branch upon review, IIRC both Github and Gitlab can see that the
;;   changes that started a particular discussion are no longer there
;;   (and collapse the comment sub-thread a no longer relevant, while
;;   allowing the user to expand it again if they so wish).
;;
;;   I think I'm starting to see what you mean.  You're talking about
;;   a tight integration where a pull-request is also itself an issue,
;;   so the comments can be directly on the patch itself.  As opposed
;;   to having issues and pull-request be two separate things that can
;;   refer to each other via an indirection.
;;
;;   So this is particularly useful/meaningful when reviewing a
;;   proposed patch from another developer, rather than when
;;   interacting with an end-user trying to track down some bugs
;;   here's experiencing (which is the kind of use-case I've had in
;;   mind when working on BugIt).
;;
;;   But indeed, the two use-cases would best be served by the same
;;   tool since after the bug is tracked a patch might show up to fix
;;   it, after which a review process will come up.
;;
;;   And on the more basic level, compared to flat discussions in
;;   mailing lists, having separate subthread for each part of the
;;   patch the reviewer commented on, is great. You can have
;;   discussion sub-threads in the mailing list too, but people never
;;   split their emails in pieces that small.
;;
;; * The next link in the chain are CI/CD hooks. You can set up a
;;   Github repo, for instance, to build every pull request before the
;;   reviewer ever looks, which saves a lot of time with compiled
;;   languages. It will run tests and so on, but most important is
;;   that it keeps the context inside the pull request, you don't have
;;   to go elsewhere.

;;; debbugs-gnu.el ends here
