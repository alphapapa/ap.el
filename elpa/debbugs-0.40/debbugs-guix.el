;;; debbugs-guix.el --- guix specific debbugs functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Joshua Branson <jbranso@dismail.de>
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

;; By default, `debbugs-gnu-search' searches for open and closed bugs
;; of all GNU packages, which can make searching a little slow.  To
;; remedy this situation, this file provides the functions
;; `debbugs-gnu-guix-search' and `debbugs-org-guix-search', which
;; search for open bugs that relate to the GNU guix.

;;; Code:

(declare-function debbugs-gnu-search "debbugs-gnu")
(declare-function debbugs-org-show-reports "debbugs-org")
(defvar debbugs-gnu-show-reports-function)

;;;###autoload
(defun debbugs-gnu-guix-search ()
  "Search for open guix bugs and patches."
  (interactive)
  (debbugs-gnu-search (read-string "Search String: ") '((pending . "pending"))
                      nil '("guix" "guix-patches") nil))

;;;###autoload
(defun debbugs-org-guix-search ()
  "Search for open guix bugs and patches and display the results in an \
org buffer."
  (interactive)
  (let ((debbugs-gnu-show-reports-function #'debbugs-org-show-reports))
    (debbugs-gnu-search (read-string "Search String: ") '((pending . "pending"))
                        nil '("guix" "guix-patches") nil)))

(provide 'debbugs-guix)

;;; debbugs-guix.el ends here
