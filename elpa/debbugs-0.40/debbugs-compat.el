;;; debbugs-compat.el --- Compatibility library for debbugs  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hypermedia
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

;; Function `string-replace' is new in Emacs 28.1.
(defalias 'debbugs-compat-string-replace
  (if (fboundp 'string-replace)
      #'string-replace
    (lambda (from-string to-string in-string)
      (let ((case-fold-search nil))
        (replace-regexp-in-string
         (regexp-quote from-string) to-string in-string t t)))))

(provide 'debbugs-compat)

;;; debbugs-compat.el ends here
