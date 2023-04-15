;;; switchy-window.el --- A most-recently-used window switcher  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Free Software Foundation, Inc
;;
;; Author: Tassilo Horn <tsdh@gnu.org>
;; Version: 1.3
;; Keywords: windows
;; Homepage: https://sr.ht/~tsdh/switchy-window/
;; Repository: https://git.sr.ht/~tsdh/switchy-window
;; Bug-Tracker: https://todo.sr.ht/~tsdh/switchy-window
;; Package-Requires: ((emacs "25.1") (compat "29.1.4.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; switchy-window.el is a most-recently-used window switcher.  It suits my
;; personal Emacs layout and workflow where I usually have at most two editing
;; windows but up to three side-windows which I have to select only seldomly.
;;
;; The idea of switchy-window is simple: when you invoke `switchy-window' in
;; quick succession, it will switch to one window after the other in
;; most-recently-used order.  Once you stop switching for long enough time
;; (`switchy-window-delay', 1.5 seconds by default), the selected window gets
;; locked in, i.e., its LRU timestamp is updated and this switching sequence is
;; ended.  Thusly, you can toggle between two windows simply by invoking
;; `switchy-window', waiting at least `switchy-window-delay', and then invoking
;; `switchy-window' again to switch back to the original window.
;;
;; Activate `switchy-window-minor-mode' which tracks window changes and bind
;; `switchy-window' to a key of your liking in `switchy-window-minor-mode-map'
;; (or globally, see the variable's docstring for examples).
;;
;; Hint: Since the order of window switching is not as obvious as it is with
;; `other-window', adding a bit visual feedback to window selection changes can
;; be helpful.  That can be done easily with the stock Emacs pulse.el, e.g.:
;;
;; (defun my-pulse-line-on-window-selection-change (frame)
;;   (when (eq frame (selected-frame))
;;     (pulse-momentary-highlight-one-line)))
;;
;; (add-hook 'window-selection-change-functions
;;           #'my-pulse-line-on-window-selection-change)

;;; Code:

(require 'compat)

(defgroup switchy-window nil
  "Switchy is a most-recently-used window-switcher."
  :group 'windows)

(defvar switchy-window--tick-counter 0
  "Values of this counter represent the most-recently-used order of windows.
Only for internal use.")

(defvar switchy-window--tick-alist nil
  "An alist with entries (WINDOW . TICK).
A higher TICK value means a window has more recently been
visited.  Only for internal use.")

(defcustom switchy-window-delay 1.5
  "Number of seconds before the current window gets locked in.
If more time elapses between consecutive invocations of
`switchy-window', the current window's tick (timestamp) is
updated in `switchy-window--tick-alist' and the current switching
cycle ends."
  :type 'number)

(defvar switchy-window--timer nil
  "The timer locking in the current window after `switchy-window-delay' seconds.
Only for internal use.")

(defvar switchy-window--visited-windows nil
  "The windows having already been visited in the current switching cycle.")

(defun switchy-window--on-window-selection-change (&optional frame)
  "Record the next tick value for the selected window of FRAME.
Meant to be used in `window-selection-change-functions' which is
arranged by `switchy-window-minor-mode'."
  (when (eq frame (selected-frame))
    (when switchy-window--timer
      (cancel-timer switchy-window--timer))
    (setq switchy-window--timer (run-at-time
                                 switchy-window-delay nil
                                 (lambda ()
                                   (setf (alist-get (selected-window)
                                                    switchy-window--tick-alist)
                                         (cl-incf switchy-window--tick-counter))
                                   (setq switchy-window--visited-windows nil))))))

(defvar-keymap switchy-window-minor-mode-map
  :doc "The mode map of `switchy-window-minor-mode'.
No keys are bound by default.  Bind the main command
`switchy-window' to a key of your liking, e.g.,

  ;; That's what I use.
  (keymap-set switchy-window-minor-mode-map \"C-<\" #\\='switchy-window)

  ;; Or as a substitute for `other-window'.
  (keymap-set switchy-window-minor-mode-map
              \"<remap> <other-window>\" #\\='switchy-window)")

;;;###autoload
(define-minor-mode switchy-window-minor-mode
  "Activates recording of window selection ticks.
Those are the timestamps for figuring out the most-recently-used
order of windows.

The minor-mode provides the keymap `switchy-window-minor-mode-map',
which see."
  :global t
  (if switchy-window-minor-mode
      (add-hook 'window-selection-change-functions
                #'switchy-window--on-window-selection-change)
    (remove-hook 'window-selection-change-functions
                 #'switchy-window--on-window-selection-change)))

;;;###autoload
(defun switchy-window (&optional arg)
  "Switch to other windows in most-recently-used order.
If prefix ARG is given, use least-recently-used order.

If the time between consecutive invocations is smaller than
`switchy-window-delay' seconds, selects one after the other window in
LRU order and cycles when all windows have been visited.  If
`switchy-window-delay' has passed, the current switching cycle ends and
the now selected window gets its tick updated (a kind of
timestamp)."
  (interactive)

  (unless switchy-window-minor-mode
    (user-error "switchy-window requires `switchy-window-minor-mode' being active"))

  ;; Remove dead windows.
  (setq switchy-window--tick-alist (seq-filter
                                    (lambda (e)
                                      (window-live-p (car e)))
                                    switchy-window--tick-alist))
  ;; Add windows never selected.
  (dolist (win (window-list (selected-frame)))
    (unless (assq win switchy-window--tick-alist)
      (setf (alist-get win switchy-window--tick-alist) 0)))

  ;; Ensure the current window is marked as visited.
  (setq switchy-window--visited-windows (cons (selected-window)
                                              switchy-window--visited-windows))

  (let ((win-entries (seq-filter
                      (lambda (e)
                        (let ((win (car e)))
                          (and (eq (window-frame win) (selected-frame))
                               (or (minibuffer-window-active-p win)
                                   (not (eq win (minibuffer-window
                                                 (selected-frame)))))
                               (not (memq win switchy-window--visited-windows)))))
                      switchy-window--tick-alist)))
    (if win-entries
        (when-let ((win (car (seq-reduce (lambda (x e)
                                           (if (and x (funcall (if arg #'< #'>)
                                                               (cdr x) (cdr e)))
                                               x
                                             e))
                                         win-entries nil))))
          (setq switchy-window--visited-windows
                (cons win switchy-window--visited-windows))
          (select-window win))
      ;; Start a new cycle if we're not at the start already, i.e., we visited
      ;; just one (the current) window.
      (when (length> switchy-window--visited-windows 1)
        (setq switchy-window--visited-windows nil)
        (switchy-window)))))

(provide 'switchy-window)

;;; switchy-window.el ends here
