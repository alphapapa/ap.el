;;; scrollkeeper.el --- Custom scrolling commands with visual guidelines  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/scrollkeeper.el
;; Package-Version: 20190109.629
;; Package-Commit: 3c4ac6b6b44686d31c260ee0b19daaee59bdccd6
;; Keywords: convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1"))

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

;; This package provides scrolling commands and several customization
;; options.  The commands use `pulse' to display a quickly fading
;; guideline on the line at which new contents are visible after
;; scrolling.  Also, scrolling can be divided into adjustable steps at
;; the desired speed.  Together, these features help your eyes to keep
;; their place in the buffer while scrolling.

;; To use this package, simply bind these commands to your preferred
;; keys:

;; + `scrollkeeper-up'
;; + `scrollkeeper-down'

;;;; Credits

;; + Inspired by Clemens Radermacher's blog post, <https://with-emacs.com/posts/keep-scrollin-scrollin-scrollin/>.
;; + Aided by studying Michael Heerdegen's package, <https://github.com/michael-heerdegen/on-screen.el>.

;;;; See also

;; These packages provide some similar functionality but in very different ways.

;; + https://github.com/michael-heerdegen/on-screen.el: A more complex
;; and comprehensive implementation that uses hooks to observe
;; scrolling in other windows.

;; + https://github.com/ska2342/highlight-context-line/: Highlights
;; the boundary line statically, using a minor mode rather than
;; commands.

;; + https://github.com/Malabarba/beacon: Highlights the cursor rather
;; than the boundary line between new and old content.

;;; TODOs:

;; MAYBE: Use different faces for scrolling down and up.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'pulse)

;;;; Customization

(defgroup scrollkeeper nil
  "Scroll with a helpful guideline."
  :link '(url-link "https://github.com/alphapapa/scrollkeeper.el")
  :group 'convenience)

(defcustom scrollkeeper-scroll-distance 0.75
  "Scroll this far with each command.
This may be let-bound in custom commands, or buffer-locally."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Ratio of window size")))

(defcustom scrollkeeper-scroll-steps 3
  "Scroll in this many steps.
The lines computed from `scrollkeeper-scroll-distance' are divided
into this many steps.

In a heavily font-locked buffer, scrolling may be slower, so this
variable could be set buffer-locally to a lower value."
  :type 'integer)

(defcustom scrollkeeper-scroll-step-delay 0.001
  "Scroll one step ahead at this interval, in seconds."
  :type 'float)

(defcustom scrollkeeper-guideline-pulse-interval 0.05
  "Step through guideline pulsing at this interval, in seconds."
  :type 'float)

(defcustom scrollkeeper-guideline-pulse-steps 10
  "Divide guideline pulsing into this many steps."
  :type 'integer)

(defcustom scrollkeeper-guideline-fn #'scrollkeeper--highlight
  "Display the guideline with this function."
  :type '(choice (const :tag "Highlight line" scrollkeeper--highlight)
                 (const :tag "Underline line" scrollkeeper--underline)
                 (const :tag "Insert thin line" scrollkeeper--thinline)))

(defcustom scrollkeeper-guideline-dynamic-background '(font-lock-string-face . :foreground)
  "Dynamically set guideline face background color.
When the `scrollkeeper-guideline' faces have unspecified
background colors, they are set according to this.  This allows
the guideline faces to automatically adapt to changing themes
without user intervention.

For example, you could set the face name to
`font-lock-string-face', and the attribute keyword to
`:foreground', and the guideline's background color would be set
to the string face's foreground color (which should be visible
against the background in every theme).

Setting the background colors of faces
`scrollkeeper-guideline-highlight' and
`scrollkeeper-guideline-thinline' overrides this setting."
  :type '(cons (face :tag "Face")
               (symbol :tag "Face attribute keyword")))

;;;; Faces

(defface scrollkeeper-guideline-highlight nil
  "Face for highlighting scrolling guideline.
If its background color is unspecified, it's set automatically
according to `scrollkeeper-guideline-dynamic-background', which see.")

(defface scrollkeeper-guideline-thinline
  ;; FIXME: 0.1 is still not as thin as I would like, but I don't know
  ;; if it's possible to make it thinner.
  `((t :height 0.1))
  "Face for thinline guideline.
If its background color is unspecified, it's set automatically
according to `scrollkeeper-guideline-inherit', which see.")

(defface scrollkeeper-guideline-underline
  ;; FIXME: The underline only applies to the empty space from the last
  ;; character on a line to the side of the window.  Not sure if this
  ;; can be fixed.

  ;; FIXME: I picked `font-lock-string-face' because it looks nice
  ;; with my theme.  Maybe not the best default.

  ;; NOTE: Unlike the other two faces, this doesn't need to dynamically
  ;; adapt, because its underline color can be set to the foreground
  ;; color of another face by inheriting from it.
  `((t :inherit font-lock-string-face
       :underline (:style line)))
  "Face for underline guideline.")

;;;; Commands

;;;###autoload
(cl-defun scrollkeeper-contents-up (&optional (lines scrollkeeper-scroll-distance))
  "Scroll page contents up by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'."
  (interactive)
  (let* ((lines (cl-typecase lines
                  (integer lines)
                  (float (floor (* lines (window-text-height))))))
         (step-lines (floor (/ lines scrollkeeper-scroll-steps)))
         (pulse-delay scrollkeeper-guideline-pulse-interval)
         (pulse-iterations scrollkeeper-guideline-pulse-steps))
    (save-excursion
      (move-to-window-line (if (< lines 0)
                               0
                             -1))
      (funcall scrollkeeper-guideline-fn))
    (dotimes (_ scrollkeeper-scroll-steps)
      (scroll-up step-lines)
      (sit-for scrollkeeper-scroll-step-delay))))

;;;###autoload
(defalias 'scrollkeeper-down #'scrollkeeper-contents-up)

;;;###autoload
(cl-defun scrollkeeper-contents-down (&optional (lines scrollkeeper-scroll-distance))
  "Scroll page contents down by LINES, displaying a guideline.
LINES may be an integer number of lines or a float ratio of
window height; see `scrollkeeper-scroll-distance'."
  (interactive)
  (scrollkeeper-contents-up (* -1 lines)))

;;;###autoload
(defalias 'scrollkeeper-up #'scrollkeeper-contents-down)

;;;; Functions

;; The first two functions check the background color of the appropriate face and, if
;; unspecified, set it according to `scrollkeeper-guideline-dynamic-background'.  This allows
;; the face to adapt to theme changes without user intervention.  Profiling shows that this
;; checking makes no discernible difference in performance, as e.g. `scrollkeeper--highlight'
;; runs in 0.0004 seconds with and without the checking.  Thanks to Steve Purcell for holding
;; this code to a higher standard.  :)

(defun scrollkeeper--highlight ()
  "Pulse-highlight the line at point."
  (when (eq 'unspecified (face-attribute 'scrollkeeper-guideline-highlight :background))
    (face-spec-set 'scrollkeeper-guideline-highlight
                   `((t :background ,(face-attribute (car scrollkeeper-guideline-dynamic-background)
                                                     (cdr scrollkeeper-guideline-dynamic-background))))
                   'face-defface-spec))
  (pulse-momentary-highlight-one-line (point) 'scrollkeeper-guideline-highlight))

(defun scrollkeeper--thinline ()
  "Pulse-highlight a thin line between lines."
  ;; Like `pulse-momentary-highlight-region'.
  (when (eq 'unspecified (face-attribute 'scrollkeeper-guideline-thinline :background))
    (face-spec-set 'scrollkeeper-guideline-thinline
                   `((t :background ,(face-attribute (car scrollkeeper-guideline-dynamic-background)
                                                     (cdr scrollkeeper-guideline-dynamic-background))))
                   'face-defface-spec))
  (save-excursion
    (let ((o (make-overlay (line-beginning-position) (line-beginning-position))))
      (overlay-put o 'pulse-delete t)
      (overlay-put o 'before-string (propertize "\n" 'face 'scrollkeeper-guideline-thinline))
      (pulse-momentary-highlight-overlay o 'scrollkeeper-guideline-thinline))))

(defun scrollkeeper--underline ()
  "Pulse-highlight an underline overlay on the line at point."
  ;; Like `pulse-momentary-highlight-region'.
  (save-excursion
    (let ((o (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put o 'pulse-delete t)
      ;; Thanks to `on-screen-make-narrow-line-overlay' for showing
      ;; how to get the details of the `space' `display' property
      ;; right.
      (overlay-put o 'after-string
                   (propertize " "
                               'face 'scrollkeeper-guideline-underline
                               'display `(space :align-to ,(window-width))
                               ;; FIXME: Not sure if `cursor' is necessary here.  Doesn't seem to have any effect.
                               'cursor 0))
      (pulse-momentary-highlight-overlay o 'scrollkeeper-guideline-underline))))

;;;; Footer

(provide 'scrollkeeper)

;;; scrollkeeper.el ends here
