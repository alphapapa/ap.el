;;; ef-night-theme.el --- Legible dark theme with mostly green, blue, purple colors -*- lexical-binding:t -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Ef-Themes Development <~protesilaos/ef-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/ef-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/ef-themes
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `ef-themes' are a collection of light and dark themes for GNU
;; Emacs whose goal is to provide colorful ("pretty") yet legible
;; options for users who want something with a bit more flair than the
;; `modus-themes' (also designed by me).

;;; Code:



(eval-and-compile
  (require 'ef-themes)

  (deftheme ef-night
    "Legible dark theme with mostly green, blue, purple colors.")

  (defconst ef-night-palette
    '(
;;; Basic values

      (bg-main     "#000e17")
      (fg-main     "#afbcbf")
      (bg-dim      "#1a202b")
      (fg-dim      "#70819f")
      (bg-alt      "#242e39")
      (fg-alt      "#b0a0a0")

      (bg-active   "#444e59")
      (bg-inactive "#11141f")

;;; Basic hues for foreground values

      (red             "#ef656a")
      (red-warmer      "#f47360")
      (red-cooler      "#ef798f")
      (red-faint       "#d56f72")
      (green           "#1fa526")
      (green-warmer    "#50a22f")
      (green-cooler    "#00a972")
      (green-faint     "#61a06c")
      (yellow          "#c48502")
      (yellow-warmer   "#e6832f")
      (yellow-cooler   "#df8f6f")
      (yellow-faint    "#cf9f7f")
      (blue            "#379cf6")
      (blue-warmer     "#6a88ff")
      (blue-cooler     "#029fff")
      (blue-faint      "#7a94df")
      (magenta         "#d570af")
      (magenta-warmer  "#e580ea")
      (magenta-cooler  "#af8aff")
      (magenta-faint   "#c59faf")
      (cyan            "#4fb0cf")
      (cyan-warmer     "#6fafff")
      (cyan-cooler     "#3dc0b0")
      (cyan-faint      "#92b4df")

;;; Basic hues for background values

      (bg-red-intense     "#cd2f30")
      (bg-green-intense   "#20a020")
      (bg-yellow-intense  "#904f30")
      (bg-blue-intense    "#2f439f")
      (bg-magenta-intense "#b04fcf")
      (bg-cyan-intense    "#027080")

      (bg-red-subtle      "#6a002a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#5a3000")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#50105a")
      (bg-cyan-subtle     "#004065")

;;; Diffs

      (bg-added          "#00371f")
      (bg-added-faint    "#002918")
      (bg-added-refine   "#004c2f")
      (fg-added          "#a0e0a0")

      (bg-changed        "#363300")
      (bg-changed-faint  "#2a1f00")
      (bg-changed-refine "#4a4a00")
      (fg-changed        "#efef80")

      (bg-removed        "#450f1f")
      (bg-removed-faint  "#2f060f")
      (bg-removed-refine "#641426")
      (fg-removed        "#ffbfbf")

;;; Graphs

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#0fed00")
      (bg-graph-green-1   "#007800")
      (bg-graph-yellow-0  "#f1e00a")
      (bg-graph-yellow-1  "#b08940")
      (bg-graph-blue-0    "#2fafef")
      (bg-graph-blue-1    "#1f2f8f")
      (bg-graph-magenta-0 "#bf94fe")
      (bg-graph-magenta-1 "#5f509f")
      (bg-graph-cyan-0    "#47dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special hues

      (bg-mode-line       "#003a7f")
      (fg-mode-line       "#ceeeff")
      (bg-completion      "#00314f")
      (bg-hover           "#6f345a")
      (bg-hover-secondary "#493737")
      (bg-hl-line         "#002255")
      (bg-region          "#222f40")
      (bg-paren           "#664950")
      (bg-err             "#331419") ; check with err
      (bg-warning         "#332613") ; check with warning
      (bg-info            "#002e1f") ; check with info

      (border        "#3a4a66")
      (cursor        "#00ccff")
      (fg-intense    "#ffffff")

      (modeline-err     "#ff8faf")
      (modeline-warning "#dfcf00")
      (modeline-info    "#00e06f")

      (underline-err     "#df2f2f")
      (underline-warning "#c0b000")
      (underline-info    "#22b022")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Conditional hues

      (bg-region-intense "#3f2f60")

;;; Mappings

;;;; General mappings

      (err red)
      (warning yellow-warmer)
      (info green-cooler)

      (link cyan-warmer)
      (link-alt magenta)
      (name blue-warmer)
      (keybind green-cooler)
      (identifier magenta-faint)
      (prompt cyan-warmer)

;;;; Code mappings

      (builtin green-cooler)
      (comment yellow-faint)
      (constant magenta-cooler)
      (fnname cyan-cooler)
      (keyword blue-warmer)
      (preprocessor red-warmer)
      (docstring cyan-faint)
      (string blue-cooler)
      (type magenta-cooler)
      (variable cyan)
      (rx-escape green) ; compare with `string'
      (rx-construct magenta-warmer)

;;;; Accent mappings

      (accent-0 blue-cooler)
      (accent-1 magenta-warmer)
      (accent-2 cyan-cooler)
      (accent-3 red-cooler)

;;;; Date mappings

      (date-common cyan)
      (date-deadline red)
      (date-event fg-alt)
      (date-holiday magenta-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code magenta-warmer)
      (prose-done green-cooler)
      (prose-macro cyan-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag fg-alt)
      (prose-todo red)
      (prose-verbatim blue-cooler)

;;;; Mail mappings

      (mail-cite-0 blue)
      (mail-cite-1 magenta-warmer)
      (mail-cite-2 green-cooler)
      (mail-cite-3 yellow-cooler)
      (mail-part cyan)
      (mail-recipient blue-warmer)
      (mail-subject cyan-cooler)
      (mail-other cyan-warmer)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Rainbow mappings

      (rainbow-0 cyan-warmer)
      (rainbow-1 magenta-cooler)
      (rainbow-2 yellow-cooler)
      (rainbow-3 blue-warmer)
      (rainbow-4 magenta)
      (rainbow-5 green)
      (rainbow-6 yellow)
      (rainbow-7 cyan)
      (rainbow-8 green-warmer))
    "The `ef-night' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom ef-night-palette-overrides nil
    "Overrides for `ef-night-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Ef themes,
refer to `ef-themes-common-palette-overrides'.

To preview the palette entries, use `ef-themes-preview-colors' or
`ef-themes-preview-colors-current' (read the documentation for
further details)."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

  (ef-themes-theme ef-night ef-night-palette ef-night-palette-overrides)

  (provide-theme 'ef-night))

;;;###theme-autoload
(put 'ef-night 'theme-properties '(:background-mode dark :kind color-scheme :family ef))

;;; ef-night-theme.el ends here
