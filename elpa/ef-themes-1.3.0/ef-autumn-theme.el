;;; ef-autumn-theme.el --- Legible dark theme with warm, varied colors (red, yellow, green, teal) -*- lexical-binding:t -*-

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

  (deftheme ef-autumn
    "Legible dark theme with warm, varied colors (red, yellow, green, teal).")

  (defconst ef-autumn-palette
    '(
;;; Basic values

      (bg-main     "#0f0e06")
      (fg-main     "#cfbcba")
      (bg-dim      "#26211d")
      (fg-dim      "#887c8a")
      (bg-alt      "#36322f")
      (fg-alt      "#70a89f")

      (bg-active   "#56524f")
      (bg-inactive "#15140d")

;;; Basic hues for foreground values

      (red             "#ef656a")
      (red-warmer      "#f06a3f")
      (red-cooler      "#ff7a7f")
      (red-faint       "#df7f7f")
      (green           "#2fa526")
      (green-warmer    "#64aa0f")
      (green-cooler    "#00b066")
      (green-faint     "#5f9f6f")
      (yellow          "#c48702")
      (yellow-warmer   "#d0730f")
      (yellow-cooler   "#df8f6f")
      (yellow-faint    "#cf9f7f")
      (blue            "#379cf6")
      (blue-warmer     "#6a88ff")
      (blue-cooler     "#029fff")
      (blue-faint      "#6a84af")
      (magenta         "#d570af")
      (magenta-warmer  "#e580ea")
      (magenta-cooler  "#af8aff")
      (magenta-faint   "#c590af")
      (cyan            "#4fb0cf")
      (cyan-warmer     "#6fafff")
      (cyan-cooler     "#3dbbb0")
      (cyan-faint      "#82a0af")

;;; Basic hues for background values

      (bg-red-intense     "#cd2f30")
      (bg-green-intense   "#20a020")
      (bg-yellow-intense  "#9a4f20")
      (bg-blue-intense    "#3f43af")
      (bg-magenta-intense "#b04fcf")
      (bg-cyan-intense    "#027080")

      (bg-red-subtle      "#77002a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#693200")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#611062")
      (bg-cyan-subtle     "#004065")

;;; Diffs

      (bg-added          "#17360f")
      (bg-added-faint    "#0a2900")
      (bg-added-refine   "#204810")
      (fg-added          "#a0e0a0")

      (bg-changed        "#363300")
      (bg-changed-faint  "#2a1f00")
      (bg-changed-refine "#4a4a00")
      (fg-changed        "#efef80")

      (bg-removed        "#4b120a")
      (bg-removed-faint  "#3a0a00")
      (bg-removed-refine "#6f1a16")
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

      (bg-mode-line       "#692a12")
      (fg-mode-line       "#feeeca")
      (bg-completion      "#36213a")
      (bg-hover           "#265f4a")
      (bg-hover-secondary "#55345a")
      (bg-hl-line         "#302a3a")
      (bg-region          "#3f1020")
      (bg-paren           "#2f6c4a")
      (bg-err             "#391400") ; check with err
      (bg-warning         "#332800") ; check with warning
      (bg-info            "#152a00") ; check with info

      (border        "#58514f")
      (cursor        "#ffaa33")
      (fg-intense    "#ffffff")

      (modeline-err     "#ff9fbf")
      (modeline-warning "#dfcf33")
      (modeline-info    "#4fdf5f")

      (underline-err     "#df2f2f")
      (underline-warning "#c0b000")
      (underline-info    "#22b022")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Conditional hues

      (bg-region-intense "#4f4400")

;;; Mappings

;;;; General mappings

      (err red-warmer)
      (warning yellow)
      (info green-cooler)

      (link yellow)
      (link-alt cyan-cooler)
      (name green-warmer)
      (keybind red-warmer)
      (identifier magenta-faint)
      (prompt cyan-cooler)

;;;; Code mappings

      (builtin red-cooler)
      (comment yellow-faint)
      (constant green-warmer)
      (fnname cyan-cooler)
      (keyword yellow)
      (preprocessor magenta)
      (docstring green-faint)
      (string red-warmer)
      (type green)
      (variable cyan-warmer)
      (rx-escape green-cooler) ; compare with `string'
      (rx-construct magenta-cooler)

;;;; Accent mappings

      (accent-0 green-cooler)
      (accent-1 yellow-warmer)
      (accent-2 cyan-cooler)
      (accent-3 magenta-cooler)

;;;; Date mappings

      (date-common yellow-cooler)
      (date-deadline red-warmer)
      (date-event fg-alt)
      (date-holiday cyan-cooler)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code yellow-warmer)
      (prose-done green-cooler)
      (prose-macro cyan-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag fg-alt)
      (prose-todo red-warmer)
      (prose-verbatim green-cooler)

;;;; Mail mappings

      (mail-cite-0 yellow-warmer)
      (mail-cite-1 green-warmer)
      (mail-cite-2 red)
      (mail-cite-3 yellow-cooler)
      (mail-part green-faint)
      (mail-recipient green-warmer)
      (mail-subject green-cooler)
      (mail-other yellow)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Rainbow mappings

      (rainbow-0 yellow-warmer)
      (rainbow-1 green-warmer)
      (rainbow-2 red)
      (rainbow-3 cyan-cooler)
      (rainbow-4 yellow-cooler)
      (rainbow-5 blue)
      (rainbow-6 red-cooler)
      (rainbow-7 green)
      (rainbow-8 yellow))
    "The `ef-autumn' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom ef-autumn-palette-overrides nil
    "Overrides for `ef-autumn-palette'.

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

  (ef-themes-theme ef-autumn ef-autumn-palette ef-autumn-palette-overrides)

  (provide-theme 'ef-autumn))

;;;###theme-autoload
(put 'ef-autumn 'theme-properties '(:background-mode dark :kind color-scheme :family ef))

;;; ef-autumn-theme.el ends here
