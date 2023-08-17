;;; ef-trio-dark-theme.el --- Legible dark theme with magenta, blue, teal colors -*- lexical-binding:t -*-

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

  (deftheme ef-trio-dark
    "Legible dark theme with magenta, blue, teal colors.")

  (defconst ef-trio-dark-palette
    '(
;;; Basic values

      (bg-main      "#160f0f")
      (fg-main      "#d8cfd5")
      (bg-dim       "#2a2228")
      (fg-dim       "#908890")
      (bg-alt       "#362f35")
      (fg-alt       "#afdacf")

      (bg-active    "#564f55")
      (bg-inactive  "#1c1719")

;;; Basic hues for foreground values

      (red             "#f48359")
      (red-warmer      "#ff7560")
      (red-cooler      "#ff85aa")
      (red-faint       "#e07a9a")
      (green           "#60b444")
      (green-warmer    "#a0c27f")
      (green-cooler    "#60bf88")
      (green-faint     "#61a06c")
      (yellow          "#d4a052")
      (yellow-warmer   "#ef926f")
      (yellow-cooler   "#ef9680")
      (yellow-faint    "#c7a07f")
      (blue            "#7fa5f6")
      (blue-warmer     "#8895ff")
      (blue-cooler     "#72afff")
      (blue-faint      "#7a94df")
      (magenta         "#d37faf")
      (magenta-warmer  "#e772df")
      (magenta-cooler  "#a698ef")
      (magenta-faint   "#c9addf")
      (cyan            "#8fbaff")
      (cyan-warmer     "#9ac2ff")
      (cyan-cooler     "#8fcfdf")
      (cyan-faint      "#8ac0ef")

;;; Basic hues for background values

      (bg-red-intense     "#bd1f30")
      (bg-green-intense   "#20a020")
      (bg-yellow-intense  "#845020")
      (bg-blue-intense    "#3633b0")
      (bg-magenta-intense "#b04fcf")
      (bg-cyan-intense    "#2270be")

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

      (bg-mode-line       "#6a294f")
      (fg-mode-line       "#ffdfdf")
      (bg-completion      "#2f335a")
      (bg-hover           "#004f3f")
      (bg-hover-secondary "#452f5f")
      (bg-hl-line         "#34223f")
      (bg-region          "#16304f")
      (bg-paren           "#2f605e")
      (bg-err             "#3a0f06") ; check with err
      (bg-warning         "#332910") ; check with warning
      (bg-info            "#1f2c00") ; check with info

      (border        "#605760")
      (cursor        "#ff99ff")
      (fg-intense    "#ffffff")

      (modeline-err     "#ff9fcf")
      (modeline-warning "#edbf00")
      (modeline-info    "#5fd0bf")

      (underline-err     "#df2f2f")
      (underline-warning "#c0b000")
      (underline-info    "#22b022")

      (bg-char-0 "#0050af")
      (bg-char-1 "#7f1f7f")
      (bg-char-2 "#625a00")

;;; Conditional hues

      (bg-region-intense "#5d336c")

;;; Mappings

;;;; General mappings

      (err red-warmer)
      (warning yellow)
      (info green)

      (link cyan)
      (link-alt green-warmer)
      (name blue)
      (keybind magenta-warmer)
      (identifier red-faint)
      (prompt blue-cooler)

;;;; Code mappings

      (builtin magenta-cooler)
      (comment yellow-faint)
      (constant magenta-warmer)
      (fnname blue-warmer)
      (keyword magenta)
      (preprocessor red-cooler)
      (docstring magenta-faint)
      (string green-cooler)
      (type cyan-cooler)
      (variable cyan-warmer)
      (rx-escape blue-warmer) ; compare with `string'
      (rx-construct red)

;;;; Accent mappings

      (accent-0 magenta-warmer)
      (accent-1 green-cooler)
      (accent-2 blue-warmer)
      (accent-3 yellow)

;;;; Date mappings

      (date-common cyan-cooler)
      (date-deadline red-warmer)
      (date-event fg-alt)
      (date-holiday magenta-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code green-cooler)
      (prose-done green)
      (prose-macro blue-warmer)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag magenta-faint)
      (prose-todo red-warmer)
      (prose-verbatim magenta-warmer)

;;;; Mail mappings

      (mail-cite-0 blue-warmer)
      (mail-cite-1 magenta-warmer)
      (mail-cite-2 green-cooler)
      (mail-cite-3 magenta-cooler)
      (mail-part cyan-cooler)
      (mail-recipient blue)
      (mail-subject magenta)
      (mail-other cyan)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Rainbow mappings

      (rainbow-0 magenta-warmer)
      (rainbow-1 blue-warmer)
      (rainbow-2 green-cooler)
      (rainbow-3 magenta)
      (rainbow-4 cyan)
      (rainbow-5 green-warmer)
      (rainbow-6 magenta-cooler)
      (rainbow-7 cyan-warmer)
      (rainbow-8 yellow-cooler))
    "The `ef-trio-dark' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom ef-trio-dark-palette-overrides nil
    "Overrides for `ef-trio-dark-palette'.

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

  (ef-themes-theme ef-trio-dark ef-trio-dark-palette ef-trio-dark-palette-overrides)

  (provide-theme 'ef-trio-dark))

;;;###theme-autoload
(put 'ef-trio-dark 'theme-properties '(:background-mode dark :kind color-scheme :family ef))

;;; ef-trio-dark-theme.el ends here
