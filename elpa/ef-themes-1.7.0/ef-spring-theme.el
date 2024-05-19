;;; ef-spring-theme.el --- Legible light theme with cool, varied colors (green, cyan, red) -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/ef-themes
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

;;;###theme-autoload
  (deftheme ef-spring
    "Legible light theme with cool, varied colors (green, cyan, red)."
    :background-mode 'light
    :kind 'color-scheme
    :family 'ef)

  (defconst ef-spring-palette
    '(
;;; Basic values

      (bg-main     "#f6fff9")
      (fg-main     "#34494a")
      (bg-dim      "#e8f0f0")
      (fg-dim      "#777294")
      (bg-alt      "#e0e6e3")
      (fg-alt      "#9d5e7a")

      (bg-active   "#c0c6c3")
      (bg-inactive "#f0f8f4")

;;; Basic hues for foreground values

      (red             "#c42d2f")
      (red-warmer      "#d03003")
      (red-cooler      "#cf2f4f")
      (red-faint       "#b64850")
      (green           "#1a870f")
      (green-warmer    "#4a7d00")
      (green-cooler    "#007f68")
      (green-faint     "#61756c")
      (yellow          "#a45f22")
      (yellow-warmer   "#b6540f")
      (yellow-cooler   "#ae5a30")
      (yellow-faint    "#876450")
      (blue            "#375cc6")
      (blue-warmer     "#5f5fdf")
      (blue-cooler     "#265fbf")
      (blue-faint      "#6a65bf")
      (magenta         "#d5206f")
      (magenta-warmer  "#cb26a0")
      (magenta-cooler  "#9435b4")
      (magenta-faint   "#a04360")
      (cyan            "#1f6fbf")
      (cyan-warmer     "#3f6faf")
      (cyan-cooler     "#0f7b8f")
      (cyan-faint      "#5f6abf")

;;; Basic hues for background values

      (bg-red-intense     "#ff8f88")
      (bg-green-intense   "#7fdda0")
      (bg-yellow-intense  "#efcf00")
      (bg-blue-intense    "#afdfff")
      (bg-magenta-intense "#df8fff")
      (bg-cyan-intense    "#80caf0")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#afffbf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

;;; Diffs

      (bg-added          "#c9ffda")
      (bg-added-faint    "#d7ffe5")
      (bg-added-refine   "#b3efcf")
      (fg-added          "#005000")

      (bg-changed        "#f0e8bd")
      (bg-changed-faint  "#f5efcb")
      (bg-changed-refine "#efd299")
      (fg-changed        "#553d00")

      (bg-removed        "#ffd6e0")
      (bg-removed-faint  "#ffe9e6")
      (bg-removed-refine "#f5bfc8")
      (fg-removed        "#8f1313")

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#45c050")
      (bg-graph-green-1   "#75ef30")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7f90ff")
      (bg-graph-blue-1    "#a6c0ff")
      (bg-graph-magenta-0 "#e07fff")
      (bg-graph-magenta-1 "#fad0ff")
      (bg-graph-cyan-0    "#70d3f0")
      (bg-graph-cyan-1    "#afefff")

;;; Special hues

      (bg-mode-line       "#90e8b0")
      (fg-mode-line       "#243228")
      (bg-completion      "#ccedff")
      (bg-hover           "#ffc09f")
      (bg-hover-secondary "#f0bfff")
      (bg-hl-line         "#f9e0e5")
      (bg-paren           "#7fddd0")
      (bg-err             "#ffe8e0") ; check with err
      (bg-warning         "#ffecba") ; check with warning
      (bg-info            "#ccf5dd") ; check with info

      (border        "#b4c4c0")
      (cursor        "#bf005f")
      (fg-intense    "#000000")

      (modeline-err     "#9f0030")
      (modeline-warning "#5f0f9f")
      (modeline-info    "#1240af")

      (underline-err     "#ef0f1f")
      (underline-warning "#bf5f00")
      (underline-info    "#02af52")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

;;; Mappings

;;;; General mappings

      (bg-fringe unspecified)
      (fg-fringe unspecified)

      (err red-warmer)
      (warning yellow)
      (info green)

      (link cyan-cooler)
      (link-alt yellow-cooler)
      (name green-cooler)
      (keybind magenta-warmer)
      (identifier magenta-faint)
      (prompt green)

      (bg-region "#d0e6ff")
      (fg-region unspecified)

;;;; Code mappings

      (builtin green)
      (comment yellow-faint)
      (constant red-warmer)
      (fnname green-warmer)
      (keyword green-cooler)
      (preprocessor magenta-warmer)
      (docstring magenta-faint)
      (string yellow-warmer)
      (type magenta-cooler)
      (variable cyan)
      (rx-escape cyan) ; compare with `string'
      (rx-construct magenta-warmer)

;;;; Accent mappings

      (accent-0 green)
      (accent-1 red-cooler)
      (accent-2 blue)
      (accent-3 yellow-warmer)

;;;; Date mappings

      (date-common cyan)
      (date-deadline red-warmer)
      (date-event fg-alt)
      (date-holiday magenta-warmer)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Prose mappings

      (prose-code red-cooler)
      (prose-done green)
      (prose-macro blue)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-table-formula info)
      (prose-tag magenta-faint)
      (prose-todo red-warmer)
      (prose-verbatim green)

;;;; Mail mappings

      (mail-cite-0 green)
      (mail-cite-1 yellow-cooler)
      (mail-cite-2 blue-warmer)
      (mail-cite-3 magenta-cooler)
      (mail-part green-warmer)
      (mail-recipient green-cooler)
      (mail-subject cyan)
      (mail-other cyan-cooler)

;;;; Search mappings

      (bg-search-current bg-yellow-intense)
      (bg-search-lazy bg-blue-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-magenta-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-cyan-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

;;;; Tab mappings

      (bg-tab-bar      bg-alt)
      (bg-tab-current  bg-main)
      (bg-tab-other    bg-active)

;;;; Terminal mappings

      (bg-term-black           "black")
      (fg-term-black           "black")
      (bg-term-black-bright    "gray35")
      (fg-term-black-bright    "gray35")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-cooler)
      (fg-term-red-bright      red-cooler)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow-warmer)
      (fg-term-yellow          yellow-warmer)
      (bg-term-yellow-bright   yellow)
      (fg-term-yellow-bright   yellow)

      (bg-term-blue            blue-warmer)
      (fg-term-blue            blue-warmer)
      (bg-term-blue-bright     blue-cooler)
      (fg-term-blue-bright     blue-cooler)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "gray65")
      (fg-term-white           "gray65")
      (bg-term-white-bright    "white")
      (fg-term-white-bright    "white")

;;;; Rainbow mappings

      (rainbow-0 red-cooler)
      (rainbow-1 green-cooler)
      (rainbow-2 magenta-warmer)
      (rainbow-3 blue-warmer)
      (rainbow-4 yellow-warmer)
      (rainbow-5 cyan)
      (rainbow-6 magenta)
      (rainbow-7 magenta-cooler)
      (rainbow-8 yellow-cooler))
    "The `ef-spring' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

  (defcustom ef-spring-palette-overrides nil
    "Overrides for `ef-spring-palette'.

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

  (ef-themes-theme ef-spring ef-spring-palette ef-spring-palette-overrides)

  (provide-theme 'ef-spring))

;;; ef-spring-theme.el ends here
