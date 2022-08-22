;;; ef-day-theme.el --- Legible light theme -*- lexical-binding:t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Ef-Themes Development <~protesilaos/ef-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/ef-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/ef-themes
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

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

  (deftheme ef-day "Legible light theme.")

  (defconst ef-day-palette
    '(;; Basic tones
      (bg-main     "#fff5ea")
      (fg-main     "#584141")
      (bg-dim      "#f3ebdc")
      (fg-dim      "#63728f")
      (bg-alt      "#e9e0d8")
      (fg-alt      "#8f5f4a")

      (bg-active   "#d9d0c8")
      (bg-inactive "#f9f1e8")

      ;; Basic hues for foreground values
      (red             "#ba2d2f")
      (red-warmer      "#ce3f00")
      (red-cooler      "#cf2f4f")
      (red-faint       "#b05350")
      (green           "#007a0a")
      (green-warmer    "#5a7400")
      (green-cooler    "#0f7f5f")
      (green-faint     "#61756c")
      (yellow          "#a45a22")
      (yellow-warmer   "#b75515")
      (yellow-cooler   "#aa4f30")
      (yellow-faint    "#9a625a")
      (blue            "#375cc6")
      (blue-warmer     "#5f5fdf")
      (blue-cooler     "#265fbf")
      (blue-faint      "#4a659f")
      (magenta         "#ca3e54")
      (magenta-warmer  "#cb2f80")
      (magenta-cooler  "#8448aa")
      (magenta-faint   "#a04450")
      (cyan            "#3f60af")
      (cyan-warmer     "#3f6faf")
      (cyan-cooler     "#0f7b8f")
      (cyan-faint      "#4f6f8f")

      ;; Basic hues for background values
      (bg-red      "#ff8f88")
      (bg-green    "#96df80")
      (bg-yellow   "#efbf00")
      (bg-blue     "#cfceff")
      (bg-magenta  "#ff9fee")
      (bg-cyan     "#88cfd0")

      ;; Diffs
      (bg-added          "#d6efd2")
      (bg-added-faint    "#e5f5e0")
      (bg-added-refine   "#c8e3c0")

      (bg-changed        "#ffe5b9")
      (bg-changed-faint  "#ffefc5")
      (bg-changed-refine "#ffd09f")

      (bg-removed        "#ffd8d8")
      (bg-removed-faint  "#ffe5e5")
      (bg-removed-refine "#ffc0b0")

      ;; Graphs
      (red-graph-0-bg     "#ef7969")
      (red-graph-1-bg     "#ffaab4")
      (green-graph-0-bg   "#4faa09")
      (green-graph-1-bg   "#8fef00")
      (yellow-graph-0-bg  "#ffcf00")
      (yellow-graph-1-bg  "#f9ff00")
      (blue-graph-0-bg    "#7090ff")
      (blue-graph-1-bg    "#9fc6ff")
      (magenta-graph-0-bg "#e07fff")
      (magenta-graph-1-bg "#fad0ff")
      (cyan-graph-0-bg    "#70d3f0")
      (cyan-graph-1-bg    "#afefff")

      ;; Special hues
      (bg-mode-line  "#ffaf72") (fg-mode-line  "#542f38")
      (bg-accent     "#106246") (fg-accent     "#ffffff")
      (bg-completion "#ffd5d3")
      (bg-hover      "#b0e0df")
      (bg-hover-alt  "#febcaf")
      (bg-hl-line    "#f9e2b2")
      (bg-region     "#f0d2df")
      (bg-paren      "#8fcfdf")
      (bg-err        "#ffddee") ; check with err
      (bg-warning    "#ffe0aa") ; check with warning
      (bg-info       "#ddf5cc") ; check with info

      (border        "#ded0bb")
      (cursor        "#cf1f00")
      (fg-intense    "#000000")

      ;; Mappings
      (err red-warmer)
      (warning yellow-warmer)
      (info green)

      (link red)
      (link-alt green-warmer)
      (date cyan-warmer)
      (name yellow)
      (keybind red-cooler)

      (builtin red-cooler)
      (comment green-faint)
      (constant red-warmer)
      (fnname magenta)
      (keyword yellow)
      (preprocessor cyan-warmer)
      (docstring yellow-faint)
      (string green-warmer)
      (type green-cooler)
      (variable magenta-cooler)

      (accent-0 red)
      (accent-1 green-cooler)
      (accent-2 yellow)
      (accent-3 magenta-warmer)

      (mail-0 red)
      (mail-1 green)
      (mail-2 yellow)
      (mail-3 green-cooler)
      (mail-4 yellow-cooler)

      (rainbow-0 yellow)
      (rainbow-1 red)
      (rainbow-2 green-warmer)
      (rainbow-3 magenta-warmer)
      (rainbow-4 cyan)
      (rainbow-5 yellow-cooler)
      (rainbow-6 magenta-cooler)
      (rainbow-7 red-cooler)
      (rainbow-8 green-cooler))
    "The `ef-day' palette.")

  (ef-themes-theme ef-day ef-day-palette)

  (provide-theme 'ef-day))

;;; ef-day-theme.el ends here
