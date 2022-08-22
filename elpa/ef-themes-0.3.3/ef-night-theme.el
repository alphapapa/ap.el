;;; ef-night-theme.el --- Legible dark theme -*- lexical-binding:t -*-

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

  (deftheme ef-night "Legible dark theme.")

  (defconst ef-night-palette
    '(;; Basic tones
      (bg-main     "#000e17")
      (fg-main     "#afbcbf")
      (bg-dim      "#0f1b29")
      (fg-dim      "#70819f")
      (bg-alt      "#1a2a2f")
      (fg-alt      "#b0a0a0")

      (bg-active   "#28353f")
      (bg-inactive "#0f121f")

      ;; Basic hues for foreground values
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

      ;; Basic hues for background values
      (bg-red      "#cd2f30")
      (bg-green    "#20a020")
      (bg-yellow   "#904f30")
      (bg-blue     "#2f439f")
      (bg-magenta  "#c040cf")
      (bg-cyan     "#027080")

      ;; Diffs
      (bg-added          "#002d1f")
      (bg-added-faint    "#001f18")
      (bg-added-refine   "#003b2f")

      (bg-changed        "#323200")
      (bg-changed-faint  "#292000")
      (bg-changed-refine "#484700")

      (bg-removed        "#450f1f")
      (bg-removed-faint  "#2f060f")
      (bg-removed-refine "#641426")

      ;; Graphs
      (red-graph-0-bg     "#b52c2c")
      (red-graph-1-bg     "#702020")
      (green-graph-0-bg   "#4fd100")
      (green-graph-1-bg   "#007800")
      (yellow-graph-0-bg  "#f1e00a")
      (yellow-graph-1-bg  "#b08600")
      (blue-graph-0-bg    "#2fafef")
      (blue-graph-1-bg    "#1f2f8f")
      (magenta-graph-0-bg "#bf94fe")
      (magenta-graph-1-bg "#5f509f")
      (cyan-graph-0-bg    "#47dfea")
      (cyan-graph-1-bg    "#00808f")

      ;; Special hues
      (bg-mode-line  "#003a7f") (fg-mode-line  "#ceeeff")
      (bg-accent     "#ff9270") (fg-accent     "#000000")
      (bg-completion "#00314f")
      (bg-hover      "#6f345a")
      (bg-hover-alt  "#664f4a")
      (bg-hl-line    "#002255")
      (bg-region     "#222f40")
      (bg-paren      "#703350")
      (bg-err        "#331419") ; check with err
      (bg-warning    "#332613") ; check with warning
      (bg-info       "#00251f") ; check with info

      (border        "#34445a")
      (cursor        "#00ccff")
      (fg-intense    "#ffffff")

      ;; Mappings
      (err red)
      (warning yellow-warmer)
      (info green-cooler)

      (link blue)
      (link-alt magenta)
      (date cyan)
      (name blue-warmer)
      (keybind blue-cooler)

      (builtin green-cooler)
      (comment yellow-faint)
      (constant magenta-cooler)
      (fnname cyan-cooler)
      (keyword blue-warmer)
      (preprocessor red-warmer)
      (docstring cyan-faint)
      (string blue-cooler)
      (type magenta-cooler)
      (variable cyan-warmer)

      (accent-0 blue-cooler)
      (accent-1 magenta-warmer)
      (accent-2 cyan-cooler)
      (accent-3 red-cooler)

      (mail-0 blue)
      (mail-1 magenta-warmer)
      (mail-2 green-cooler)
      (mail-3 yellow)
      (mail-4 magenta-cooler)

      (rainbow-0 cyan-warmer)
      (rainbow-1 magenta-cooler)
      (rainbow-2 yellow-cooler)
      (rainbow-3 blue-warmer)
      (rainbow-4 red)
      (rainbow-5 green)
      (rainbow-6 magenta)
      (rainbow-7 cyan)
      (rainbow-8 yellow))
    "The `ef-night' palette.")

  (ef-themes-theme ef-night ef-night-palette)

  (provide-theme 'ef-night))

;;; ef-night-theme.el ends here
