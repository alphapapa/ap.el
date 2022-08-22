;;; ef-deuteranopia-dark-theme.el --- Legible dark theme, optimized for red-green color deficiency -*- lexical-binding:t -*-

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

  ;; Most of the colors here, like the red and green hues, are defined
  ;; simply to preserve compatibility with the rest of the project.  We
  ;; don't actually rely on them for anything critical.
  (deftheme ef-deuteranopia-dark "Legible dark theme, optimized for red-green color deficiency.")

  (defconst ef-deuteranopia-dark-palette
    '(;; Basic tones
      (bg-main     "#000a1f")
      (fg-main     "#ddddee")
      (bg-dim      "#0f1c2d")
      (fg-dim      "#7f8797")
      (bg-alt      "#19263a")
      (fg-alt      "#90afef")

      (bg-active   "#30354f")
      (bg-inactive "#071225")

      ;; Basic hues for foreground values
      (red             "#cf8560")
      (red-warmer      "#e47360")
      (red-cooler      "#cf7a7a")
      (red-faint       "#b57f82")
      (green           "#3faa26")
      (green-warmer    "#7aad0f")
      (green-cooler    "#3fa672")
      (green-faint     "#61a06c")
      (yellow          "#aa9f32")
      (yellow-warmer   "#cfaf00")
      (yellow-cooler   "#bfaf7a")
      (yellow-faint    "#af9a6a")
      (blue            "#3f95f6")
      (blue-warmer     "#6a9fff")
      (blue-cooler     "#1f90ff")
      (blue-faint      "#7a94df")
      (magenta         "#b379bf")
      (magenta-warmer  "#af80ea")
      (magenta-cooler  "#9f95ff")
      (magenta-faint   "#c59fcf")
      (cyan            "#5faaef")
      (cyan-warmer     "#7fafff")
      (cyan-cooler     "#0db0ff")
      (cyan-faint      "#8aa0df")

      ;; Basic hues for background values
      (bg-red      "#8d4f5f")
      (bg-green    "#4f6f20")
      (bg-yellow   "#5f5f00")
      (bg-blue     "#264fa0")
      (bg-magenta  "#8040cf")
      (bg-cyan     "#00709e")

      ;; Diffs
      (bg-added          "#00234f")
      (bg-added-faint    "#00143f")
      (bg-added-refine   "#03395f")

      (bg-changed        "#2f123f")
      (bg-changed-faint  "#1f022f")
      (bg-changed-refine "#3f224f")

      (bg-removed        "#323200")
      (bg-removed-faint  "#281a00")
      (bg-removed-refine "#484800")

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
      (bg-mode-line  "#003f8f") (fg-mode-line  "#ffffff")
      (bg-accent     "#ffaa33") (fg-accent     "#000000")
      (bg-completion "#343420")
      (bg-hover      "#4f4f00")
      (bg-hover-alt  "#003a7f")
      (bg-hl-line    "#2e2e1b")
      (bg-region     "#202d3f")
      (bg-paren      "#0f4f9a")
      (bg-err        "#232d09") ; check with err
      (bg-warning    "#332600") ; check with warning
      (bg-info       "#001a4a") ; check with info

      (border        "#40455f")
      (cursor        "#ffff00")
      (fg-intense    "#ffffff")

      ;; Mappings
      (err yellow-warmer)
      (warning yellow)
      (info blue-cooler)

      (link blue)
      (link-alt yellow-cooler)
      (date yellow-cooler)
      (name blue-warmer)
      (keybind yellow-warmer)

      (builtin blue-warmer)
      (comment yellow-faint)
      (constant blue-cooler)
      (fnname yellow-cooler)
      (keyword yellow-warmer)
      (preprocessor green-cooler)
      (docstring cyan-faint)
      (string blue)
      (type magenta-cooler)
      (variable cyan-cooler)

      (accent-0 blue-cooler)
      (accent-1 yellow-warmer)
      (accent-2 cyan-warmer)
      (accent-3 magenta-cooler)

      (mail-0 blue-warmer)
      (mail-1 yellow)
      (mail-2 blue-cooler)
      (mail-3 yellow-cooler)
      (mail-4 cyan)

      (rainbow-0 yellow-warmer)
      (rainbow-1 blue)
      (rainbow-2 yellow-cooler)
      (rainbow-3 blue-warmer)
      (rainbow-4 yellow)
      (rainbow-5 blue-cooler)
      (rainbow-6 yellow-faint)
      (rainbow-7 blue-faint)
      (rainbow-8 magenta-faint))
    "The `ef-deuteranopia-dark' palette.")

  (ef-themes-theme ef-deuteranopia-dark ef-deuteranopia-dark-palette)

  (provide-theme 'ef-deuteranopia-dark))

;;; ef-deuteranopia-dark-theme.el ends here
