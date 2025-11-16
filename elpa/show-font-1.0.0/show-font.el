;;; show-font.el --- Show font features in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/show-font
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, writing, font

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show font features in a buffer.  This depends on the font being
;; available on the system.
;;
;; Consult the manual for further information.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup show-font nil
  "Show font features in a buffer."
  :group 'font
  :link '(info-link "(show-font) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/show-font"))

;; NOTE 2025-04-24: We do not need to check for capital letters.  A
;; font that does not support those is a weird case that is not our
;; problem.
(defconst show-font-latin-characters
  (number-sequence ?a ?z)
  "The Latin lower-case characters.")

(defconst show-font-icon-characters
  (number-sequence #xE000 #xF8FF)
  "A sequence of characters in the Unicode Private Use Area.
The PUA is typically used by icon fonts.")

(defconst show-font-emoji-characters
  '(#x1F600 #x1F601 #x1F602 #x1F349)
  "A sequence of characters in the emoji range.")

(defconst show-font-arabic-characters
  (number-sequence #x0627 #x064A)
  "The Arabic characters from alif to yeh.")

(defconst show-font-greek-characters
  (number-sequence ?α ?ω)
  "The Greek lower-case characters.")

(defconst show-font-chinese-characters
  '(#x4E00 #x4EBA #x5927 #x5B66)
  "A sequence of common Chinese characters.")

(defconst show-font-japanese-characters
  '(#x3042 #x3044 #x3046 #x30A2 #x30A4 #x30A6)
  "A sequence of common Japanese characters.")

(defconst show-font-korean-characters
  '(#xAC00 #xAC01 #xAC02 #xD55C #xAE00)
  "A sequence of common Korean characters.")

(defconst show-font-russian-characters
  (number-sequence ?а ?я)
  "The Russian lower-case characters.")

(defconst show-font-pangrams
  '((fox . "The quick brown fox jumps over the lazy dog")
    (wizards . "Grumpy wizards make toxic brew for the evil queen and jack")
    (gunboats . "A quick movement of the enemy will jeopardize six gunboats")
    (prot . "Protesilaos may find zesty owls and quiet jays vexing the black cat"))
  "Default list of pangrams.")

(defcustom show-font-pangram 'prot
  "Pangram to display previewed font in.
This can be a symbol among the `car' of each element in
`show-font-pangrams' or it can be a string.  The string does not
actually need to be a pangram, though users can still have fun
experimenting with `show-font-pangram-p'."
  :package-version '(show-font . "0.1.0")
  :type `(choice
          ,@(mapcar
             (lambda (element)
               (list 'const :tag (cdr element) (car element)))
             show-font-pangrams)
          (string :tag "A custom pangram"))
  :group 'show-font)

(defcustom show-font-arabic-sample
  "بروتيسيلاوس لا يستطيع قراءة اللغة العربية" ; Protesilaos cannot read Arabic
  "Character sample to showcase Arabic fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-chinese-sample
  "普罗泰西劳斯无法阅读中文" ; Protesilaos cannot read Chinese
  "Character sample to showcase Chinese fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-emoji-sample
  "😀😁😂🤣😃😄😅😆😉😊😋😎😍😘🥰"
  "Character sample to showcase emoji fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "0.4.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-greek-sample
  "Πρωτεσίλαος ο φιλόσοφος του οποίου τα έργα βρίθουν αστειισμών"
  "Character sample to showcase Greek fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-japanese-sample
  "プロテシラオスは日本語を読むことができません" ; Protesilaos cannot read Japanese
  "Character sample to showcase Japanese fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-korean-sample
  "프로테실라오스는 한국어를 읽을 수 없습니다" ; Protesilaos cannot read Korean
  "Character sample to showcase Korean fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-mathematics-sample
  "x = (-b ± √(b² - 4ac)) / 2a"
  "Character sample to showcase mathematical fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-music-sample
  "♩♪♫♬"
  "Character sample to showcase music fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-russian-sample
  "Протесилаос не умеет читать по-русски" ; Protesilaos cannot read Russian
  "Character sample to showcase Russian fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-icon-sample
  (mapconcat #'char-to-string (seq-take show-font-icon-characters 10))
  "Character sample to showcase icon fonts.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "0.4.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-symbols-sample
  "←→↑↓ ─│┌┐└┘ €¥£₩ ■□▲▶▼◀◆ ● ±×÷≠≤≥∞ ♩♪♫♬"
  "Character sample to showcase Symbols.
This is displayed in the buffer produced by the command
`show-font-select-preview' and `show-font-tabulated'."
  :package-version '(show-font . "1.0.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-character-sample
  "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X Ee€£Ll .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"
  "Character sample to showcase font qualities.
This is displayed in the buffer produced by the command
`show-font-select-preview'."
  :package-version '(show-font . "0.1.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-sentences-sample
  (list
   "anonymous jiggly bidirectional numeral equip toxic yacht zetetic zodiac"
   "because illicit mnemonic gimmicky query incandescent transcendent"
   "gymnasium interoperability bifurcation amphibian quixotic verboten"
   "incongruous malfeasance syntax hitherto yesterday paramilitary"
   "coagulation acerbic admiration juxtapose superjacent tequila"
   "hyperion scholarch industrious quotidian caffeine intergovernmentalism"
   "obfuscate liaison voyage reify antipode heteroclite sovereignty")
  "List of strings that can exhibit common patterns or letter combinations.
This is displayed in the buffer produced by the command
`show-font-select-preview'."
  :package-version '(show-font . "0.3.0")
  :type 'string
  :group 'show-font)

(defcustom show-font-display-buffer-action-alist
  '((display-buffer-at-bottom)
    (dedicated . t)
    (preserve-size . (t . t)))
  "The `display-buffer' action alist for displaying a font list.
This is the same data that is passed to `display-buffer-alist'.
Read Info node `(elisp) Displaying Buffers'.  As such, it is
meant for experienced users.

Example of a valid value:

    \\='((display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.2)
      (preserve-size . (t . t)))

The value may also be a function, which returns a `display-buffer'
action alist."
  :group 'show-font
  :package-version '(show-font . "0.2.0")
  :type `(choice
          (alist :key-type
                 (choice :tag "Condition"
                         regexp
                         (function :tag "Matcher function"))
                 :value-type ,display-buffer--action-custom-type)
          (function :tag "Custom function to return an action alist"))
  :risky t)

(defconst show-font-greek-families
  '("Gentium" "Gentium Book Plus" "Gentium Plus" "GentiumAlt"
    "GFS Artemisia" "GFS Baskerville" "GFS BodoniClassic" "GFS Complutum"
    "GFS Didot" "GFS Didot Classic" "GFS Gazis" "GFS Neohellenic" "GFS Olga"
    "GFS Porson" "GFS Solomos" "GFS Theokritos" )
  "List of families that specialise in the Greek language.
Such fonts typically also support Latin, but we want to highlight the
fact that they are well suited for Greek.")

(defconst show-font-arabic-families
  '("AlArabiya" "AlBattar" "AlHor" "AlManzomah" "AlYarmook"
    "Dimnah" "Hani" "Haramain" "Hor" "Kayrawan" "Khalid" "Mashq"
    "Nagham" "Noto Kufi Arabic" "Noto Naskh Arabic" "Noto Sans Arabic"
    "Rehan" "Sharjah" "Sindbad")
  "List of families that specialise in Arabic.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-chinese-families
  '("Noto Sans CJK HK" "Noto Sans Mono CJK HK"
    "Noto Sans CJK SC" "Noto Sans CJK TC"
    "Noto Sans Mono CJK SC" "Noto Sans Mono CJK TC"
    "Noto Serif CJK HK" "Noto Serif Mono CJK HK"
    "Noto Serif CJK SC" "Noto Serif CJK TC"
    "Noto Serif Mono CJK SC" "Noto Serif Mono CJK TC")
  "List of families that specialise in Chinese.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-japanese-families
  '("Japan" "Noto Sans CJK JP" "Noto Sans Mono CJK JP"
    "Noto Serif CJK JP" "Noto Serif Mono CJK JP")
  "List of families that specialise in Japanese.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-korean-families
  '("Noto Sans CJK KR" "Noto Sans Mono CJK KR"
    "Noto Serif CJK KR" "Noto Serif Mono CJK KR")
  "List of families that specialise in Korean.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-russian-families
  '("PT Mono" "PT Sans" "PT Sans Caption" "PT Sans Narrow" "PT Serif" "PT Serif Caption")
  "List of families that specialise in Russian.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-mathematics-families
  '("MathJax_Fraktur" "MathJax_Main" "MathJax_SansSerif")
  "List of families that specialise in rendering Mathematical formulas.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-music-families
  '("Denemo" "Emmentaler" "Musica" "Nada")
  "List of families that specialise in rendering music notation.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-symbols-families
  '("Noto Sans Symbols" "Symbola")
  "List of families that specialise in rendering symbols.
Also see `show-font-greek-families' for the rationale of grouping font
families in distinct variables.")

(defconst show-font-hidden-families
  '("D050000L" "Droid Sans Fallback" "MathJax_AMS" "MathJax_Caligraphic" "MathJax_Math"
    "MathJax_Size1" "MathJax_Size2" "MathJax_Size3" "MathJax_Size4" "MathJax_Script"
    "MathJax_Typewriter" "MathJax_Vector" "MathJax_Vector-Bold" "MathJax_WinChrome"
    "MathJax_WinIE6" "Standard Symbols PS" "Symbols Nerd Font Mono" "Unifont CSUR"
    "Unifont Sample" "Unifont Upper" "Unifont-JP" "feta26")
  "List of families to omit from the `show-font-tabulated'.")

;;;; Faces

(defgroup show-font-faces nil
  "Show font features in a buffer."
  :group 'show-font
  :link '(info-link "(show-font) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/show-font"))

(defface show-font-small '((t :height 0.8))
  "Face for font preview at small height."
  :group 'show-font-faces)

(defface show-font-regular '((t :height 1.0))
  "Face for font preview at regular height."
  :group 'show-font-faces)

(defface show-font-medium '((t :height 1.3))
  "Face for font preview at medium height."
  :group 'show-font-faces)

(defface show-font-large '((t :height 1.8))
  "Face for font preview at large height."
  :group 'show-font-faces)

(defconst show-font-title-common
  '((((class color) (min-colors 88) (background dark))
     :foreground "#fff29a")
    (((class color) (min-colors 88) (background light))
     :foreground "#873300")
    (t
     :inherit warning))
  "Common face attributes for titles.")

(defface show-font-title
  `((default :height 2.6)
    ,@show-font-title-common)
  "Face for font preview title."
  :group 'show-font-faces)

(define-obsolete-face-alias
 'show-font-title-small
 'show-font-title-in-listing
 "0.3.0")

(defface show-font-title-in-listing
  `((default :height 1.0)
    ,@show-font-title-common)
  "Face for font preview title in listings (see `show-font-tabulated')."
  :group 'show-font-faces)

(defface show-font-misc '((t :inherit shadow))
  "Face for other, less important, elements in a preview.")

(defface show-font-button '((t :inherit button))
  "Face for buttons, like to install a missing font.")

;;;; Helper functions

(defun show-font-pangram-p (string &optional characters)
  "Return non-nil if STRING is a pangram.
With optional CHARACTERS as a list of single character strings, test
that all of them occur at least once in STRING.

If there are characters missing from STRING, print them in a message and
return nil."
  (let ((missing-characters nil))
    (dolist (character (or characters (mapcar #'string show-font-latin-characters)))
      (unless (string-match-p character string)
        (push character missing-characters)))
    (if (not missing-characters)
        t
      (message "Still missing: %s" (mapconcat #'identity missing-characters ", "))
      nil)))

(defmacro show-font--define-prefer-check (language)
  "Define a function to check if LANGUAGE has preferred fonts."
  (let ((families (intern (format "show-font-%s-families" language))))
    `(defun ,(intern (format "show-font--prefers-%s-p" language)) (family)
       ,(format "Return non-nil if FAMILY is among %s." families)
       (member family ,families))))

(show-font--define-prefer-check arabic)
(show-font--define-prefer-check chinese)
(show-font--define-prefer-check greek)
(show-font--define-prefer-check japanese)
(show-font--define-prefer-check korean)
(show-font--define-prefer-check mathematics)
(show-font--define-prefer-check music)
(show-font--define-prefer-check russian)
(show-font--define-prefer-check symbols)

;;;###autoload
(defun show-font-handler (operation &rest args)
  "Handle the given I/O `file-name-handler-alist' OPERATION with ARGS.
Determine how to render the font file contents in a buffer."
  (if-let* ((_ (eq operation 'insert-file-contents))
            (filename (car args))
            (visit (cadr args)))
      (progn
        (setq buffer-file-name filename)
        (show-font--add-text))
    ;; Handle any operation we do not know about.  This is copied from
    ;; the example shown in (info "(elisp) Magic File Names").
    (let ((inhibit-file-name-handlers
           (cons #'show-font-handler
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))

(defun show-font--get-attribute-from-file (attribute &optional file)
  "Get font family ATTRIBUTE from the current file or given FILE.
ATTRIBUTE is a string, such as \"family\" or \"fullname\", which is
matched against the output of the `fc-scan' executable."
  ;; FIXME 2024-09-06: Make this work with other font backends.
  (unless (executable-find "fc-scan")
    (error "Cannot find `fc-scan' executable; will not render font"))
  (when-let* ((f (or file buffer-file-name))
              (_ (string-match-p "\\.\\(ttf\\|otf\\)\\'" f))
              (output (shell-command-to-string (format "fc-scan -f \"%%{%s}\" %s"
                                                       (shell-quote-argument attribute)
                                                       (shell-quote-argument f)))))
    (if (string-match-p "," output)
        (car (split-string output ","))
      output)))

(defun show-font-get-installed-font-families (&optional regexp show-hidden)
  "Return list of installed font families names.
With optional REGEXP filter the list to only include fonts whose name
matches the given regular expression.

With optional SHOW-HIDDEN, include the `show-font-hidden-families'."
  (let ((fonts (thread-last (x-family-fonts)
                            (mapcar
                             (lambda (font)
                               (when-let* ((family (format "%s" (aref font 0))))
                                 family)))
                            (delq nil)
                            (delete-dups))))
    (when regexp
      (setq fonts (seq-filter (lambda (family) (string-match-p regexp family)) fonts)))
    (unless show-hidden
      (setq fonts (seq-remove (lambda (family) (member family show-font-hidden-families)) fonts)))
    (sort fonts #'string-lessp)))

(defun show-font--displays-characters-p (family characters)
  "Return non-nil if the font FAMILY can display CHARACTERS.
CHARACTERS is a sequence of numbers, corresponding to characters."
  (if-let* ((font-object (find-font (font-spec :family family))))
      (catch 'exit
        (dolist (character characters)
          (or (font-has-char-p font-object character)
              (throw 'exit nil)))
        t)
    (error "No font object for family `%s'" family)))

(defmacro show-font--define-display-check (language)
  "Define a `show-font--displays-characters-p' function for LANGUAGE."
  `(defun ,(intern (format "show-font--displays-%s-p" language)) (family)
     ,(format
       "Return non-nil if the font FAMILY can display %s."
       language)
     (show-font--displays-characters-p family ,(intern (format "show-font-%s-characters" language)))))

(show-font--define-display-check arabic)
(show-font--define-display-check chinese)
(show-font--define-display-check emoji)
(show-font--define-display-check greek)
(show-font--define-display-check icon)
(show-font--define-display-check japanese)
(show-font--define-display-check korean)
(show-font--define-display-check latin)
(show-font--define-display-check russian)

(defun show-font-installed-p (family &optional regexp)
  "Return non-nil if font family FAMILY is installed on the system.
FAMILY is a string like those of `show-font-get-installed-font-families'.
With optional REGEXP filter the list to only include fonts whose name
matches the given regular expression."
  (member family (show-font-get-installed-font-families regexp)))

(defun show-font--get-installed-font-files ()
  "Get list of font files available on the system."
  (unless (executable-find "fc-list")
    (error "Cannot find `fc-list' executable; will not find installed fonts"))
  ;; FIXME 2024-09-06: Make this work with other font backends.
  (process-lines "fc-list" "-f" (format "%%{%s}\n" "file")))

(defun show-font-installed-file-p (file)
  "Return non-nil if FILE is among `show-font--get-installed-font-files'."
  (member file (show-font--get-installed-font-files)))

(defun show-font--get-pangram ()
  "Return `show-font-pangram' or fallback string."
  (cond
   ((stringp show-font-pangram) show-font-pangram)
   ((alist-get show-font-pangram show-font-pangrams))
   (t "No string or acceptable symbol value for `show-font-pangram', but this will do...")))

(defun show-font--install-get-destination ()
  "Return directory where fonts can be copied locally."
  (cond
   ((member system-type '(gnu gnu/linux))
    (expand-file-name "~/.local/share/fonts/"))
   ((eq system-type 'darwin)
    (expand-file-name "~/Library/Fonts/"))
   (t
    (error "Unknown destination for Operating System of type `%s'" system-type))))

(defun show-font--install-confirmation (destination)
  "Prompt whether to copy the font to DESTINATION."
  (y-or-n-p (format "Install font by copying it to `%s'?" destination)))

(defun show-font--install (file)
  "Install the font FILE."
  (when-let* ((destination (show-font--install-get-destination))
              (_ (show-font--install-confirmation destination)))
    (copy-file file destination 1) ; ask for confirmation to overwrite
    (message "Copied `%s' to `%s'; now updating the font cache" file destination)
    ;; FIXME 2024-09-06: How to do the same on all operating systems?
    (shell-command-to-string (format "fc-cache -f -v"))
    (message "Font installed; restart Emacs to notice the effect")))

(defun show-font-install (&optional file)
  "Install font FILE locally.
FILE must be of type TTF or OTF and must not already be installed (per
`show-font-installed-file-p')."
  (let ((f (or file buffer-file-name)))
    (if (string-match-p "\\.\\(ttf\\|otf\\)\\'" f)
        (cond
         ((show-font-installed-file-p f)
          (user-error "`%s' is already installed; aborting" f))
         (t
          (show-font--install f)))
      (user-error "`%s' is not a known font file (TTF or OTF); aborting" f))))

(defun show-font--string-p (string)
  "Return non-nil if STRING is a string that is not empty."
  (and (stringp string) (not (string-blank-p string))))

(defun show-font--prepare-text-generic-sample (check-type family language generic-title-font)
  "Return generic `show-font--prepare-text-subr' sample of FAMILY for LANGUAGE.
Use CHECK-TYPE as a symbol of `displays' or `prefers' to run the
corresponding checker function, like `show-font--displays-greek-p'
instead of `show-font--prefers-greek-p'.

If GENERIC-TITLE-FONT is non-nil, do not try to render the title in
FAMILY but use whatever the `default' face is."
  (unless (memq check-type '(prefers displays))
    (error "The CHECK-TYPE must be the symbol `prefers' or `displays'"))
  (let* ((check-fn (intern-soft (format "show-font--%s-%s-p" check-type language)))
         (language-sample (or (intern-soft (format "show-font-%s-sample" language))
                              (error "There is no known sample for `%s'" language)))
         (sample (symbol-value language-sample)))
    (when (funcall check-fn family)
      (let ((output-sample nil))
        (dolist (face '(show-font-small show-font-regular show-font-medium show-font-large))
          (push (propertize sample 'face (list face :family family)) output-sample))
        (concat
         (propertize (or family (show-font--get-attribute-from-file "fullname"))
                     'face (cond
                            (generic-title-font
                             'show-font-title-in-listing)
                            ((show-font--displays-latin-p family)
                             (list 'show-font-title-in-listing :family family))
                            (t
                             'show-font-title-in-listing)))
         "\n"
         (make-separator-line)
         "\n"
         (mapconcat #'identity (nreverse output-sample) "\n"))))))

(defun show-font--prepare-text-subr (&optional family)
  "Prepare pangram text at varying font heights for the current font file.
With optional FAMILY, prepare a preview for the given font family
instead of that of the file."
  (cond
   ((not (display-graphic-p))
    (propertize "Fonts cannot be displayed in a terminal or TTY." 'face 'show-font-title))
   ((and (not family)
         (not (show-font-installed-file-p buffer-file-name)))
    nil)
   ((show-font--prepare-text-generic-sample 'prefers family 'mathematics :generic-title-family))
   ((show-font--prepare-text-generic-sample 'prefers family 'music :generic-title-family))
   ((show-font--prepare-text-generic-sample 'prefers family 'symbols :generic-title-family))
   ((show-font--prepare-text-generic-sample 'displays family 'emoji :generic-title-family))
   ((show-font--prepare-text-generic-sample 'displays family 'icon :generic-title-family))
   ((show-font--prepare-text-generic-sample 'prefers family 'arabic nil))
   ((show-font--prepare-text-generic-sample 'prefers family 'chinese nil))
   ;; NOTE 2025-09-06: Many Latin fonts support Greek characters.
   ;; If we check for `show-font--displays-greek-p' here we will end
   ;; up with very few Latin fonts, which does not look right.
   ((show-font--prepare-text-generic-sample 'prefers family 'greek nil))
   ((show-font--prepare-text-generic-sample 'prefers family 'japanese nil))
   ((show-font--prepare-text-generic-sample 'prefers family 'korean nil))
   ((show-font--prepare-text-generic-sample 'prefers family 'russian nil))
   ((show-font--displays-latin-p family)
    (let* ((faces '(show-font-small show-font-regular show-font-medium show-font-large))
           (list-of-lines nil)
           (list-of-blocks nil)
           (list-of-sentences nil)
           (pangram (show-font--get-pangram))
           (name (or family (show-font--get-attribute-from-file "fullname")))
           (family (or family (show-font--get-attribute-from-file "family")))
           (character-sample show-font-character-sample)
           (propertize-sample-p (show-font--string-p character-sample)))
      (dolist (face faces)
        (push (propertize pangram 'face (list face :family family)) list-of-lines)
        (push (propertize pangram 'face (list face :family family :slant 'italic)) list-of-lines)
        (push (propertize pangram 'face (list face :family family :weight 'bold)) list-of-lines)
        (push (propertize pangram 'face (list face :family family :slant 'italic :weight 'bold)) list-of-lines)
        (when propertize-sample-p
          (push (propertize character-sample 'face (list face :family family)) list-of-blocks)
          (push (propertize character-sample 'face (list face :family family :slant 'italic)) list-of-blocks)
          (push (propertize character-sample 'face (list face :family family :weight 'bold)) list-of-blocks)
          (push (propertize character-sample 'face (list face :family family :slant 'italic :weight 'bold)) list-of-blocks))
        (when show-font-sentences-sample
          (dolist (sentence show-font-sentences-sample)
            (when (show-font--string-p sentence)
              (push (propertize sentence 'face (list face :family family)) list-of-sentences)
              (push (propertize sentence 'face (list face :family family :slant 'italic)) list-of-sentences)
              (push (propertize sentence 'face (list face :family family :weight 'bold)) list-of-sentences)
              (push (propertize sentence 'face (list face :family family :slant 'italic :weight 'bold)) list-of-sentences)))))
      (concat
       (propertize name 'face (list 'show-font-title :family family))
       "\n"
       (make-separator-line)
       (if (not (equal name family))
           (concat
            "\n"
            (propertize "Rendered with parent family: " 'face (list 'show-font-regular :family family))
            (propertize family 'face (list 'show-font-regular :family family))
            "\n"
            (make-separator-line))
         "")
       "\n"
       (mapconcat #'identity (nreverse list-of-lines) "\n") "\n"
       (mapconcat #'identity (nreverse list-of-blocks) "\n") "\n" "\n"
       (mapconcat #'identity (nreverse list-of-sentences) "\n") "\n")))
   (t
    (concat
     (propertize (or family (show-font--get-attribute-from-file "fullname"))
                 'face 'show-font-title-in-listing)
     "\n"
     (make-separator-line)
     "\n"
     (propertize "No known preview for this font family" 'face 'error)))))

(defun show-font--install-file-button (_button)
  "Wrapper for `show-font-install' to work as a button."
  (show-font-install))

(define-button-type 'show-font-installed-file-button
  'follow-link nil
  'action #'show-font--install-file-button
  'face 'show-font-button)

(defun show-font--insert-button ()
  "Insert `show-font-installed-file-button' at point."
  (when buffer-file-name
    (insert
     (concat (propertize "Cannot preview this font" 'face 'show-font-title)
             "\n\n"
             (propertize buffer-file-name 'face 'bold)
             " is not installed"
             "\n\n"
             "Install this font file?"
             "\n"))
    (goto-char (point-max))
    (make-text-button (line-beginning-position 0) (line-end-position 0) :type 'show-font-installed-file-button)))

(defun show-font--add-text (&optional buffer)
  "Add the `show-font-pangram' as an overlay at `point-min'.
With optional BUFFER, operate therein.  Otherwise, do it in the current
buffer."
  (with-silent-modifications
    (with-current-buffer (or buffer (current-buffer))
      (let ((inhibit-read-only t))
        (save-excursion
          (if-let* ((text (show-font--prepare-text-subr nil)))
              (insert text)
            (show-font--insert-button)))))))

(defmacro show-font-with-preview-buffer (name &rest body)
  "Evaluate BODY inside NAME buffer."
  (declare (indent 1))
  `(let ((buffer (get-buffer-create ,name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         ,@body)
       (show-font-mode))
     (display-buffer buffer show-font-display-buffer-action-alist)))

;;;; Preview an installed font

(defvar show-font-select-preview-history nil
  "Minibuffer history for `show-font-select-preview'.")

(defun show-font--select-preview-prompt (&optional regexp)
  "Prompt for a font among `show-font-get-installed-font-families'.
Optional REGEXP has the same meaning as in the aforementioned function."
  (let ((def (car show-font-select-preview-history)))
    (completing-read
     (format-prompt "Select font to preview" def)
     (show-font-get-installed-font-families regexp)
     nil t nil 'show-font-select-preview-history def)))

;;;###autoload
(defun show-font-select-preview (family)
  "Prepare a preview for font FAMILY.
When called interactively, prompt for FAMILY.  When called from Lisp,
FAMILY is a string that satisfies `show-font-installed-p'."
  (interactive (list (show-font--select-preview-prompt)))
  (unless (display-graphic-p)
    (user-error "Can only preview fonts in graphical Emacs"))
  (if (show-font-installed-p family)
      (show-font-with-preview-buffer (format "*show-font preview of `%s'*" family)
        (save-excursion
          (insert (show-font--prepare-text-subr family)))
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                      (show-font-select-preview family))))
    (error "The font family `%s' does not seem to be installed" family)))

;;;; Preview fonts in a list

(defvar show-font-regexp-history nil
  "Minibuffer history for `show-font-regexp-prompt'.")

(defun show-font-regexp-prompt ()
  "Prompt for a string or regular expression."
  (let ((default (car show-font-regexp-history)))
    (read-string
     (format-prompt "Fonts matching REGEXP" default)
     nil 'show-font-regexp-history default)))

(defun show-font--list-family-preview (check-type family language generic-title-font)
  "Return generic `show-font--list-families' sample of FAMILY for LANGUAGE.
Use CHECK-TYPE as a symbol of `displays' or `prefers' to run the
corresponding checker function, like `show-font--displays-greek-p'
instead of `show-font--prefers-greek-p'.

If GENERIC-TITLE-FONT is non-nil, do not try to render the title in
FAMILY but use whatever the `default' face is."
  (unless (memq check-type '(prefers displays))
    (error "The CHECK-TYPE must be the symbol `prefers' or `displays'"))
  (let* ((check-fn (intern-soft (format "show-font--%s-%s-p" check-type language)))
         (language-sample (or (intern-soft (format "show-font-%s-sample" language))
                              (error "There is no known sample for `%s'" language)))
         (sample (symbol-value language-sample)))
    (when (funcall check-fn family)
      (vector
       (propertize family 'face (cond
                                 (generic-title-font
                                  'show-font-title-in-listing)
                                 ((show-font--displays-latin-p family)
                                  (list 'show-font-title-in-listing :family family))
                                 (t
                                  'show-font-title-in-listing)))
       (propertize sample 'face (list 'show-font-regular :family family))))))

(defun show-font--list-families (&optional regexp)
  "Return a list of propertized family strings for `show-font-list'.
Optional REGEXP has the meaning documented in the function
`show-font-get-installed-font-families'."
  (if-let* ((families (show-font-get-installed-font-families regexp)))
      (mapcar
       (lambda (family)
         (list
          family
          (cond
           ((show-font--list-family-preview 'prefers family "mathematics" :generic-title-font))
           ((show-font--list-family-preview 'prefers family "music" :generic-title-font))
           ((show-font--list-family-preview 'prefers family "symbols" :generic-title-font))
           ((show-font--list-family-preview 'displays family "emoji" :generic-title-font))
           ((show-font--list-family-preview 'displays family "icon" :generic-title-font))
           ((show-font--list-family-preview 'prefers family "arabic" nil))
           ((show-font--list-family-preview 'prefers family "chinese" nil))
           ((show-font--list-family-preview 'prefers family "greek" nil))
           ((show-font--list-family-preview 'prefers family "japanese" nil))
           ((show-font--list-family-preview 'prefers family "korean" nil))
           ((show-font--list-family-preview 'prefers family "russian" nil))
           ((show-font--displays-latin-p family)
            (vector
             (propertize family 'face (list 'show-font-title-in-listing :family family))
             (propertize (show-font--get-pangram) 'face (list 'show-font-regular :family family))))
           (t
            (vector
             (propertize family 'face 'show-font-title-in-listing)
             (propertize "No known preview" 'face 'error))))))
       families)
    (error "No font families found")))

(defvar show-font-tabulated-current-regexp nil
  "Regexp for `show-font-get-installed-font-families'.
Only `let' bind this while calling `show-font-tabulated-mode'.")

(defun show-font-tabulated-copy-name ()
  "Copy the name of the font family of the current line into the `kill-ring'."
  (interactive nil show-font-tabulated-mode)
  (unless (derived-mode-p 'show-font-tabulated-mode)
    (user-error "Can only do this inside the `show-font-tabulated-mode'"))
  (if-let* ((family (tabulated-list-get-id)))
      (prog1
          (kill-new family)
        (message "Copied the `%s' font family" (propertize family 'face 'bold)))
    (error "No font family on the current line")))

(defun show-font-tabulated-select-preview ()
  "Preview the font on the current line in `show-font-tabulated-mode'."
  (interactive nil show-font-tabulated-mode)
  (unless (derived-mode-p 'show-font-tabulated-mode)
    (user-error "Can only do this inside the `show-font-tabulated-mode'"))
  (if-let* ((family (tabulated-list-get-id)))
      (show-font-select-preview family)
    (error "No font family on the current line")))

(define-derived-mode show-font-tabulated-mode tabulated-list-mode "Show fonts"
  "Major mode to display font previews."
  :interactive nil
  (setq-local tabulated-list-format
              (vector (list (substitute-command-keys "Font family (\\<show-font-tabulated-mode-map>\\[show-font-tabulated-copy-name] to copy)") 60 t)
                      (list (substitute-command-keys "Sample text (\\<show-font-tabulated-mode-map>\\[show-font-tabulated-select-preview] for full preview)") 0 t)))
  (setq-local tabulated-list-entries
              (show-font--list-families show-font-tabulated-current-regexp))
  (tabulated-list-init-header)
  (tabulated-list-print))

(let ((map show-font-tabulated-mode-map))
  (define-key map (kbd "w") #'show-font-tabulated-copy-name)
  (define-key map (kbd "RET") #'show-font-tabulated-select-preview))

;;;###autoload
(defun show-font-tabulated (&optional regexp)
  "Produce a tabulated view of installed fonts with `show-font-pangram' preview.
With optional REGEXP as a prefix argument, prompt for a string or
regular expression to list only fonts matching the given input.
Otherwise, list all installed fonts."
  (interactive (list (when current-prefix-arg (show-font-regexp-prompt))))
  (unless (display-graphic-p)
    (user-error "Can only preview fonts in graphical Emacs"))
  (let ((buffer (get-buffer-create "*show-font-list*")))
    (with-current-buffer buffer
      (let ((show-font-tabulated-current-regexp regexp))
        (show-font-tabulated-mode)))
    (display-buffer buffer show-font-display-buffer-action-alist)))

(defalias 'show-font-list 'show-font-tabulated
  "Alias for `show-font-tabulated'.")

;;;; Major mode to preview the font of the current TTF or OTF file

;;;###autoload
(define-derived-mode show-font-mode special-mode "Show Font"
  "Major mode to preview a font file's character set."
  (set-buffer-multibyte t)
  (setq-local truncate-lines t
              buffer-undo-list t
              auto-save-default nil
              buffer-read-only t)
  (display-line-numbers-mode -1)
  (visual-line-mode -1))

;;;###autoload
(add-to-list 'file-name-handler-alist (cons "\\.\\(ttf\\|otf\\)\\'" #'show-font-handler))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.\\(ttf\\|otf\\)\\'" 'show-font-mode))

(provide 'show-font)
;;; show-font.el ends here
