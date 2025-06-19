;;; propertized-text-to-svg.el --- Convert propertized text to an SVG image -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; Homepage: https://github.com/rgkirch/propertized-text-to-svg
;; Keywords: svg, faces, text, images, conversion

;; Package-Version: 0.7
;; Package-Requires: ((emacs "29.1"))

;; <<GPL-3.0>>

;;; Commentary:
;;
;; This package provides functions to convert an Emacs string with text
;; properties (i.e., faces) into an SVG image. The generated SVG
;; preserves the colors and styles (bold, italic, underline, etc.)
;; from the string's faces, making it ideal for creating images of
;; code snippets or for use in modelines and UIs where dynamic,
;; styled text-as-an-image is desired.
;;
;; It correctly handles anonymous faces and complex face properties by
;; recursively resolving face inheritance to determine the final, on-screen
;; appearance.
;;
;; The primary functions are `propertized-text-to-svg-data`, which
;; produces the raw SVG data structure, and `propertized-text-to-svg`,
;; which renders the final XML string.

;;; Code:

(require 'svg)
(require 'map)
(require 'subr-x)
(require 'color)
(require 'cl-lib)
(require 'resolve-face)

(defcustom propertized-text-to-svg-padding 10
  "The amount of padding in pixels around the text in the SVG."
  :type 'integer
  :group 'propertized-text-to-svg)

(defun propertized-text-to-svg--resolve-face (face)
  "Return alist of symbols for use with `let-alist' representing FACE attributes."
  (resolve-face-attributes-as-alist-of-symbols face nil 'default))

(defun propertized-text-to-svg--color-name-to-rgb-hex (color-name)
  "Convert COLOR-NAME to hex format compatible with svg."
  (apply #'color-rgb-to-hex (append (color-name-to-rgb color-name) '(2))))

(defun propertized-text-to-svg--get-face-attributes (face)
  "Return an alist of SVG presentation attributes for the given emacs FACE.

This function translates the resolved attributes of FACE into a list of
cons cells, where each cell is `(CSS-PROPERTY . VALUE)`, suitable for
styling an SVG `<text>` element.

It handles the following face attributes:
- :foreground, :background, :inverse-video
- :family, :height, :weight, :slant, :width
- :underline, :overline, :strike-through (including color and style)

It intentionally ignores attributes that don't map directly to
CSS properties for a `<text>` element, such as `:box` and
`:stipple`, as these would require generating separate SVG
elements like `<rect>` or `<pattern>`."
  (let-alist (propertized-text-to-svg--resolve-face face)
    (let ((attrs '()) ; A list to hold direct presentation attributes.
          (foreground (if .inverse-video .background .foreground)))

      ;; --- Direct Presentation Attributes ---

      ;; -- Color --
      (when foreground
        (push `("fill" . ,(propertized-text-to-svg--color-name-to-rgb-hex foreground)) attrs))

      ;; -- Font Family --
      (when (stringp .family)
        (push `("font-family" . ,.family) attrs))

      ;; -- Font Size --
      (cond
       ((integerp .height)
        (push `("font-size" . ,(format "%.1fpt" (/ (float .height) 10.0))) attrs))
       ((floatp .height)
        (push `("font-size" . ,(format "%.2fem" .height)) attrs)))

      ;; -- Font Weight --
      (let ((css-weight
             (pcase .weight
               ('thin "100")
               ('(or extra-light ultra-light) "200")
               ('light "300")
               ('(or semi-light demi-light) "300")
               ('(or normal medium regular book) "normal")
               ('(or semi-bold demi-bold) "600")
               ('bold "bold")
               ('(or ultra-bold extra-bold) "800")
               ('(or heavy black) "900")
               ('ultra-heavy "900")
               (_ nil))))
        (when css-weight
          (push `("font-weight" . ,(format "%s" css-weight)) attrs)))

      ;; -- Font Slant --
      (let ((css-slant (pcase .slant
                         ('italic "italic")
                         ('oblique "oblique")
                         ('normal "normal")
                         (_ nil))))
        (when css-slant
          (push `("font-style" . ,css-slant) attrs)))

      ;; -- Font Width (Stretch) --
      (let ((css-width (pcase .width
                         ('ultra-condensed "ultra-condensed")
                         ('extra-condensed "extra-condensed")
                         ('condensed "condensed")
                         ('semi-condensed "semi-condensed")
                         ('normal "normal")
                         ('semi-expanded "semi-expanded")
                         ('expanded "expanded")
                         ('extra-expanded "extra-expanded")
                         ('ultra-expanded "ultra-expanded")
                         (_ nil))))
        (when css-width
          (push `("font-stretch" . ,css-width) attrs)))

      ;; -- Text Decoration (Simplified for Compatibility) --
      (let ((decoration-lines '()))
        (when .strike-through (push "line-through" decoration-lines))
        (when .overline (push "overline" decoration-lines))
        (when .underline (push "underline" decoration-lines))
        (when decoration-lines
          (push `("text-decoration" . ,(string-join decoration-lines " ")) attrs)))

      (nreverse attrs))))


(defun propertized-text-to-svg--to-tspans (p-string)
  "Create a list of SVG `tspan' s-expressions from a P-STRING."
  (when (> (length p-string) 0)
    (cl-loop for (beg . end) being the intervals of p-string
             for segment-text = (substring-no-properties p-string beg end)
             for raw-face = (or (get-text-property beg 'face p-string) 'default)
             for svg-attrs = (propertized-text-to-svg--get-face-attributes raw-face)
             collect `(tspan ,svg-attrs ,segment-text))))

(defun propertized-text-to-svg--create-svg-element (body p-string)
  "Wrap a BODY s-expression in a top-level `svg' element using width of P-STRING."
  (let* ((width (+ (string-pixel-width p-string) propertized-text-to-svg-padding))
         (height (+ (window-font-height) (/ propertized-text-to-svg-padding 2)))
         (bg-name (face-attribute 'default :background nil 'default))
         (bg-hex (if (stringp bg-name)
                     (apply #'color-rgb-to-hex (append (color-name-to-rgb bg-name) '(2)))
                   "#ffffff")))
    `(svg ((width . ,width) (height . ,height) (version . "1.1")
           (xmlns . "http://www.w3.org/2000/svg"))
          (rect ((x "0") (y "0") (width "100%") (height "100%") (fill . ,bg-hex)))
          ,body)))

(defun propertized-text-to-svg-data (p-string)
  "Convert a propertized string P-STRING into an svg s-expression."
  (let* ((tspans (propertized-text-to-svg--to-tspans p-string))
         (font-family (or (face-attribute 'default :family nil 'default) "monospace"))
         (font-spec (face-attribute 'default :font nil 'default))
         (font-size (or (and font-spec (font-get font-spec :size)) 16))
         (text-element
          `(text
            ((font-family . ,font-family)
             (font-size . ,(format "%dpx" font-size))
             (xml:space . "preserve")
             (x . "50%")
             (y . "50%")
             (dominant-baseline . "middle")
             (text-anchor . "middle"))
            ,@tspans)))
    (propertized-text-to-svg--create-svg-element text-element p-string)))

(defun propertized-text-to-svg (p-string &optional buffer)
  "Convert P-STRING to svg. Print to BUFFER if non-nil or temp buffer otherwise."
  (if buffer
      (with-current-buffer buffer
        (svg-print (propertized-text-to-svg-data p-string)))
    (with-temp-buffer
      (svg-print (propertized-text-to-svg-data p-string))
      (buffer-string))))

(provide 'propertized-text-to-svg)

;;; propertized-text-to-svg.el ends here
