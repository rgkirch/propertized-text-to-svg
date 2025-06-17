;;; propertized-text-to-svg.el --- Convert propertized text to an SVG image -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; Homepage: https://github.com/rgkirch/propertized-text-to-svg
;; Keywords: svg, faces, text, images, conversion

;; Package-Version: 0.6
;; Package-Requires: ((emacs "29.1") svg color cl-lib)

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
;; The primary functions are `propertized-text-to-svg-data`, which
;; produces the raw SVG data structure, and `propertized-text-to-svg`,
;; which renders the final XML string.

;;; Code:

(require 'svg)
(require 'subr-x)
(require 'color)
(require 'cl-lib)

(defcustom propertized-text-to-svg-padding 10
  "The amount of padding in pixels around the text in the SVG."
  :type 'integer
  :group 'propertized-text-to-svg)

(defun propertized-text-to-svg--get-face-attributes (face)
  "Return an alist of SVG style attributes for the given Emacs FACE.
This function correctly determines the final, effective attributes
by respecting the full face inheritance chain."
  (let ((attrs '())
        (decorations '()))

    ;; Get the final, effective values for all attributes by resolving
    ;; inheritance all the way to the `default' face.
    (let* ((fg-name (face-attribute face :foreground nil 'default))
           (weight  (face-attribute face :weight nil 'default))
           (slant   (face-attribute face :slant nil 'default))
           (underline (face-attribute face :underline nil 'default))
           (overline (face-attribute face :overline nil 'default))
           (strike-through (face-attribute face :strike-through nil 'default)))

      ;; Foreground Color
      (let* ((svg-color (when (stringp fg-name)
                          (let ((rgb (color-name-to-rgb fg-name)))
                            (when rgb
                              ;; Force 6-digit hex format for consistency.
                              (apply #'color-rgb-to-hex (append rgb '(2))))))))
        (push `(fill . ,(or svg-color "#000000")) attrs))

      ;; Font Weight
      (when (memq weight '(bold semibold))
        (push '(font-weight . "bold") attrs))

      ;; Font Style
      (when (memq slant '(italic oblique))
        (push '(font-style . "italic") attrs))

      ;; Text Decoration
      (when underline (push "underline" decorations))
      (when overline (push "overline" decorations))
      (when strike-through (push "line-through" decorations))
      (when decorations
        (push `(text-decoration . ,(string-join decorations " ")) attrs)))
    (nreverse attrs)))


(defun propertized-text-to-svg--to-tspans (p-string)
  "Create a list of SVG `tspan` s-expressions from a propertized string."
  (when (> (length p-string) 0)
    (cl-loop for (beg . end) being the intervals of p-string
             for segment-text = (substring-no-properties p-string beg end)
             for face = (or (get-text-property beg 'face p-string) 'default)
             for svg-attrs = (propertized-text-to-svg--get-face-attributes face)
             collect `(tspan ,svg-attrs ,segment-text))))

(defun propertized-text-to-svg--create-svg-element (body p-string)
  "Wrap a BODY s-expression in a top-level `svg` element.
Calculates width and height from the P-STRING's pixel metrics
and sets the background color."
  (let* ((width (+ (string-pixel-width p-string) propertized-text-to-svg-padding))
         (height (+ (window-font-height) (/ propertized-text-to-svg-padding 2)))
         (bg-name (face-attribute 'default :background nil 'default))
         (bg-hex (if (stringp bg-name)
                     (apply #'color-rgb-to-hex (append (color-name-to-rgb bg-name) '(2)))
                   "#ffffff")))
    `(svg ((width . ,width) (height . ,height) (version . "1.1")
           (xmlns . "http://www.w3.org/2000/svg"))
          ;; Background rectangle
          (rect ((x "0") (y "0") (width "100%") (height "100%") (fill . ,bg-hex)))
          ,body)))

(defun propertized-text-to-svg-data (p-string)
  "Convert a propertized string P-STRING into an SVG s-expression.
This function composes helper functions to build the SVG data."
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
  "Generate an SVG XML string from a propertized string P-STRING.
This is a convenience wrapper that calls `propertized-text-to-svg-data`
to build the SVG structure and then uses `svg-print` to render
it as an XML string."
  (if buffer
      (with-current-buffer buffer
        (svg-print (propertized-text-to-svg-data p-string)))
    (with-temp-buffer
      (svg-print (propertized-text-to-svg-data p-string))
      (buffer-string))))

(provide 'propertized-text-to-svg)

;;; propertized-text-to-svg.el ends here
