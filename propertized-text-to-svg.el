;;; propertized-text-to-svg.el --- Convert propertized text to an SVG image -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; Homepage: https://github.com/rgkirch/propertized-text-to-svg
;; Keywords: svg, faces, text, images, conversion

;; Package-Version: 0.1
;; Package-Requires: ((emacs "26.1") svg)

;; <<GPL-3.0>>

;;; Commentary:
;;
;; This package provides functions to convert an Emacs string with text
;; properties (i.e., faces) into an SVG image. The generated SVG
;; preserves the colors from the string's faces, making it ideal for
;; creating images of code snippets or for use in modelines and UIs
;; where dynamic, styled text-as-an-image is desired.
;;
;; The primary functions are `propertized-text-to-svg-data`, which
;; produces the raw SVG data structure, and `propertized-text-to-svg`,
;; which renders the final XML string.

;;; Code:

(require 'svg)
(require 'subr-x)

(defun propertized-text-to-svg--to-tspans (p-string)
  "Create a list of SVG `tspan` s-expressions from a propertized string.
This is the core conversion logic."
  (let ((tspans nil)
        (pos 0)
        (end (length p-string)))
    ;; Only process non-empty strings.
    (when (> end 0)
      (while (and pos (< pos end))
        (let* ((segment-end (next-property-change pos p-string))
               (segment-text (substring-no-properties p-string pos segment-end))
               (face (get-text-property pos 'face p-string))
               ;; Try to get foreground from the specific face.
               (face-color (and face (face-attribute face :foreground nil t)))
               ;; If face is nil, or if the attribute is 'unspecified,
               ;; fall back to the default foreground color.
               (color (if (or (not face-color) (eq face-color 'unspecified))
                          (face-attribute 'default :foreground nil t)
                        face-color)))
          (push `(tspan ((fill . ,color)) ,segment-text) tspans)
          (setq pos segment-end))))
    (nreverse tspans)))

(defun propertized-text-to-svg--with-text-element (tspans)
  "Wrap a list of TSPANs in an SVG `text` element s-expression."
  (let ((font-size 16))
    `(text
      ((font-family . "monospace")
       (font-size . ,(format "%dpx" font-size))
       ;; Add xml:space to preserve whitespace at the start/end of tspans.
       (xml:space . "preserve")
       (x . "50%")
       (y . "50%")
       (dominant-baseline . "middle")
       (text-anchor . "middle"))
      ,@tspans)))

(defun propertized-text-to-svg--with-svg-element (body p-string-length)
  "Wrap a BODY s-expression in a top-level `svg` element.
Calculates width and height from the P-STRING-LENGTH."
  (let* ((font-size 16)
         ;; Approximate width based on font size. This is a heuristic.
         (width (* p-string-length (/ font-size 1.6)))
         (height (+ font-size 4)))
    `(svg
      ((width . ,width) (height . ,height) (version . "1.1")
       (xmlns . "http://www.w3.org/2000/svg"))
      ,body)))

(defun propertized-text-to-svg-data (p-string)
  "Convert a propertized string P-STRING into an SVG s-expression.
This function composes helper functions to build the SVG data."
  (thread-first p-string
                (propertized-text-to-svg--to-tspans)
                (propertized-text-to-svg--with-text-element)
                (propertized-text-to-svg--with-svg-element (length p-string))))

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

(when nil
  (let ((my-propertized-string
         (concat (propertize "-115" 'face 'magit-diff-removed-highlight)
                 (propertize "+250" 'face 'magit-diff-added-highlight))))

    (let ((svg-data (propertized-text-to-svg-data my-propertized-string)))
      (message "SVG Data:\n%S" svg-data))

    (let ((svg-xml (propertized-text-to-svg my-propertized-string)))
      (message "SVG XML String:\n%s" svg-xml))))

(provide 'propertized-text-to-svg)

;;; propertized-text-to-svg.el ends here
