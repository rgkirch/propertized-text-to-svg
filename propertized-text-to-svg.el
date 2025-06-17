;;; propertized-text-to-svg.el --- Convert propertized text to an SVG image -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; Homepage: https://github.com/rgkirch/propertized-text-to-svg
;; Keywords: svg, faces, text, images, conversion

;; Package-Version: 0.7
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
;; It correctly handles anonymous faces and complex face properties by
;; recursively resolving face inheritance to determine the final, on-screen
;; appearance.
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

(defun propertized-text-to-svg--resolve-face (face)
  "Return a resolved plist of attributes for FACE.
FACE can be a named face symbol (e.g., 'font-lock-warning-face),
a list of faces (e.g., '(face-a face-b)), or an anonymous face
plist (e.g., '(:inherit error :weight bold)).  Correctly
handles single and multiple inheritance."
  (cond
   ;; Case 1: FACE is a named symbol.
   ((symbolp face)
    (cl-loop for (attr) in custom-face-attributes
             for value = (face-attribute face attr nil 'default)
             when (and (not (eq attr :inherit))
                       value
                       (not (eq value 'unspecified)))
             collect attr and collect value))

   ;; Case 2: FACE is a list.
   ((consp face)
    (if (keywordp (car-safe face))
        ;; It's an anonymous face plist, e.g., '(:inherit foo :weight bold)
        (let* ((parents (plist-get face :inherit))
               (parent-attrs
                (cond
                 ;; No inheritance
                 ((null parents) '())
                 ;; Single inheritance
                 ((symbolp parents) (propertized-text-to-svg--resolve-face parents))
                 ;; Multiple inheritance (a list of faces)
                 ((consp parents)
                  (let ((merged-attrs '()))
                    ;; Iterate through parents. Attributes from later faces
                    ;; in the list take precedence.
                    (dolist (p parents)
                      (setq merged-attrs (append (propertized-text-to-svg--resolve-face p) merged-attrs)))
                    merged-attrs))))
               ;; Get the anonymous face's own specific attributes
               (own-attrs
                (cl-loop for (prop val) on face by #'cddr
                         unless (eq prop :inherit)
                         collect prop and collect val)))
          ;; Merge, with own-attrs taking highest precedence.
          (append own-attrs parent-attrs))
      ;; It's a list of faces, e.g., '(face-a face-b)
      (let ((merged-attrs '()))
        ;; Iterate through faces. Attributes from later faces
        ;; in the list take precedence.
        (dolist (p face)
          (setq merged-attrs (append (propertized-text-to-svg--resolve-face p) merged-attrs)))
        merged-attrs)))

   ;; Case 3: Invalid input.
   (t
    (error "Invalid face specifier: %S" face))))

(defun propertized-text-to-svg--get-face-attributes (face)
  "Return an alist of SVG style attributes for the given Emacs FACE."
  (let ((attrs '())
        (decorations '())
        (resolved-attrs (propertized-text-to-svg--resolve-face face)))

    (let* ((fg-name (plist-get resolved-attrs :foreground))
           (weight  (plist-get resolved-attrs :weight))
           (slant   (plist-get resolved-attrs :slant))
           (underline (plist-get resolved-attrs :underline))
           (overline (plist-get resolved-attrs :overline))
           (strike-through (plist-get resolved-attrs :strike-through)))

      ;; Foreground Color
      (let* ((svg-color (when (stringp fg-name)
                          (let ((rgb (color-name-to-rgb fg-name)))
                            (when rgb
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
             for raw-face = (or (get-text-property beg 'face p-string) 'default)
             for svg-attrs = (propertized-text-to-svg--get-face-attributes raw-face)
             collect `(tspan ,svg-attrs ,segment-text))))

(defun propertized-text-to-svg--create-svg-element (body p-string)
  "Wrap a BODY s-expression in a top-level `svg` element."
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
  "Convert a propertized string P-STRING into an SVG s-expression."
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
  "Generate an SVG XML string from a propertized string P-STRING."
  (if buffer
      (with-current-buffer buffer
        (svg-print (propertized-text-to-svg-data p-string)))
    (with-temp-buffer
      (svg-print (propertized-text-to-svg-data p-string))
      (buffer-string))))

(provide 'propertized-text-to-svg)

;;; propertized-text-to-svg.el ends here
