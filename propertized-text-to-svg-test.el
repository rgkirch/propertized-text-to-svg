;;; propertized-text-to-svg-tests.el --- Tests for propertized-string-to-svg -*- lexical-binding:t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; <<GPL-3.0>>

;;; Code:

(require 'propertized-text-to-svg)
(require 'ert)
(require 'color)
(require 'cl-lib)

(defface test-face-red   '((t (:foreground "red")))
  "A test face with red foreground.")
(defface test-face-blue  '((t (:foreground "blue")))
  "Blue.")
(defface test-face-light-green '((t (:foreground "light green"
                                     :slant italic)))
  "Light green and italic.")
(defface test-face-no-fg '((t (:background "yellow")))
  "A test face with no foreground color.")
(defface test-face-orange '((t (:foreground "orange"
                                :background "purple")))
  "Orange foreground and purple background.")

(ert-deftest ptts-basic-propertized-string-test ()
  "Test a standard string with two different faces."
  (let* ((p-string (concat (propertize "Hello" 'face 'test-face-orange)
                           "World"
                           (propertize "!" 'face 'test-face-light-green)))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal tspans
                   '((tspan
                      ((fill . "#ffa500") (text-decoration . "line-through overline underline"))
                      "Hello")
                     (tspan ((fill . "#bbc2cf")) "World")
                     (tspan
                      ((fill . "#90ee90") (font-style . "italic")
                       (text-decoration . "line-through overline underline"))
                      "!"))))))

(ert-deftest ptts-string-with-unpropertized-segment-test ()
  "Test a string containing a segment with no face property."
  (let* ((p-string (concat (propertize "Red" 'face 'test-face-red)
                           "Default"
                           (propertize "Blue" 'face 'test-face-blue)))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data)))
         (default-color-name (face-attribute 'default :foreground))
         (default-color-hex (apply #'color-rgb-to-hex (append (color-name-to-rgb default-color-name) '(2)))))
    (should (equal (length tspans) 3))
    (should (equal (cdr (assoc 'fill (cadr (nth 1 tspans)))) default-color-hex))))

(ert-deftest ptts-fully-unpropertized-string-test ()
  "Test a string that has no properties at all."
  (let* ((p-string "Just plain text")
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data)))
         (default-color-name (face-attribute 'default :foreground))
         (default-color-hex (apply #'color-rgb-to-hex (append (color-name-to-rgb default-color-name) '(2)))))
    (should (equal (length tspans) 1))
    (should (equal (cdr (assoc 'fill (cadr (car tspans)))) default-color-hex))))

(ert-deftest ptts-face-with-no-foreground-test ()
  "Test that a face missing a :foreground attribute falls back to the default."
  (let* ((p-string (propertize "Test" 'face 'test-face-no-fg))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data)))
         (default-color-name (face-attribute 'default :foreground))
         (default-color-hex (apply #'color-rgb-to-hex (append (color-name-to-rgb default-color-name) '(2)))))
    (should (equal (length tspans) 1))
    (should (equal (cdr (assoc 'fill (cadr (car tspans)))) default-color-hex))))

(ert-deftest ptts-empty-string-test ()
  "Test that an empty string produces an empty list of tspans."
  (let* ((p-string "")
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (null tspans))))

(ert-deftest ptts-multi-word-color-name-test ()
  "Test that a color name with spaces is correctly converted to hex."
  (let* ((p-string (propertize "Green Text" 'face 'test-face-dark-green))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal (length tspans) 1))
    (should (equal (cdr (assoc 'fill (cadr (car tspans)))) "#006400"))))

(provide 'propertized-text-to-svg-tests)

;; Local Variables:
;; read-symbol-shorthands: (("ptts-" . "propertized-text-to-svg-"))
;; End:
;;; propertized-text-to-svg-tests.el ends here
