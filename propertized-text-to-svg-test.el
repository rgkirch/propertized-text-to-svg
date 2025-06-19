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

;; --- Test Faces ---

(defface test-face-red
  '((t (:foreground "red")))
  "A test face with red foreground.")

(defface test-face-blue
  '((t (:foreground "blue")))
  "A test face with blue foreground.")

(defface test-face-dark-green
  '((t (:foreground "dark green")))
  "A test face with a space in the color name.")

(defface test-face-no-fg
  '((t (:background "yellow")))
  "A test face with no foreground color.")

(defface test-face-bold
  '((t (:weight bold)))
  "A bold test face.")

(defface test-face-italic-underline
  '((t (:slant italic :underline t)))
  "An italic and underlined test face.")

(defface test-face-orange-bold
  '((t :inherit test-face-bold :foreground "orange"))
  "Orange foreground that inherits bold.")


(defun ptts-test--get-default-fg-hex ()
  "Get the current theme's default foreground color as a 6-digit hex string."
  (let* ((color-name (face-attribute 'default :foreground))
         (rgb (color-name-to-rgb color-name)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))


(ert-deftest ptts-basic-propertized-string-test ()
  "Test a standard string with multiple faces."
  (let* ((p-string (concat (propertize "Hello" 'face 'test-face-orange-bold)
                           " World"
                           (propertize "!" 'face 'test-face-italic-underline)))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal tspans
                   `((tspan ((fill . "#ffa500") (font-weight . "bold")) "Hello")
                     (tspan ((fill . ,(ptts-test--get-default-fg-hex))) " World")
                     (tspan ((fill . ,(ptts-test--get-default-fg-hex)) (font-style . "italic") (text-decoration . "underline")) "!"))))))

(ert-deftest ptts-complex-inheritance-test ()
  "Test complex face properties like lists and anonymous faces."
  (let* ((p-string (concat
                    ;; A simple list of faces
                    (propertize "RedBold" 'face '(test-face-red test-face-bold))
                    ;; An anonymous face inheriting and overriding
                    (propertize "BlueBold" 'face '(:inherit test-face-blue :weight bold))))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal tspans
                   `((tspan ((fill . "#ff0000") (font-weight . "bold")) "RedBold")
                     (tspan ((fill . "#0000ff") (font-weight . "bold")) "BlueBold"))))))

(ert-deftest ptts-fully-unpropertized-string-test ()
  "Test a string that has no properties at all."
  (let* ((p-string "Just plain text")
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal tspans
                   `((tspan ((fill . ,(ptts-test--get-default-fg-hex))) "Just plain text"))))))

(ert-deftest ptts-face-with-no-foreground-test ()
  "Test that a face missing a :foreground attribute falls back to the default."
  (let* ((p-string (propertize "Test" 'face 'test-face-no-fg))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (cadddr svg-data))))
    (should (equal tspans
                   `((tspan ((fill . ,(ptts-test--get-default-fg-hex))) "Test"))))))

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
    (should (equal tspans
                   `((tspan ((fill . "#006400")) "Green Text"))))))


(provide 'propertized-text-to-svg-tests)

;; Local Variables:
;; read-symbol-shorthands: (("ptts-" . "propertized-text-to-svg-"))
;; End:
;;; propertized-text-to-svg-tests.el ends here
