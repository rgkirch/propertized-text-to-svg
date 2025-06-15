;;; propertized-text-to-svg-tests.el --- Tests for propertized-string-to-svg -*- lexical-binding:t; -*-

;; Copyright (C) 2025 Richie Kirchofer

;; Author: Richie Kirchofer
;; Maintainer: Richie Kirchofer

;; <<GPL-3.0>>

;;; Code:

(require 'propertized-text-to-svg)
(require 'ert)

(defface test-face-red   '((t (:foreground "red")))    "A test face with red foreground.")
(defface test-face-blue  '((t (:foreground "blue")))   "A test face with blue foreground.")
(defface test-face-no-fg '((t (:background "yellow"))) "A test face with no foreground color.")

(ert-deftest ptts-basic-propertized-string-test ()
  "Test a standard string with two different faces."
  (let* ((p-string (concat (propertize "Hello" 'face 'test-face-red)
                           (propertize "World" 'face 'test-face-blue)))
         (svg-data (propertized-text-to-svg-data p-string))
         ;; Extract the list of tspans. The path is (cddr (caddr ...)) because:
         ;; - `svg-data` is `(svg (ATTRS...) (BODY...))`
         ;; - `(caddr svg-data)` gets the BODY, which is `(text (ATTRS...) (TSPANS...))`
         ;; - `(cddr ...)` gets the list of TSPANS.
         (tspans (cddr (caddr svg-data))))
    (should (equal tspans
                   '((tspan ((fill . "red")) "Hello")
                     (tspan ((fill . "blue")) "World"))))))

(ert-deftest ptts-string-with-unpropertized-segment-test ()
  "Test a string containing a segment with no face property."
  (let* ((p-string (concat (propertize "Red" 'face 'test-face-red)
                           "Default"
                           (propertize "Blue" 'face 'test-face-blue)))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (caddr svg-data)))
         (default-color (face-attribute 'default :foreground)))
    (should (equal tspans
                   `((tspan ((fill . "red")) "Red")
                     (tspan ((fill . ,default-color)) "Default")
                     (tspan ((fill . "blue")) "Blue"))))))

(ert-deftest ptts-fully-unpropertized-string-test ()
  "Test a string that has no properties at all."
  (let* ((p-string "Just plain text")
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (caddr svg-data)))
         (default-color (face-attribute 'default :foreground)))
    (should (equal tspans
                   `((tspan ((fill . ,default-color)) "Just plain text"))))))

(ert-deftest ptts-face-with-no-foreground-test ()
  "Test that a face missing a :foreground attribute falls back to the default."
  (let* ((p-string (propertize "Test" 'face 'test-face-no-fg))
         (svg-data (propertized-text-to-svg-data p-string))
         (tspans (cddr (caddr svg-data)))
         (default-color (face-attribute 'default :foreground)))
    (should (equal tspans
                   `((tspan ((fill . ,default-color)) "Test"))))))

(ert-deftest ptts-empty-string-test ()
  "Test that an empty string produces an empty list of tspans."
  (let* ((p-string "")
         (svg-data (propertized-text-to-svg-data p-string))
         ;; The body of the `text` element should be empty for an empty string.
         (tspans (cddr (caddr svg-data))))
    (should (null tspans))))

(provide 'propertized-text-to-svg-tests)

;; Local Variables:
;; read-symbol-shorthands: (("ptts-" . "propertized-text-to-svg-"))
;; End:
;;; propertized-text-to-svg-tests.el ends here
