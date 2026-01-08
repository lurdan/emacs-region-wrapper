;;; region-wrapper-test.el --- Tests for region-wrapper -*- lexical-binding: t; -*-
(require 'ert)
(require 'region-wrapper)

;;;; Test helpers

(defmacro region-wrapper-test-with-buffer (content &rest body)
  "Create a temp buffer with CONTENT and execute BODY.
Region is active from beginning to end of buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((region-wrapper-pairs
            '(("(" . ")")
              ("{" . "}")
              ("/*" . "*/"))))
       (insert ,content)
       (goto-char (point-min))
       (set-mark (point-max))
       (activate-mark)
       ,@body)))

(defmacro region-wrapper-test-without-region (content &rest body)
  "Create a temp buffer with CONTENT and execute BODY without region."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;;; Normal behavior

(ert-deftest region-wrapper-wrap-basic ()
  "Wrap region with a pair."
  (region-wrapper-test-with-buffer
      "foo"
    (region-wrapper-wrap "(")
    (should (equal (buffer-string) "(foo)"))))

(ert-deftest region-wrapper-unwrap-same-pair ()
  "Unwrap when wrapped with the same pair."
  (region-wrapper-test-with-buffer
      "(foo)"
    (region-wrapper-wrap "(")
    (should (equal (buffer-string) "foo"))))

(ert-deftest region-wrapper-rewrap-different-pair ()
  "Rewrap when wrapped with a different pair."
  (region-wrapper-test-with-buffer
      "(foo)"
    (region-wrapper-wrap "{")
    (should (equal (buffer-string) "{foo}"))))

(ert-deftest region-wrapper-force-wrap-with-prefix ()
  "With prefix argument, always wrap."
  (region-wrapper-test-with-buffer
      "(foo)"
    (region-wrapper-wrap "(" t)
    (should (equal (buffer-string) "((foo))"))))

(ert-deftest region-wrapper-multichar-pair ()
  "Support multi-character surround pairs."
  (region-wrapper-test-with-buffer
      "foo"
    (region-wrapper-wrap "/*")
    (should (equal (buffer-string) "/*foo*/"))))

;;;; Boundary conditions

(ert-deftest region-wrapper-empty-region ()
  "Signal error on empty region."
  (with-temp-buffer
    (insert "foo")
    (goto-char (point-min))
    (set-mark (point))
    (activate-mark)
    (should-error (region-wrapper-wrap "(") :type 'user-error)))

(ert-deftest region-wrapper-no-region ()
  "Signal error when no region is active."
  (region-wrapper-test-without-region
      "foo"
    (should-error (region-wrapper-wrap "(") :type 'user-error)))

(ert-deftest region-wrapper-undefined-pair ()
  "Signal error for undefined surround key."
  (region-wrapper-test-with-buffer
      "foo"
    (should-error (region-wrapper-wrap "<") :type 'user-error)))

;;;; Idempotency-like behavior

(ert-deftest region-wrapper-double-toggle ()
  "Wrapping twice with same key toggles state."
  (region-wrapper-test-with-buffer
      "foo"
    (region-wrapper-wrap "(")
    (goto-char (point-min))
    (set-mark (point-max))
    (activate-mark)
    (region-wrapper-wrap "(")
    (should (equal (buffer-string) "foo"))))

;;; region-wrapper-test.el ends here
