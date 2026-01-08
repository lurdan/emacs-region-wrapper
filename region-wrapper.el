;;; region-wrapper.el --- Simple wrap / unwrap / rewrap for Emacs regions -*- lexical-binding: t; -*-

;; Author: KURASHIKI Satoru
;; Version: 0.1
;; Keywords: editing, convenience
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/lurdan/emacs-region-wrapper

;;; Commentary:

;; region-wrapper provides wrap / unwrap / rewrap for selected regions.
;;
;; Features:
;; - Wrap a region with a pair of characters or strings
;; - Unwrap if the same pair is already used
;; - Rewrap if a different pair is specified
;; - Supports multi-character pairs (e.g., "/*" … "*/", "begin" … "end")
;; - With a universal argument (`C-u`), always wrap

;;; Code:

(require 'seq)

(defgroup region-wrapper nil
  "Simple surround / rewrap utilities."
  :group 'editing)

(defcustom region-wrapper-pairs
  '(("'" . "'")
    ("\"" . "\"")
    ("`" . "`")
    ("(" . ")")
    ("( " . " )")
    ("[" . "]")
    ("[ " . " ]")
    ("{" . "}")
    ("{ " . " }")
    ("/*" . "*/")
    ("begin" . "end"))
  "Alist of surround pairs.
Key is input string, value is a cons (OPEN . CLOSE)."
  :type '(alist :key-type string :value-type (cons string string))
  :group 'region-wrapper)

(defun region-wrapper--wrap (beg end open close)
  "Insert OPEN at BEG and CLOSE at END, wrapping the region.
BEG and END are buffer positions delimiting the original region."
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)))

(defun region-wrapper--detect-pair (beg end)
  "If region itself is wrapped, return (OPEN . CLOSE), otherwise nil.
Wrapper must be at the beginning and end of the region."
  (seq-some
   (lambda (p)
     (let ((open (car p))
           (close (cdr p)))
       (when (and
              (>= (- end beg) (+ (length open) (length close)))
              (string= (buffer-substring-no-properties
                        beg (+ beg (length open)))
                       open)
              (string= (buffer-substring-no-properties
                        (- end (length close)) end)
                       close))
         p)))
   region-wrapper-pairs))

;;;###autoload
(defun region-wrapper-wrap (open &optional arg)
  "Wrap, unwrap, or rewrap the active region with OPEN.
If the region is already wrapped with the same pair, unwrap it.
If wrapped with a different pair, rewrap it.
With universal argument ARG, always wrap."
  (interactive
   (list (completing-read "Pair key: " (mapcar #'car region-wrapper-pairs) nil t)
         current-prefix-arg))
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((close (assoc-default open region-wrapper-pairs #'string=)))
    (unless close
      (user-error "Undefined pair key: %s" open))
    (let ((beg (region-beginning))
          (end (region-end)))
      (atomic-change-group
        (let ((wrapped (region-wrapper--detect-pair beg end)))
          (cond
           ((or arg (not wrapped))
            (region-wrapper--wrap beg end open close))
           (t ; when wrapped
            (let ((prev (car wrapped))
                  (next (cdr wrapped)))
              ;; unwrap
              (delete-region (- end (length next)) end)
              (delete-region beg (+ beg (length prev)))
              ;; rewrap
              (unless (and (string= prev open)
                           (string= next close))
                (setq end (- end (+ (length prev) (length next))))
                (region-wrapper--wrap beg end open close))))
           ))))))

;;;###autoload
(defun region-wrapper-sqm () (interactive) (region-wrapper-wrap "'"))

;;;###autoload
(defun region-wrapper-dqm () (interactive) (region-wrapper-wrap "\""))

;;;###autoload
(defun region-wrapper-paren () (interactive) (region-wrapper-wrap "("))

;;;###autoload
(defun region-wrapper-bracket () (interactive) (region-wrapper-wrap "["))

;;;###autoload
(defun region-wrapper-brace () (interactive) (region-wrapper-wrap "{"))

(provide 'region-wrapper)

;;; region-wrapper.el ends here
