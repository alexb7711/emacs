;;; 3-modeline.el --- Configuration for modeline.    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;;==============================================================================
;; Modeline cleanup
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;;

(defvar mode-line-cleaner-alist
  `((eldoc-mode . "")
    (company-mode . "")
    (git-gutter-mode . "")
    (visual-line-mode . "")
    (which-key-mode . "")
    (olivetti-mode . "")
    (auto-fill-function . "")
    (whitespace-mode . "")
    (superword-mode . "")
    (flyspell-mode . ""))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  "Cleanup the modeline."
  (interactive)
  (cl-loop
   for cleaner in mode-line-cleaner-alist do
   (let* ((mode (car cleaner))
          (mode-str (cdr cleaner))
          (old-mode-str (cdr (assq mode minor-mode-alist))))
     (when old-mode-str
       (setcar old-mode-str mode-str))
     ;; major mode
     (when (eq mode major-mode)
       (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;==============================================================================
;; `modeline' Configuration


;;------------------------------------------------------------------------------
;; Set `modeline' Variables

(require 'which-func)
(defvar mod/mode-line-misc-info
  '(("[" eglot--mode-line-format "] ")
    (which-function-mode    ;Only display if mode is enabled.
    (which-func-mode       ;Only display if buffer supports it.
     ("" which-func-format " "))))
  "Custom miscellaneous information.")

(defvar mod/mode-line-modified
   '("<%1+> ")
   "Mode line construct for displaying whether the current buffer is modified.")

;;------------------------------------------------------------------------------
;; Set `modeline'
(setq-default mode-line-format
              `(
                ("%e"
                 ("  " viper-mode-string)
                 "| "
                 mod/mode-line-modified
                 mode-line-buffer-identification
                 " | "
                 mode-line-modes
                 "| "
                 ,mod/mode-line-misc-info
                 "|"
                 (vc-mode vc-mode)
                 )
                ))

;;==============================================================================
;; Miscellaneous cleanup

;; Display column number in modeline
(column-number-mode -1)
(line-number-mode -1)

;; Make sure modeline VC branch updates automatically
(setq auto-revert-check-vc-info t)


(provide '3-modeline)
;;; 3-modeline.el ends here
