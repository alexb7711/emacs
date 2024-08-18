;;; mod-document-editing.el --- Configuration for editing (natural language) documents.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords: docs, tex, bib

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
;; General

;;------------------------------------------------------------------------------
;; Configuration

(setq sentence-end-double-space nil) ; Sentences end with a single space

;;------------------------------------------------------------------------------
;; Hooks

(add-hook 'text-mode-hook 'abbrev-mode) ; Enable `abbrev' in non-programming modes

;;==============================================================================
;; Automatic table detection
(when (not (eq (buffer-file-name) "*vc-log*"))

  ;;----------------------------------------------------------------------------
  ;; Configuration
  (setq prettify-symbols-unprettify-at-point 'right-edge) ; Make symols devolve to plain text to

  ;;----------------------------------------------------------------------------
  ;; Hooks

  ;; Make sybols look nice to read edit when at point
  (add-hook 'text-mode-hook 'prettify-symbols-mode))

;;==============================================================================
;; Unfill Paragraph
(defun mod/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;==============================================================================
;; `langtool'

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; `langtool' function
(defun mod/langtool ()
  "Function to reload `langtool' after each save."
  (if (eq system-type 'windows-nt)
      (setq langtool-java-bin "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
    (setq langtool-language-tool-jar "C:/msys64/usr/share/languagetool/languagetool-commandline.jar")
    (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")))

  ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;
  (defun mod/langtool-wrapper ()
    "Function wrapper for deleting trailing whitespace."
    (when (or (eq major-mode 'org-mode) (eq major-mode 'latex-mode) (eq major-mode 'markdown-mode))
      (langtool-check)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure

(use-package
 langtool
 :ensure t
 :defer t

 :hook
 (latex-mode . mod/langtool)
 (markdown-mode . mod/langtool)
 (org-mode . mod/langtool)
 (latex-mode . mod/langtool)
 (markdown-mode . mod/langtool)
 (org-mode . mod/langtool))

;;======================================================================================================================
;; Load natural language files
(mapc 'load (directory-files doc-dir t "^lang-.*\.el$"))

(provide 'mod-document-editing)
;;; mod-document-editing.el ends here
