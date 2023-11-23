;;; mod-buffer-completions.el --- Configure the buffer completion environment.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: faces

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

;; https://company-mode.github.io/
;; https://emacs-lsp.github.io/lsp-mode/

;;; Code:

(require 'company nil t)
(require 'eglot nil t)

;;==============================================================================
;; Defaults

;;==============================================================================
;; Symbol completion

;;------------------------------------------------------------------------------
;; `company'

;; Add hooks
;; (add-hook 'after-init-hook 'global-company-mode)
;;
;; ;; Configuration
;; (setq
;;  company-tooltip-align-annotations t                                          ; Align annotations to the right
;;  company-tooltip-offset-display    'lines                                     ; Display output with line count instead
;;                                                                                  ; of scroll bar
;;  company-tooltip-flip-when-above   t                                          ; Invert tooltip when displayed above
;;  company-format-margin-function    'company-text-icons-margin                 ; Set margins to have a character
;;  company-text-icons-add-background t                                          ; Add a shaded background to margin
;;  company-backends                  '((company-capf))                          ; Use compbletion at point functions
;;  company-transformers '(delete-consecutive-dups                               ; Post processing on candidates
;;                         company-sort-by-occurrence))

;;------------------------------------------------------------------------------
;; `*Completions*'

;; Configuration
(setq
 completion-auto-help 'visible
 completion-auto-select 'second-tab
 completions-max-height 4
 completions-header-format nil)

;;------------------------------------------------------------------------------
;; Add file completion at point
(autoload 'ffap-file-at-point "ffap")
(defun mod/complete-path-at-point ()
  "Return completion data for UNIX path at point."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap)) (save-excursion (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0) (match-end 0) #'completion-file-name-table :exclusive 'no))))

;;------------------------------------------------------------------------------
;; `abbrev'

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Automatically add spelling corrections into `abbrev' file
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Add` dabbrev'
(defun mod/dabbrev-completion-at-point ()
  (dabbrev--reset-global-variables)
  (let* ((abbrev (dabbrev--abbrev-at-point))
         (candidates (dabbrev--find-all-expansions abbrev t))
         (bnd (bounds-of-thing-at-point 'symbol)))
    (list (car bnd) (cdr bnd) candidates)))

;;-----------------------------------------------------------------------------
;; Add completions to `completions-at-point-functions'
(add-hook 'text-mode #'mod/language-text-completion)
(add-hook 'org-mode #'mod/language-text-completion)

(defun mod/language-text-completion ()
  "Adds the completion functions when editing natural lanugage files."
  (add-to-list 'completion-at-point-functions '(mod/complete-path-at-point mod/dabbrev-completion-at-point)))

;;-----------------------------------------------------------------------------
;; LSP (Eglot)
;; Requirements:
;;    - C/C++: `ccls'
;;    - Python: `python-lsp-server'
;;    - Rust: `rust-analyzer'

;; Add hooks

;; replace XXX-mode with concrete major-mode(e. g. python-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)
(add-hook 'c-ts-mode-hook #'eglot-ensure)
;; (add-hook 'lsp-mode-hook    #'lsp-enable-which-key-integration)                  ; `which-key' integration

;; Configuration

(setq eglot-autoshutdown t)

(if (eq system-type 'windows-nt)
    (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) "C:/msys64/usr/bin/rust-analyzer.exe")))

;; (setq
;;  lsp-headerline-breadcrumb-enable   nil                                          ; Disable breadcrumbs
;;  lsp-modeline-diagnostics-enable t                                               ; Show diagnostics in modeline
;;  lsp-keymap-prefix "C-c l"                                                       ; Set prefix for lsp-command-keymap
;;  lsp-enable-snippet 0                                                            ; Disable snippets
;;  lsp-rust-server 'rust-analyzer)                                                 ;

(require 'lsp-mode nil t) ; This needs to stay here

(provide 'mod-buffer-completion)
;;; mod-buffer-completion.el ends here
