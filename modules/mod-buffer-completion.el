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

;;==============================================================================
;; Defaults

;;==============================================================================
;; Symbol completion

;;------------------------------------------------------------------------------
;; `*Completions*'

(use-package
 completion
 :defer t

 :init
 (setq
  completion-auto-help 'visible
  completion-auto-select 'second-tab
  completions-max-height 4
  completions-header-format nil))

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

;;------------------------------------------------------------------------------
;; Configuration

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Automatically add spelling corrections into `abbrev' file
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;;------------------------------------------------------------------------------
;; Hooks
(add-hook 'text-mode #'mod/language-text-completion)
(add-hook 'org-mode #'mod/language-text-completion)

;;-----------------------------------------------------------------------------
;; LSP (Eglot)
;; Requirements:
;;    - C/C++: `ccls'
;;    - Python: `python-lsp-server'
;;    - Rust: `rust-analyzer'
(use-package
 eglot
 :ensure t
 :defer t

 :init (setq eglot-autoshutdown t)

 :config
 (if (eq system-type 'windows-nt)
     (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) "C:/msys64/usr/bin/rust-analyzer.exe")))

 :hook
 (c++-mode . eglot-ensure)
 (c++-ts-mode . eglot-ensure)
 (c-mode . eglot-ensure)
 (c-ts-mode . eglot-ensure)
 (python-mode . eglot-ensure)
 (python-ts-mode . eglot-ensure)
 (rust-mode . eglot-ensure)
 (rust-ts-mode . eglot-ensure))

(provide 'mod-buffer-completion)
;;; mod-buffer-completion.el ends here
