;;; lang-rust.el --- Rust language configuration     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
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

;; https://github.com/rust-lang/rust-mode

;;; Code:

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;;
(defun rust/rust-configuration ()
  "Configuration for when the `rust-mode-hook' is enabled."
  (setq
   indent-tab-mode nil ; Ensure tabs instead of spaces
   comment-fill-column 80
   fill-column 80)

  (prettify-symbols-mode))

;;------------------------------------------------------------------------------
;;
(defun rust/format-on-save ()
  "Run `rustfm' before the file has been saved"
  (when (and rust-format-on-save (derived-mode-p 'rust-ts-mode))
    (rust-format-buffer)))

;;==============================================================================
;; Configuration

(require 'rust-mode)
(use-package rust-mode
 :ensure t
 :defer t
 :init
 (setq
  rust-format-on-save t ; Run `rustfmt' on save
  )

 :config
 ;; Enable `tree-sitter' when available
 (if (treesit-language-available-p 'rust)
   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
 (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

 (if (eq system-type 'windows-nt)
     (setq rust-rustfmt-bin "C:/msys64/mingw64/bin/rustfmt.exe")
   (setq rust-rustfmt-bin "/usr/bin/rustfmt"))

 ;; Set up `rustfmt'
 (dolist (item `(,(concat (getenv "HOME") "/.config/rustfmt/rustfmt.toml") "--config-path"))
   (add-to-list 'rust-rustfmt-switches item))

 :hook (before-save . rust/format-on-save))

(provide 'lang-rust)
;;; lang-rust.el ends here
