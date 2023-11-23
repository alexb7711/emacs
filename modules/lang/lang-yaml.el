;;; ab-yaml.el --- Configure YAML editing            -*- lexical-binding: t; -*-

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

;; https://github.com/yoshiki/yaml-mode

;;; Code:
(require 'yaml-mode)

;;==============================================================================
;; Functions

;;==============================================================================
;; Configuration

;; Enable `tree-sitter' when available
(when (treesit-language-available-p 'yaml)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode)))

;;==============================================================================
;; Hooks

(provide 'ab-yaml)
;;; ab-yaml.el ends here
