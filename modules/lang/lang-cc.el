;;; lang-cc.el --- C/C++ configuration                 -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'cc-mode nil t)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;;
(defun c/config-c-c++-modes ()
  "Configurations shared by C/C++."
  (c-toggle-auto-newline 1) ; Automatically create new lines
  (setq
   c-default-style "bsd" ; Set C-style
   c-tab-always-indent nil ; Indent line if at the beginning,
   ; otherwise add space
   c-basic-offset 2)) ; Set tab width to 2 spaces

;;==============================================================================
;; Configuration
(when (treesit-language-available-p 'c++)
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode)))
(when (treesit-language-available-p 'c)
  (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode)))

;;==============================================================================
;; Hooks

(add-hook 'c-mode-hook #'c/config-c-c++-modes)
(add-hook 'c++-mode-hook #'c/config-c-c++-modes)

(provide 'lang-cc)
;;; lang-cc.el ends here
