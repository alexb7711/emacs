;;; mod-programming.el --- Configuration for programming modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords: tools

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

;;------------------------------------------------------------------------------
;; Buffer feedback

;;------------------------------------------------------------------------------
;; 80 character marker

;; Configuration

(setq
 display-fill-column-indicator-column t
 display-fill-column-indicator-character "U+2502")

;; Hooks

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode) ; Turn on marker

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Display tabs and trailing `whitespace'

;; Functions

(defun mod/load-whitespace (&optional frame)
  "Function to load `whitespace' parameters in FRAME when running daemon."

  (require 'whitespace nil t)

  (whitespace-mode 1)

  (mod/load-face-with-daemon 'whitespace-indentation nil
                             :inherit nil
                             :background 'unspecified
                             :foreground 'unspecified
                             :foreground "light gray"
                             :strike-through t))
;; Defaults

(setq-default whitespace-style '(face tabs indentation::tab trailing))

;; Hooks

(add-hook 'prog-mode-hook #'mod/load-whitespace)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Display which function you are under in the `modeline'

;; Hooks

(add-hook 'prog-mode-hook 'which-function-mode)

;;======================================================================================================================
;; Load programming language files
(mapc 'load (directory-files prog-dir t "^lang-.*\.el$"))

(provide 'mod-programming)
;;; mod-programming.el ends here
