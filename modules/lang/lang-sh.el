;;; lang-shell.el --- Shell scripting configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: languages

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
;; Functions

;;==============================================================================
;; Configuration

;;==============================================================================
;; Hooks

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(provide 'lang-shell)
;;; lang-shell.el ends here
