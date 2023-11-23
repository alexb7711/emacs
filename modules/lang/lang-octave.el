;;; lang-octave.el --- Configuration for Octave        -*- lexical-binding: t; -*-

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

;;------------------------------------------------------------------------------
;;
(defun octave/octave-configuration ()
  "Configure the octave coding environment."
  (toggle-truncate-lines 1))

;;==============================================================================
;; Configuration
(setq
 auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist)
 auto-mode-alist (cons '("\\.octaverc$" . octave-mode) auto-mode-alist))


;;==============================================================================
;; Hooks

(add-hook 'octave-mode-hook #'octave/octave-configuration)

(provide 'lang-octave)
;;; lang-octave.el ends here
