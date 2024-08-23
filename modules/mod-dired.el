;;; mod-dired.el --- My dired configuration.          -*- lexical-binding: t; -*-

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
(if (display-graphic-p)
    (require 'all-the-icons-dired nil t))

;;==============================================================================
;; Defaults
(setq-default dired-listing-switches "-alh")

;;==============================================================================
;; Configuration
(setq
 dired-dwim-target t                    ; Suggest other visible dired buffer
 dired-omit-files
 (rx (or (seq bol (? ".") "#")          ; Emacs autosave files
         (seq bol "." (not (any ".")))  ; dot-files
         (seq "~" eol)                  ; backup-files
         ))
 )

;; Make going to the previous directory also kill the current buffer.
(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key
    dired-mode-map (kbd "^")
    (lambda ()
      (interactive)
      (find-alternate-file "..")))))

;;==============================================================================
;; Hooks
(add-hook 'dired-load-hook (lambda () (require 'dired-x nil t))) ; Make sure `dired-x' is loade "" td
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ; Spare me the details `dired'
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode) ; Add icons to `dired'


(provide 'mod-dired)
;;; mod-dired.el ends here
