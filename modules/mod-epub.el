;;; mod-epub.el --- Read EPUB directly in Emacs!      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: docs

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

;; https://depp.brause.cc/nov.el/

;;; Code:

(require 'nov nil t)

;;==============================================================================
;; Enable `nov'
(with-eval-after-load 'org
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;==============================================================================
;; Configuration
(setq-default nov-text-width 100)

;;------------------------------------------------------------------------------
;; Font
(add-hook
 'nov-mode-hook
 #'(lambda ()
     (if (eq system-type 'windows-nt)
         (face-remap-add-relative 'variable-pitch :family "Iosevka NF" :weight 'normal :height 150)
       (face-remap-add-relative 'variable-pitch :family "Iosevka Nerd Font" :weight 'normal :height 150))))

(provide 'mod-epub)
;;; mod-epub.el ends here
