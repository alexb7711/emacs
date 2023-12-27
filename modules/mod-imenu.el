;;; mod-imenu.el --- `imenu' configuration           -*- lexical-binding: t; -*-

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

;; https://github.com/bmag/imenu-list/

;;; Code:

;;==============================================================================
;; `imenu-list'
(use-package
 imenu-list
 :ensure t
 :defer t

 :init
 (setq
  imenu-list-auto-resize nil ; Auto resize based on text
  imenu-list-focus-after-activation t ; Make active window after opening
  ))


(provide 'mod-imenu)
;;; mod-imenu.el ends here
