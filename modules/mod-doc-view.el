;;; mod-doc-view.el --- Configuration for document viewing  -*- lexical-binding: t; -*-

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

;; https://github.com/vedang/pdf-tools
;; https://github.com/nicolaisingh/saveplace-pdf-view

;;; Code:

(require 'saveplace-pdf-view)
(require 'pdf-tools)

;;==============================================================================
;; PDF

;; Load `pdf-tools' when required
(pdf-loader-install)

;; Add hook
(add-hook 'pdf-view-mode-hook #'save-place-local-mode)
(add-hook 'doc-view-mode-hook #'save-place-local-mode)
(add-hook 'doc-view-mode-hook #'pdf-tools-enable-minor-modes)

(provide 'mod-doc-view)

;;; mod-doc-view.el ends here
