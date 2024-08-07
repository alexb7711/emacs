;;; mod-tabs.el --- Configuration for tabs.          -*- lexical-binding: t; -*-

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

(defun mod/load-tab-bar ()
  "Load `tab-bar' in the specified FRAME."
  (set-face-attribute 'tab-bar nil :inherit t :height 90)
  (setq tab-bar-separator "|")
  (tab-bar-history-mode 1))

(use-package tab-bar
  :init
  (add-hook 'tab-bar-mode-hook #'mod/load-tab-bar)

  :config
  (tab-bar-mode 1))

(provide 'mod-tabs)
;;; mod-tabs.el ends here
