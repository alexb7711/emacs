;;; mod-treesitter.el --- Configuration to automatically install tresitter language grammars  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
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

(unless (or (version< emacs-version "29.0") mod/force-legacy)
  (progn
    (require 'treesit-auto nil t)
    (unless (equal (system-name) "KRSML-8TDXBV3") ; Ignore work laptop
      (use-package treesit-auto
        :custom
        (setq treesit-auto-langs '(awk bash bibtex c cmake cpp html latex make markdown org perl python rust yaml))
        (treesit-auto-install 'prompt)
        :config
        (treesit-auto-add-to-auto-mode-alist 'all)
        (global-treesit-auto-mode)))))

(provide 'mod-treesitter)
;;; mod-treesitter.el ends here
