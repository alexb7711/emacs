;;; mod-denote.el --- Helper package for note taking, linking notes, and finding notes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
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

;;

;;; Code:

;; Variables
(defvar mod/note-directory (expand-file-name "~/Documents/blog/content"))

(use-package denote
  :init
  ;; General configuration
  (setq
   denote-directory mod/note-directory
   denote-prompts '(subdirectory title keywords))

  ;; Include `denote' as an option when using `org-capture'
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  :config
  (denote-rename-buffer-mode 1))

(provide 'mod-denote)
;;; mod-denote.el ends here
