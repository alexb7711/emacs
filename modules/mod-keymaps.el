;;; mod-keymaps.el --- Module that contains all the keymaps.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords: convenience

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
;;
(defvar-keymap mod/bookmark-keymap
  :doc "Maps key shortcuts to bookmark actions."
  "s" #'bookmark-save
  "S" #'bookmark-set
  "f" #'bookmark-jump
  "b" #'bookmark-bmenu-list
  )

;;==============================================================================
;;
(defvar-keymap mod/buffer-keymap
  :doc "Maps shortcuts to buffer actions."
  "b" #'ibuffer
  "f" #'switch-to-buffer
  )

;;==============================================================================
;;
(defvar-keymap mod/code-keymap
  :doc "Maps shortcuts to code actions."
  "R" #'rgrep
  "b" #'xref-go-back
  "d" #'xref-find-definitions
  "f" #'xref-find-references
  "g" #'grep
  "r" #'eglot-rename
  )

;;==============================================================================
;;
(defvar-keymap mod/find-keymap
  :doc "Maps shortcuts to find commands."
  "B" #'bookmark-jump
  "b" #'switch-buffer
  "f" #'find-file
  "p" #'project-switch-project
  )

;;==============================================================================
;;
(defvar-keymap mod/window-keymap
  :doc "Maps shortcuts to window commands."
  "h" #'windmove-right
  "l" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  )

;;==============================================================================
;;
(defvar-keymap mod/space-prefix-keymap
  :doc "Prefix keymap that is meant to be started with a `SPC'."
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right
  "B" mod/bookmark-keymap
  "SPC" #'set-mark-command
  "b" mod/buffer-keymap
  "c" mod/code-keymap
  "f" mod/find-keymap
  "h" help-map
  "p" project-prefix-map
  "v" vc-prefix-map
  )

(provide 'mod-keymaps)
;;; mod-keymaps.el ends here
