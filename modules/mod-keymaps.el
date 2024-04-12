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
;; Key Maps

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/alignment-keymap
  :doc "Maps key shortcuts to alignment actions."
  "a" 'align
  "c" 'align-column
  "l" 'sort-lines
  "r" 'align-regexp
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/bookmark-keymap
  :doc
  "Maps key shortcuts to bookmark actions."
  "s" #'bookmark-save
  "S" #'bookmark-set
  "f" #'bookmark-jump
  "b" #'bookmark-bmenu-list)

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/buffer-keymap
  :doc
  "Maps shortcuts to buffer actions."
  "b" #'ibuffer
  "f" #'switch-to-buffer
  "k" #'kill-buffer)

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/dired-keymap
  :doc
  "Maps shortcuts to dired actions."
  :parent dired-mode-map
  "r" #'denote-dired-rename-files
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/code-keymap
  :doc
  "Maps shortcuts to code actions."
  "R" #'rgrep
  "," #'xref-go-back
  "." #'xref-find-definitions
  "?" #'xref-find-references
  "r" #'eglot-rename
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/comment-keymap
  :doc "Maps shortcuts for commenting text."
  "b" #'comment-box
  "c" #'mod/comment-dwim
  "i" #'comment-indent
  "l" #'comment-line
  "r" #'comment-region
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/find-keymap
  :doc
  "Maps shortcuts to find commands."
  "B" #'bookmark-jump
  "D" #'find-name-dired
  "R" #'rgrep
  "b" #'switch-to-buffer
  "d" #'dired
  "f" #'find-file
  "g" #'grep
  "p" #'project-switch-project
  "r" #'recentf-open
   )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/window-keymap
  :doc
  "Maps shortcuts to window commands."
  "h" #'windmove-right
  "l" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "H" #'tab-bar-history-back
  "L" #'tab-bar-history-forward)

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/note-keymap
  :doc
  "Maps shortcuts to note taking actions."
  "f" #'denote-open-or-create
  "n" #'denote
  "l" #'denote-link
  "r" #'denote-rename-file-and-buffer
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/tab-bar-keymap
  :doc
  "Maps shortcuts to note taking actions."
  "0" #'tab-close
  "1" #'tab-close-other
  "2" #'tab-new
  "D" #'dired-other-tab
  "F" #'find-file-other-tab
  "RET" #'tab-switch
  "d" #'tab-duplicate
  "m" #'tab-move
  "r" #'tab-rename
  "u" #'tab-undo)

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/version-control-keymap
  :doc
  "Maps shortcuts to version control actions."
  :parent vc-prefix-map
  "c" #'mod/clone)

;;==============================================================================
;; Prefix Maps

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/space-prefix-keymap
  :doc
  "Prefix keymap that is meant to be started with a `SPC'."
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right
  "=" mod/alignment-keymap
  "B" mod/bookmark-keymap
  "C" mod/comment-keymap
  "H" help-map
  "SPC" #'set-mark-command
  "b" mod/buffer-keymap
  "c" mod/code-keymap
  "f" mod/find-keymap
  "n" mod/note-keymap
  "p" project-prefix-map
  "t" mod/tab-bar-keymap
  "v" mod/version-control-keymap
  "w" mod/window-keymap
  "x" #'execute-extended-command
  )

;;------------------------------------------------------------------------------
;;
(defvar-keymap mod/dired-space-prefix-keymap
  :doc
  "Prefix keymap that is meant to be started with a `SPC'."
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right
  "=" mod/alignment-keymap
  "B" mod/bookmark-keymap
  "H" help-map
  "SPC" #'set-mark-command
  "b" mod/buffer-keymap
  "d" mod/dired-keymap
  "f" mod/find-keymap
  "n" mod/note-keymap
  "p" project-prefix-map
  "t" mod/tab-bar-keymap
  "v" mod/version-control-keymap
  "w" mod/window-keymap
  "x" #'execute-extended-command
  )

;;==============================================================================
;; `which-key'
(use-package
 which-key
 :ensure t
 :init (setq which-key-allow-imprecise-window-fit nil)
 :config
 (which-key-add-keymap-based-replacements mod/space-prefix-keymap
   "=" `("Alignment" . ,mod/alignment-keymap)
   "B" `("Bookmark" . ,mod/bookmark-keymap)
   "C" `("Comment" . ,mod/comment-keymap)
   "H" `("Help" . ,help-map)
   "b" `("Buffer" . ,mod/buffer-keymap)
   "c" `("Coding" . ,mod/code-keymap)
   "f" `("Find" . ,mod/find-keymap)
   "n" `("Notes" . ,mod/note-keymap)
   "p" `("Project" . ,project-prefix-map)
   "t" `("Tabs" . ,mod/tab-bar-keymap)
   "v" `("VC" . ,mod/version-control-keymap)
   "w" `("Window" . ,mod/window-keymap)
 (which-key-add-keymap-based-replacements mod/dired-space-prefix-keymap
   "=" `("Alignment" . ,mod/alignment-keymap)
   "B" `("Bookmark" . ,mod/bookmark-keymap)
   "H" `("Help" . ,help-map)
   "b" `("Buffer" . ,mod/buffer-keymap)
   "d" `("Dired" . ,mod/dired-keymap)
   "f" `("Find" . ,mod/find-keymap)
   "n" `("Notes" . ,mod/note-keymap)
   "p" `("Project" . ,project-prefix-map)
   "t" `("Tabs" . ,mod/tab-bar-keymap)
   "v" `("VC" . ,mod/version-control-keymap)
   "w" `("Window" . ,mod/window-keymap))
   )
 (which-key-mode 1))


(provide 'mod-keymaps)
;;; mod-keymaps.el ends here
