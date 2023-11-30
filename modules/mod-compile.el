;;; mod-compile.el --- Configuration for the built-in compile features of Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
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


;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;; TODO: Make the default compile command be based on whether there is a
;; Makefile to Taskfile in the project.
(defun mod/compile-cmd-on-mode ()
  "Changes the default compile command based on the `major-mode'."
  (setq compile-command "make -k")

  (cond
   ((derived-mode-p 'rust-ts-mode)
    (setq compile-command "cargo build"))
   ((derived-mode-p 'rust-mode)
    (setq compile-command "cargo build"))
   ((derived-mode-p 'org-mode)
    (setq compile-command (concat compile-command " emacs")))
   ((derived-mode-p 'tex-mode)
    (setq compile-command compile-command))
   ((derived-mode-p 'emacs-lisp-mode)
    (setq compile-command compile-command))))

;;------------------------------------------------------------------------------
;;
(defun mod/hide-compilation-buffer (proc)
  "Hide the `*compilation*' buffer."
  (let ((window (get-buffer-window "*compilation*")))
    (ignore-errors
      (delete-window window))))

;;------------------------------------------------------------------------------
;;
(defun mod/disp-compile-on-error (buf str)
  "Display the `*compilation*' buffer when an error occurs."
  (when (string-match ".*exited abnormally.*" str)
    (display-buffer buf)))

;;------------------------------------------------------------------------------
;; Compile script
(defun compiler-script ()
  "Run compile command on currently opened buffer."
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))

;;==============================================================================
;; Hooks
(add-hook 'compilation-start-hook 'mod/hide-compilation-buffer)
(add-hook 'compilation-finish-functions #'mod/disp-compile-on-error)
(add-hook 'prog-mode-hook #'mod/compile-cmd-on-mode)
(add-hook 'text-mode-hook #'mod/compile-cmd-on-mode)

;;==============================================================================
;; Configuration
(setq compilation-scroll-output 1)

;;==============================================================================
;; Auto compile after saving
;; TODO: Have this hook run only after specific file types (markdown, latex, etc)
; (add-hook 'after-save-hook #'compiler-script)

(provide 'mod-compile)
;;; mod-compile.el ends here
