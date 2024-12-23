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

(require 'compile)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;;
(defun mod/compile-cmd-on-mode ()
  "Change the default compile command based on the `major-mode'."
  (setq compile-command "make -k")

  (cond
   ((or (derived-mode-p 'rust-ts-mode) (derived-mode-p 'rust-mode))
    (setq compile-command "cargo build"))
   ((derived-mode-p 'org-mode)
    (setq compile-command (concat compile-command " emacs")))
   ((or (derived-mode-p 'tex-mode) (derived-mode-p 'emacs-lisp-mode))
    (setq compile-command compile-command))))

;;------------------------------------------------------------------------------
;;
(defun mod/hide-compilation-buffer (proc)
  "Hide the `*compilation*' PROC buffer."
  (ignore proc)
  (let ((window (get-buffer-window "*compilation*")))
    (ignore-errors
      (delete-window window))))

;;------------------------------------------------------------------------------
;;
(defun mod/disp-compile-on-error (buf str)
  "Display the `*compilation*' for BUF when an error is found in STR."
  (when (string-match ".*exited abnormally.*" str)
    (let* ((comp-buf (if (processp buf)
                         (process-buffer buf)
                       buf))
           (window (get-buffer-window comp-buf)))
      (if window
          (select-window window)
        (switch-to-buffer-other-window comp-buf)))))

;;------------------------------------------------------------------------------
;;
(defun mod/compile-from-root-dir ()
  "Run compile function from the project root directory (if it exists)."
  (interactive)
  (let ((default-directory
         (if (vc-root-dir)
             (vc-root-dir)
           default-directory)))
    (call-interactively #'compile)))

;;------------------------------------------------------------------------------
;;
(defun mod/recompile-from-root-dir ()
  "Recompile function from the project root directory (if it exists)."
  (interactive)
  (let ((default-directory
         (if (vc-root-dir)
             (vc-root-dir)
           default-directory)))
    (call-interactively #'recompile)))

;;==============================================================================
;; Hooks
(add-hook 'compilation-start-hook 'mod/hide-compilation-buffer) ; TODO: TEST
(add-hook 'compilation-finish-functions #'mod/disp-compile-on-error)
(add-hook 'prog-mode-hook #'mod/compile-cmd-on-mode)
(add-hook 'text-mode-hook #'mod/compile-cmd-on-mode)

;;==============================================================================
;; Configuration
(setq compilation-scroll-output 1)

(provide 'mod-compile)
;;; mod-compile.el ends here
