;;; mod-shell.el --- Configuration for all shell interfaces in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: terminals

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

(require 'advice nil t)
(require 'term nil t)
(require 'vc-git nil t)

;;==============================================================================
;; `Eshell'
;; Find more at: `https://protesilaos.com/emacs/dotemacs#h:103a8795-c29c-474f-9ddf-ecafaa2f6775'

;; Aliases
(defalias 'g 'git)
(defalias 'task 'go-task)

;; Defaults
(setenv "PAGER" "cat") ; Set the pager to `cat' to not break anything.
; Solves the issue with `git log' and `less'.

;;------------------------------------------------------------------------------
;; Prompt

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun with-face (str &rest face-plist)
  "Helper function to create an `eshell' prompt given STR and FACE-PLIST."
  (propertize str 'face face-plist))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun git-prompt-branch-name ()
  "Get current git branch name"
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file vc-git-program nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun shk-eshell-prompt ()
  "Create `eshell' prompt."
  (let ((branch-name (git-prompt-branch-name)))
    (concat
     (with-face (format-time-string "[%H:%M] " (current-time)) :foreground "gray")
     ":"
     (with-face (concat " " (eshell/pwd) " ") :foreground "spring green")
     (with-face
      (or (ignore-errors
            (format "(%s)" branch-name))
          ""))
     (with-face "\n")
     "> ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)

;;------------------------------------------------------------------------------
;; Complete history with a `ido' interface

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defvar mod/eshell--complete-history-prompt-history '()
  "History of `eshell'.")

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/eshell--complete-history-prompt ()
  "Prompt with completion for history element. Helper function for `mod/eshell-complete-history'."
  (if-let ((hist (ring-elements eshell-history-ring)))
    (completing-read "Input from history: " hist nil t nil 'mod/eshell--complete-history-prompt-history)
    (user-error "There is no Eshell history")))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/eshell-complete-history (elt)
  "Insert ELT from eshell history using completion."
  (interactive (list (mod/eshell--complete-history-prompt)))
  (insert elt))

;;------------------------------------------------------------------------------
;; Complete previously visited directories with a `ido' interface

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/eshell--cd (dir)
  "Routine to cd into DIR."
  (delete-region eshell-last-output-end (point-max))
  (when (> eshell-last-output-end (point))
    (goto-char eshell-last-output-end))
  (insert-and-inherit "cd " (eshell-quote-argument dir))
  (eshell-send-input))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/eshell-complete-recent-dir (dir &optional arg)
  "Switch to a recent Eshell directory.

When called interactively, DIR is selected with completion from the elements of
`eshell-last-dir-ring'.

With optional ARG pefix argument (\\[universal-argument]) also open the directory
in a `dired' buffer."
  (interactive (list
                (if-let ((dirs (ring-elements eshell-last-dir-ring)))
                  (completing-read "Switch to recent dir: " dirs nil t)
                  (user-error "There is no Eshell history for recent directories"))
                current-prefix-arg))
  (mod/eshell--cd dir)
  (when arg
    (dired-other-window dir)))

;;==============================================================================
;; `shell'
(if (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "C:/msys64/usr/bin/zsh.exe")
  (setq explicit-shell-file-name "zsh"))

;;==============================================================================
;; `term'
;; NOTE:
;; This does not currently work under Windows due to an Emacs bug.
;; https://stackoverflow.com/questions/20683604/terminal-emulator-not-spawning-process

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configuration
(setq
 term-show-maximum-output 1
 term-scroll-to-bottom-on-output 1)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Default `term' to use `zsh'
(defvar mod/term-shell
  (if (eq system-type 'windows-nt)
      "C:/msys64/usr/bin/zsh.exe"
    "/usr/bin/zsh"))

(defadvice term (before force-zsh)
  "Default to using `zsh' when using `term'."
  (interactive (list mod/term-shell)))

(ad-activate 'term)

(provide 'mod-shell)
;;; mod-shell.el ends here
