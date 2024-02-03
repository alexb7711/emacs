;;; 2-mod-aethstetics.el --- Everything that makes Emacs look nice.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: faces

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

;; https://github.com/doomemacs/themes

;;; Code:

(require 'cl-lib nil t)
(require 'doom-themes nil t)
(if (display-graphic-p)
    (require 'all-the-icons nil t)
  (require 'all-the-icons-dired-mode nil t))

;;==============================================================================
;; Aesthetics

;;------------------------------------------------------------------------------
;; Theme
(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (lambda (frame)
       (with-selected-frame frame
         (load-theme 'wheatgrass t))))
  (load-theme 'wheatgrass t))

;;------------------------------------------------------------------------------
;; Modeline

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Modeline cleanup
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;;

(defvar mode-line-cleaner-alist
  `((eldoc-mode . "")
    (company-mode . "")
    (git-gutter-mode . "")
    (visual-line-mode . "")
    (which-key-mode . "")
    (olivetti-mode . "")
    (auto-fill-function . "")
    (flycheck-mode . "")
    (whitespace-mode . "")
    (flyspell-mode . ""))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  "Cleanup the modeline."
  (interactive)
  (cl-loop
   for cleaner in mode-line-cleaner-alist do
   (let* ((mode (car cleaner))
          (mode-str (cdr cleaner))
          (old-mode-str (cdr (assq mode minor-mode-alist))))
     (when old-mode-str
       (setcar old-mode-str mode-str))
     ;; major mode
     (when (eq mode major-mode)
       (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Display column number in modeline
(column-number-mode)
;; Time on modeline
(display-time-mode 1)
;; Make sure modeline VC branch updates automatically
(setq auto-revert-check-vc-info t)

;;------------------------------------------------------------------------------
;; Transparent Emacs
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))


;;==============================================================================
;; Font

;;------------------------------------------------------------------------------
;; Functions

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun mod/font-available-p (font-name)
  "Helper function to check if a FONT-NAME is installed."
  (find-font (font-spec :name font-name)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/load-fonts (&optional frame)
  "Function to load fonts in the current FRAME."
  (when frame
    (remove-hook 'after-make-frame-functions #'mod/load-fonts)
    (select-frame frame))

  ;; Font types
  (if (eq system-type 'windows-nt)
      (progn
        (when (mod/font-available-p "Mononoki Nerd Font")
          (set-face-attribute 'fixed-pitch nil :font "Mononoki Nerd Font" :height 110)
          (set-face-attribute 'default nil :font "Mononoki Nerd Font" :height 110))
        (when (mod/font-available-p "Iosevka Nerd Font")
          (set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :weight 'normal :height 130))
        (when (mod/font-available-p "Iosevka NF")
          (set-face-attribute 'variable-pitch nil :font "Iosevka NF" :weight 'normal :height 130)))
    (progn
      (when (mod/font-available-p "Mononoki Nerd Font")
        (set-face-attribute 'fixed-pitch nil :font "Mononoki Nerd Font" :height 130)
        (set-face-attribute 'default nil :font "Mononoki Nerd Font" :height 130))
      (when (mod/font-available-p "Iosevka Nerd Font")
        (set-face-attribute 'variable-pitch nil :font "Iosevka Nerd Font" :weight 'normal :height 135))
      (when (mod/font-available-p "Iosevka NF")
        (set-face-attribute 'variable-pitch nil :font "Iosevka NF" :weight 'normal :height 135))))

  ;; Font formatting
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

;;------------------------------------------------------------------------------
;; Hooks

;; Load font
(if (daemonp)
    (add-hook 'after-make-frame-functions #'mod/load-fonts)
  (funcall #'mod/load-fonts frame-initial-frame))

(provide '2-mod-aethstetics)
;;; 2-mod-aethstetics.el ends here
