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
         (load-theme 'misterioso t))))
  (load-theme 'misterioso t))

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

  ;; Ensure the function only runs once
  (when frame
    (remove-hook 'server-after-make-frame-hook #'mod/load-fonts))

  ;; Font types
  (if (eq system-type 'windows-nt)
      (progn
        (when (mod/font-available-p "Mononoki Nerd Font")
          (mod/load-face-with-daemon 'fixed-pitch nil :font "Mononoki Nerd Font" :height 110)
          (mod/load-face-with-daemon 'default nil :font "Mononoki Nerd Font" :height 110))
        (when (mod/font-available-p "Iosevka Nerd Font")
          (mod/load-face-with-daemon 'variable-pitch nil :font "Iosevka Nerd Font" :weight 'normal :height 110))
        (when (mod/font-available-p "Iosevka NF")
          (mod/load-face-with-daemon 'variable-pitch nil :font "Iosevka NF" :weight 'normal :height 110)))
    (progn
      (when (mod/font-available-p "Mononoki Nerd Font")
        (mod/load-face-with-daemon 'fixed-pitch nil :font "Mononoki Nerd Font" :height 130)
        (mod/load-face-with-daemon 'default nil :font "Mononoki Nerd Font" :height 130))
      (when (mod/font-available-p "Iosevka Nerd Font")
        (mod/load-face-with-daemon 'variable-pitch nil :font "Iosevka Nerd Font" :weight 'normal :height 130))
      (when (mod/font-available-p "Iosevka NF")
        (mod/load-face-with-daemon 'variable-pitch nil :font "Iosevka NF" :weight 'normal :height 130))))

  ;; Font formatting
  (mod/load-face-with-daemon 'font-lock-comment-face nil :slant 'italic)
  (mod/load-face-with-daemon 'font-lock-keyword-face nil :slant 'italic))

;;------------------------------------------------------------------------------
;; Hooks

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'mod/load-fonts)
  (funcall #'mod/load-fonts nil))

(provide '2-mod-aethstetics)
;;; 2-mod-aethstetics.el ends here
