;;; mod-window.el --- Configuration for how windows behave in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
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

;; https://github.com/rnkn/olivetti

;;; Code:


(require 'dired-sidebar nil t)

;;==============================================================================
;; Focus on the text

;;------------------------------------------------------------------------------
;; Functions

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/olivetti ()
  "Enable `Olivetti' for specific modes if it is the only window."
  (when (one-window-p)
    (olivetti-mode)))

;;------------------------------------------------------------------------------
;; Configuration
(use-package
 olivetti
 :ensure t

 :config
 (if (eq system-type 'windows-nt)
     (setq-default
      olivetti-style nil
      olivetti-body-width nil
      olivetti-minimum-body-width 120
      olivetti-recall-visual-line-mode-entry-state nil)
   (setq-default
    olivetti-style nil
    olivetti-body-width nil
    olivetti-minimum-body-width 124
    olivetti-recall-visual-line-mode-entry-state t))

 :init
 ;;-----------------------------------------------------------------------------
 ;; Advice
 (advice-add 'olivetti-reset-window :after #'(lambda (orig-func &rest var) (set-window-margins nil 1)))
)

;;------------------------------------------------------------------------------
;; Hooks (because use-package is breaking `olivetti' for some reason)
(add-hook 'text-mode-hook #'olivetti-mode)
(add-hook 'prog-mode-hook #'olivetti-mode)
(add-hook 'comint-mode-hook #'olivetti-mode)
(add-hook 'gnus-article-mode-hook #'olivetti-mode)
(add-hook 'gnus-summary-mode-hook #'olivetti-mode)
(add-hook 'gnus-topic-mode-hook #'olivetti-mode)
(add-hook 'nov-mode-hook #'olivetti-mode)
(add-hook 'vc-dir-mode-hook #'olivetti-mode)
(add-hook 'Info-mode-hook #'olivetti-mode)

;;==============================================================================
;; Dedicated Windows

;;------------------------------------------------------------------------------
;; Configuration

(setq switch-to-buffer-in-dedicated-window t)

;;==============================================================================
;; Misc. window placements
(setq display-buffer-alist
      '(("\\**-?e?shell\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
        ("\\*\\(Backtrace\\|Warnings|\\|Compile-Log|\\Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 1))
        ("\\*grep\\*" (display-buffer-in-side-window) (window-height . 0.15) (side . bottom) (slot . -1))
        ("\\*vc-log\\*\\(:?<.*>\\)?" (display-buffer-below-selected) (window-height . 0.25))
        ("\\*vc-git\\(:?.*\\)?\\*" (display-buffer-same-window))
        ("\\*[Hh]elp\\*" (display-buffer-in-side-window) (window-height . 0.3) (side . right) (slot . 1))
        ("\\*Completions\\*" (display-buffer-below-selected) (window-height . 0.2) (side . bottom) (slot . 0))
        ("\\*compilation\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . top) (slot . 0))
        ("\\*Ilist\\*" (display-buffer-in-side-window) (window-width . 0.1) (side . right) (slot . 0))
        ("\\*Flycheck Errors\\*" (display-buffer-below-selected) (window-width . 0.3) (side . right) (slot . 1))))

;;==============================================================================
;; Sidebars

;;------------------------------------------------------------------------------
;; Functions

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/sidebar-toggle ()
  "Toggle both `dired-siderbar'."
  (interactive)
  (dired-sidebar-toggle-sidebar))

;;------------------------------------------------------------------------------
;; Configuration

(use-package
 dired-sidebar
 :ensure t
 :defer t

 :init
 (setq
  dired-sidebar-theme 'icons ; Display icons
  dired-sidebar-subtree-line-prefix "  |" ; Column separator
  dired-sidebar-use-term-integration t ; Display +/- when in term mode
  dired-sidebar-use-magit-integration nil ; Disable `magit' integration
  dired-sidebar-pop-to-sidebar-on-toggle-open nil ; Set `dired-sidebar' as active window
  dired-sidebar-no-delete-other-windows t))

(provide 'mod-window)
;;; mod-window.el ends here
