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
;; https://github.com/emacsorphanage/sr-speedbar

;;; Code:


(require 'olivetti nil t)
(require 'dired-sidebar nil t)
(require 'ibuffer-sidebar nil t)
(require 'ibuffer-vc nil t)

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
(if (eq system-type 'windows-nt)
    (setq-default
     olivetti-body-width 0.8
     olivetti-minimum-body-width 90
     olivetti-recall-visual-line-mode-entry-state t)
  (setq-default
   olivetti-body-width 120
   olivetti-minimum-body-width 90
   olivetti-recall-visual-line-mode-entry-state t))

;;------------------------------------------------------------------------------
;; Hooks

;; Center when there is one window
(add-hook 'text-mode-hook #'mod/olivetti)
(add-hook 'prog-mode-hook #'mod/olivetti)
(add-hook 'latex-mode-hook #'mod/olivetti)
(add-hook 'nov-mode-hook #'mod/olivetti)
(add-hook 'Info-mode-hook #'mod/olivetti)

;; Center all the time
(add-hook 'gnus-article-mode-hook #'olivetti-mode)
(add-hook 'gnus-summary-mode-hook #'olivetti-mode)
(add-hook 'gnus-topic-mode-hook #'olivetti-mode)
(add-hook 'vc-dir-mode-hook #'olivetti-mode)

;;==============================================================================
;; Dedicated Windows

;;------------------------------------------------------------------------------
;; Configuration

(setq switch-to-buffer-in-dedicated-window t)

;;==============================================================================
;; Misc. window placements
(setq display-buffer-alist
      '(("\\*e?shell\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
        ("\\*\\(Backtrace\\|Warnings|\\|Compile-Log|\\Messages\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 1))
        ("\\*grep\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . -1))
        ("\\*[Hh]elp\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . top) (slot . 1))
        ("\\*Completions\\*" (display-buffer-below-selected) (window-height . 0.2) (side . bottom) (slot . 0))
        ("\\*compilation\\*" (display-buffer-in-side-window) (window-height . 0.25) (side . right) (slot . 0))
        ("\\*Ilist\\*" (display-buffer-in-side-window) (window-width . 0.1) (side . right) (slot . 0))
        ("\\*Flycheck Errors\\*" (display-buffer-below-selected) (window-width . 0.2) (side . right) (slot . 1))))

;;==============================================================================
;; Sidebars

;;------------------------------------------------------------------------------
;; Functions

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/sidebar-toggle ()
  "Toggle both `dired-siderbar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/restart-ibuffer-sidebar-mode-after-refresh (&rest var)
  "Restart `ibuffer-sidebar-mode' before a refresh."
  (when (ibuffer-sidebar-showing-sidebar-p)
    (let ((buf (get-buffer ibuffer-sidebar-name)))
      (with-current-buffer buf
        (ibuffer-vc-set-filter-groups-by-vc-root)))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/ibuffer-sidebar-remove-column-headings (&rest _args)
  "Function ran after `ibuffer-update-title-and-summary' that removes headings.

F should be function `ibuffer-update-title-and-summary'.
ARGS are args for `ibuffer-update-title-and-summary'."
  (when (and (bound-and-true-p ibuffer-sidebar-mode) (not ibuffer-sidebar-display-column-titles))
    (with-current-buffer (current-buffer)
      (goto-char 1)
      (search-forward "-\n" nil t)
      (delete-region 1 (point)))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/ibuffer-vc-set-filter-groups-by-vc-root ()
  "Set the current filter groups to filter by vc root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
      (with-current-buffer ibuf
        (ibuffer-update nil t)))))

;;------------------------------------------------------------------------------
;; Configuration
(setq
 ;; `dired-sidebar'
 dired-sidebar-theme 'icons ; Display icons
 dired-sidebar-subtree-line-prefix "  |" ; Column separator
 dired-sidebar-use-term-integration t ; Display +/- when in term mode
 dired-sidebar-use-magit-integration nil ; Disable `magit' integration
 dired-sidebar-pop-to-sidebar-on-toggle-open nil ; Set `dired-sidebar' as active window
 dired-sidebar-no-delete-other-windows t

 ;; `ibuffer-sidebar'
 ibuffer-sidebar-pop-to-sidebar-on-toggle-open nil ; Set `ibuffer-sidebar' as active window
 ibuffer-sidebar-display-alist
 '((side . left) ; Set position/height of ...
   (slot . 1) ; `ibuffer-sidebar' window
   (window-height . 0.3)))

;;------------------------------------------------------------------------------
;; Advice
(advice-add 'ibuffer-sidebar-refresh-buffer :after #'mod/restart-ibuffer-sidebar-mode-after-refresh)
(advice-add 'buffer-sidebar-remove-column-headings :override #'mod/buffer-sidebar-remove-column-headings)
(advice-add 'ibuffer-vc-set-filter-groups-by-vc-root :override #'mod/ibuffer-vc-set-filter-groups-by-vc-root)

;;------------------------------------------------------------------------------
;; Hooks

(provide 'mod-window)
;;; mod-window.el ends here
