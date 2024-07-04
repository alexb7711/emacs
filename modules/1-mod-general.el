;;; 1-mod-general.el --- General configuration for Emacs  -*- lexical-binding: t; -*-

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

;; https://github.com/justbur/emacs-which-key

;;; Code:


(require 'desktop nil t)
(require 'tooltip nil t)
(require 'recentf nil t)

;;==============================================================================
;; Cleanup Emacs
(tooltip-mode -1)                       ; Turn off GUI tooltips
(menu-bar-mode -1)                      ; Disable tool bar
(tool-bar-mode -1)                      ; Disable tool bar
(scroll-bar-mode -1)                    ; Disable scroll bar
(global-auto-revert-mode t)             ; Auto reload files

(setq
 inhibit-startup-screen t               ; Disable splash screen
 frame-resize-pixelwise t               ; Resize frame by pixels
 window-resize-pixelwise t)             ; Resize window by pixels

;;==============================================================================
;; `*scratch*'
(setq
 initial-scratch-message nil            ; Disable scratch buffer text
 initial-major-mode 'org-mode)          ; Enable `Org' mode in scratch buffer


;;==============================================================================
;; Native compilation
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t))

;;==============================================================================
;; Defaults

;; Auto reload `dired' buffers
(if (not (eq system-type 'windows-nt))
    (customize-set-variable 'global-auto-revert-non-file-buffers t)
  (customize-set-variable 'global-auto-revert-non-file-buffers -1))

(defalias 'yes-or-no-p 'y-or-n-p) ; Shorten resposes to y/n

(setq
 tooltip-use-echo-area t ; Open tooltips in echo area
 large-file-warning-threshold (* 100 1024 1024) ; Large file warning at 100MB
 use-dialog-box nil ; Disable GUI boxes
 x-select-enable-clipboard t ; Allow copy/paste from system clipboard
 auto-save-default t) ; Enable auto saving

(setq-default
 x-stretch-cursor t ; Stretch cursor to glyph length
 help-window-select t ; Focus help window when opened
 cursor-in-non-selected-windows nil ; Hide cursor in non-focused window
 window-combination-resize t) ; Take new window space from all windows

;; Allow narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;------------------------------------------------------------------------------
;; Set paths

(setq default-directory (getenv "HOME")) ; Set default directory to `$HOME'

(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path
            (append
             exec-path
             '("C:/msys64/usr/bin"
               "C:/msys64/mingw64/bin/"
               "C:/msys64/clang64/bin"
               "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/"
               "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/")))
      (setenv
       "PATH" ; Set envirenment variables
       (concat
        "C:/msys64/usr/bin"
        path-separator
        "C:/msys64/mingw64/bin"
        path-separator
        "C:/msys64/clang64/bin"
        path-separator
        "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/"
        ;; path-separator
        ;; "C:/Users/1556048963C/AppData/Local/Programs/MiKTeX/miktex/bin/x64/"
        path-separator
        "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/"
        path-separator
        (getenv "PATH"))))
  (setenv "PATH" ; Set envirenment variables
          (concat (getenv "HOME") "/.local/share" path-separator (getenv "PATH"))))

;; Set the `RUST_HOME' environment variable
(setenv "RUSTUP_HOME" (concat (getenv "HOME") "/.local/share/rustup"))

;;------------------------------------------------------------------------------
;; Delete does not mean obliterate
(setq
 trash-directory "~/.local/share/Trash" ; Set system trash location
 trash-directory temporary-file-directory) ; Set trast directory to `/tmp/'
(setq-default delete-by-moving-to-trash t) ; Move file to `/tmp/'

;;------------------------------------------------------------------------------
;; Hide temporary and backup files
(setq
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;------------------------------------------------------------------------------
;; View mode
(setq view-read-only t) ; Enable view mode in read only buffers

;;------------------------------------------------------------------------------
;; `recentf'
(use-package
 recentf
 :init
 (setq
  recentf-max-menu-items 10
  recentf-max-saved-items 50
  recentf-auto-cleanup 300
  recentf-exclude '("~$" "^#" "abbrevs_def"))
 :config (recentf-mode 1))

(provide '1-mod-general)
;;; 1-mod-general.el ends here
