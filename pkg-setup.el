;;; pkg-setup.el --- Installs and update packages when Emacs is started.  -*- lexical-binding: t; -*-

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

;; https://github.com/rranelli/auto-package-update.el

;;; Code:

(require 'package nil t)

;;==============================================================================
;; Configure packages
(if (or (equal (system-name) "KRSML-8TDXBV3")
        (equal (system-name) "KRSMW-6322DVB")) ; Work computers
      (setq package-archives
      `(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . ,(concat emacs-dir "elpa-mirror/gnu"))))
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			   ("elpa" . "https://elpa.gnu.org/packages/"))))

;;==============================================================================
;; Enable package manager
;; https://stackoverflow.com/questions/73199800/emacs-warning-package-unnecessary-call-to-package-initialize-in-init-file
(when (< emacs-major-version 27)
  (package-initialize))

;;==============================================================================
;; Install packages
(defvar common-packages
  '(all-the-icons
    all-the-icons-dired
    all-the-icons-gnus
    auto-package-update
    cmake-mode
    csv-mode
    diff-hl
    dired-sidebar
    doom-themes
    helm-bibtex
    imenu-list
    langtool
    nov
    olivetti
    org
    org-caldav
    org-sync
    plantuml-mode
    python-black
    rainbow-delimiters
    rust-mode
    which-key
    yaml-mode)
  "List of packages common to all RuneMacs instances.")

(defvar special-packages
  '(pdf-tools saveplace-pdf-view treesit-auto)
  "List of special packages for special machines.")

;; If on a personal PC, install PDF-Tools
(unless (or (equal (system-name) "KRSML-8TDXBV3")
            (equal (system-name) "KRSMW-6322DVB")) ; Work computers
  (setq packages (append common-packages special-packages)))

;; Install the packages
(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

;;==============================================================================
;; Auto update packages
(use-package
 auto-package-update
 :init
 (setq
  auto-package-update-prompt-before-update t
  auto-package-update-delete-old-versions t)
 :config
 ;; Update installed Emacs packages if at least `auto-package-update-interval'
 ;; days have passed since the last update.
 (if (daemonp)
     (add-hook
      'after-make-frame-functions
      (lambda (frame)
        (with-selected-frame frame
          (auto-package-update-maybe))))
   (auto-package-update-maybe)))

(provide 'pkg-setup)
;;; pkg-setup.el ends here
