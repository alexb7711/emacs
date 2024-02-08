;;; lang-elisp.el --- elisp configuration              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: languages

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
;;
(defun elisp/config-elisp-mode ()
  "Configure Emacs LISP mode."
  (setq
   comment-column 40
   comment-fill-column 80
   display-fill-column-indicator-column 80))

;;------------------------------------------------------------------------------
;;
(defun elisp/autofmt-on-save ()
  "Auto format `elisp' files before saving buffer."
  (when (eq major-mode 'emacs-lisp-mode)
    (elisp-autofmt-buffer)))

;;==============================================================================
;; Configuration

;; `elisp-autofmt'
(use-package
 elisp-autofmt
 :defer t
 :after (elisp-mode)
 :config
 (if (not (eq system-type 'windows-nt))
     (add-hook 'before-save-hook #'elisp/autofmt-on-save)))

;;==============================================================================
;; Hooks

(use-package
 elisp-mode
 :defer t

 :hook (emacs-lisp-mode . elisp/config-elisp-mode))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
