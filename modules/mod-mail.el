;;; mod-mail.el --- Configuration of mail in Emacs!  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords: mail

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

;; https://gist.github.com/adamczykm/c18b1dba01492adb403c301da0d3b7c1

;;; Code:

(require 'cl-lib)

;;==============================================================================
;; Variables
(defvar mod/maildir "/var/mail"
  "Base directory for mail")

(defvar mod/maildir-skip ["news" "RMAIL"]
  "Directories to skip when recursively finding `'maildir'")

(defvar mod/mailsubdir
  (seq-filter
   (lambda (mod/maildir) (not (seq-some (lambda (x) (string-match x mod/maildir)) mod/maildir-skip)))
   (directory-files-recursively mod/maildir "\\`[^.]*\\'" t))
  "List of `maildir'")


;;==============================================================================
;; Configuration
(setq
 rmail-primary-inbox-list (cl-loop for x in mod/mailsubdir collect (concat "maildir://" x))
 rmail-file-name
 "/var/mail/RMAIL" ; Primary Rmail file
 rmail-secondary-file-directory "/var/mail/"
 rmail-remote-password-required nil
 rmail-preserve-inbox 1
 rmail-delete-after-output 1)

(provide 'mod-mail)
;;; mod-mail.el ends here
