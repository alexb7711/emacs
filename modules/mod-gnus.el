;;; mod-gnus.el --- Configuration for `gnus'. This includes email, RSS, and maybe even news letters!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.cam>
;; Keywords: mail, news

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

;; Multiple SMTP Accounts: https://www.emacswiki.org/emacs/GnusMSMTP

;;; Code:

(require 'gnus nil t)
(require 'gnus-topic nil t)
(require 'gnus-sum nil t)
(require 'cl-lib nil t)
(require 'message nil t)
(require 'smtpmail nil t)
(require 'nnmail nil t)
(if (display-graphic-p)
    (progn
      (require 'all-the-icons nil t)
      (require 'all-the-icons-gnus nil t)))

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;; Send emacs via multiple server accounts
(defun local-gnus-compose-mode ()
  "Keys."
  (local-set-key (kbd "C-c C-c") 'set-smtp-server-message-send-and-exit))

;;------------------------------------------------------------------------------
;; Set the SMTP Server according to the mail address we use for sending
(defun set-smtp-server-message-send-and-exit ()
  "Set SMTP server from list of multiple ones and send mail."
  (interactive)
  (message-remove-header "X-Message-SMTP-Method")
  (let ((sender (message-fetch-field "From")))
    (cl-loop
     for
     (addr server port usr stream)
     in
     smtp-accounts
     when
     (string-match addr sender)
     do
     (message-add-header (format "X-Message-SMTP-Method: smtp %s %d %s" server port usr))
     (setq smtpmail-stream-type stream))
    (let ((xmess (message-fetch-field "X-Message-SMTP-Method")))
      (if xmess
          (progn
            (message (format "Sending message using '%s' with config '%s'" sender xmess))
            (message-send-and-exit))
        (error
         "Could not find SMTP Server for this Sender address: %s. You might want to correct it or add it to the SMTP Server list 'smtp-accounts'"
         sender)))))

;;------------------------------------------------------------------------------
;; `gnus' icons
(all-the-icons-gnus-setup)

;;==============================================================================
;; Configuration

;;------------------------------------------------------------------------------
;; Defaults

(setq-default
 gnus-agent-directory "/var/mail/mail/agent/"
 nrss-directory "/var/mail/news/rss/"
 gnus-directory "/var/mail/news/"
 message-directory "/var/mail/mail"
 nnfolder-directory "/var/mail/mail"
 nnfolder-active-file "/var/mail/mail/active"
 mail-source-directory "/var/mail/mail/")

;;------------------------------------------------------------------------------
;; Configuration

(setq
 gnus-asynchronous t
 gnus-summary-line-format "%U%R %-18,18&user-date; %4L:%-25,25f %B%s\n"
 gnus-summary-mode-line-format "[%U] %p"
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-leaf-with-other "+-> "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "\\-> "
 gnus-sum-thread-tree-vertical "|"
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date) ; How to sort messages
 nnmail-split-methods 'nnmail-split-fancy
 gnus-verbose 5 ; Shut up `gnus'
 gnus-notification-minimum-level 5 ; Set level to start notifying
 gnus-interactive-exit nil) ; Leave `gnus' without asking

;;------------------------------------------------------------------------------
;; Hooks

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode) ; Use topic mode by default
(add-hook 'kill-emacs-hook 'gnus-group-exit)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications) ; Notifications

;;==============================================================================
;; `gnus-demon'
(add-hook
 'gnus-startup-hook
 #'(lambda ()
     ;; Initialize demon
     (gnus-demon-init)

     ;; Add demons
     (gnus-demon-add-handler 'gnus-demon-scan-news 30 nil)
     (gnus-demon-add-handler 'gnus-demon-scan-mail 30 nil)))

;;==============================================================================
;; Document reception
(setq
 gnus-select-method '(nnnil "") ; Set default method
 gnus-secondary-select-methods ; Secondary methods
 '((nnml "")
   (nntp "news.gwene.org")
   (nnimap
    "gmail" ; Server `gmail'
    (nnimap-address "imap.gmail.com")
    (nnimap-server-port "imaps")
    (nnimap-stream ssl)
    (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
    (nnmail-expiry-wait 90))
   (nnimap
    "usu" ; Server `usu'
    (nnimap-address "localhost") (nnimap-server-port 1143) (nnimap-stream plain) (nnir-search-engine imap))))

;;==============================================================================
;; SMTP

;;------------------------------------------------------------------------------
;; Configuration

(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-stream-type 'starttls
 smtptail-smtp-service 587
 message-sendmail-f-is-evil 't
 message-sendmail-extra-arguments '("--read-envelope-from")
 smtp-accounts
 '(("Alexander Brown <alex.brown7711@gmail.com>" "smtp.gmail.com" 587 "alex.brown7711@gmail.com" starttls)
   ("Alexander Brown <a01704744@usu.edu>" "localhost" 1025 "a01704744@aggies.usu.edu" plain)))

;;------------------------------------------------------------------------------
;; Hooks

(add-hook 'gnus-message-setup-hook 'local-gnus-compose-mode)
(add-hook 'message-mode-hook 'local-gnus-compose-mode)

;;==============================================================================
;; `message-mode'
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mail-Aliases.html

;;------------------------------------------------------------------------------
;; Configuration
(setq mail-personal-alias-file (concat emacs-dir "mailrc"))

;;------------------------------------------------------------------------------
;; Hooks
(add-hook 'message-mode-hook #'mod/langtool)
(add-hook 'message-mode-hook #'flyspell-mode)

(provide 'mod-gnus)
;;; mod-gnus.el ends here
