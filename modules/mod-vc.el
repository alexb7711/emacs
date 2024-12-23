;;; mod-vc.el --- Configuring all things version control!  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: vc

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

(require 'ediff nil t)
(require 'vc nil t)
(require 'vc-dir nil t)
(require 'vc-git nil t)
(require 'ediff-vers nil t)

;;==============================================================================
;; `vc'

;;------------------------------------------------------------------------------
;; Functions

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/clone (url dir)
  "Clones git project from URL into DIR."
  (interactive "MURL: \nDDirectory: ")

  ;; Append the name of the project to the selected directory
  (if (eq system-type 'windows-nt)
      (setq dir (shell-quote-argument (concat dir (file-name-base url))))
    (setq dir (concat dir (file-name-base url))))

  ;; Clone the repository
  (message (shell-quote-argument (format "git clone --recurse-submodules -j4 %s %s" url dir)))
  (shell-command
   (format "git clone --recurse-submodules %s %s"
           (shell-quote-argument url)
           dir)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/vc-update-and-clean ()
  "Update and clean out up-to-date files."
  (interactive)
  (revert-buffer) ; Update the buffer
  (vc-dir-hide-up-to-date)) ; Remove up-to-date items

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/output-to-current-buffer (orig-fun &rest args)
  "Thus function outputs the vc text to the current buffer as temporary text.
The function will then ensure that the `*vc-git*' buffer stays
hidden."

  ;; Unused variables
  (ignore orig-fun)
  (ignore args)

  (let
      ((vc-git-buffer (concat "*vc-git : " (expand-file-name (vc-root-dir)) "*")))
    (with-current-buffer vc-git-buffer
      (message (buffer-string))
      )
    ))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; URL:
;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html
;;
;; NOTE:
;; The `car' of `vc-git-branches' is the active branch
;;
(defun mod/gh-pull-request-url ()
  "Visits the current branch's PR on Github."
  (interactive)
  (let* ((root (vc-git-root default-directory))
         (url (vc-git-repository-url root))
         (branch (car (vc-git-branches))))

    ;; If the URL being cloned is a `https::' url
    (if (string-match-p "^https:" url)
        (browse-url (format "%s/compare/%s?expand=1" (replace-regexp-in-string "\\.git$" "" url) branch))
     ;; Otherwise the url is a `ssh' link
      (browse-url
       (format "https://github.com/%s/compare/%s?expand=1"
               (replace-regexp-in-string "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1" url)
               branch)))))

;;------------------------------------------------------------------------------
;; Configuration

(defvar mod/work-git-path "C:/Users/1556048963C/AppData/Local/Programs/Git/bin/git.exe")
(if (and (eq system-type 'windows-nt) (file-directory-p mod/work-git-path))
    (setq vc-git-program mod/work-git-path)
  (setq vc-git-program "git"))

(setq
 vc-suppress-confirm t
 vc-command-messages nil
 change-log-version-info-enabled t)

;;===========================================================================
;; `ediff'

;;----------------------------------------------------------------------------
;; Variables
;; Note: these defvars are here to let cc-ediff-mode.el compile clean

(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-merge-job)
(defvar ediff-ancestor-buffer)

;;----------------------------------------------------------------------------
;; Functions

(defvar cc/ediff-revision-session-p nil
  "If t then `cc/ediff-revision-actual' has been called.
This state variable is used to insert added behavior to the
overridden function `ediff-janitor'.")

(defun cc/ediff-revision-from-menu (e)
  "Invoke `ediff-revision' on E with variable `buffer-file-name'."
  (interactive "e")
  (ignore e)
  (cc/ediff-revision))

(defun cc/ediff-revision ()
  "Run Ediff on the current `buffer-file-name' provided that it
is `vc-registered'. This function handles the interactive
concerns found in `ediff-revision'. This function will also test
if a diff should apply to the current buffer."
  (interactive)
  (when (and (bound-and-true-p buffer-file-name) (vc-registered (buffer-file-name)))
    (if (and (buffer-modified-p) (y-or-n-p (format "Buffer %s is modified.  Save buffer? " (buffer-name))))
        (save-buffer (current-buffer)))
    (message buffer-file-name)
    (cc/ediff-revision-actual))

  (cond
   ((not (bound-and-true-p buffer-file-name))
    (message (concat (buffer-name) " is not a file that can be diffed.")))
   ((not (vc-registered buffer-file-name))
    (message (concat buffer-file-name " is not under version control.")))))

(defun cc/ediff-revision-actual ()
  "Invoke Ediff logic to diff the modified repo file to its
counterpart in the current branch.

This function handles the actual diff behavior called by
`ediff-revision'."
  (let ((rev1 "")
        (rev2 ""))
    (setq cc/ediff-revision-session-p t)
    (ediff-load-version-control)
    (funcall (intern (format "ediff-%S-internal" ediff-version-control-package)) rev1 rev2 nil)))

(defun ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according
to `ediff-quit-merge-hook'.

ASK non-nil means ask the user whether to keep each unmodified
buffer, unless KEEP-VARIANTS is non-nil, in which case buffers
are never killed. A side effect of cleaning up may be that you
should be careful when comparing the same buffer in two separate
Ediff sessions: quitting one of them might delete this buffer in
another session as well.

  CC MODIFICATION: This method overrides the original Ediff function."
  (let ((ask
         (if (and (boundp 'cc/ediff-revision-session-p) cc/ediff-revision-session-p)
             nil
           ask)))
    (ediff-dispose-of-variant-according-to-user ediff-buffer-A 'A ask keep-variants)
    ;; !!!: CC Note: Test global state variable `cc/ediff-revision-session-p' to
    ;; determine if the modified repo file should be kept.
    ;; Guarding in place to hopefully avoid side-effects when `ediff-janitor' is
    ;; called from other Ediff functions. Informal testing has not revealed any
    ;; side-effects but YOLO.
    (if (and (boundp 'cc/ediff-revision-session-p) cc/ediff-revision-session-p)
        (ediff-dispose-of-variant-according-to-user
         ;; CC Note: keep-variants argument is hard-coded to t to keep
         ;; buffer holding modified repo file around.
         ediff-buffer-B 'B t t)
      (ediff-dispose-of-variant-according-to-user ediff-buffer-B 'B ask keep-variants))
    (if ediff-merge-job ; don't del buf C if merging--del ancestor buf instead
        (ediff-dispose-of-variant-according-to-user ediff-ancestor-buffer 'Ancestor ask keep-variants)
      (ediff-dispose-of-variant-according-to-user ediff-buffer-C 'C ask keep-variants))
    ;; CC Note: Reset global state variable `cc/ediff-revision-session-p'.
    (if (and (boundp 'cc/ediff-revision-session-p) cc/ediff-revision-session-p)
        (setq cc/ediff-revision-session-p nil))))

(defun cc/stash-window-configuration-for-ediff ()
  "Store window configuration to register 🧊.
Use of emoji is to avoid potential use of keyboard character to
reference the register."
  (window-configuration-to-register ?🧊))

(defun cc/restore-window-configuration-for-ediff ()
  "Restore window configuration from register 🧊.
Use of emoji is to avoid potential use of keyboard character to
reference the register."
  (jump-to-register ?🧊))

;;----------------------------------------------------------------------------
;; Configuration
(setq
 ediff-keep-variants nil
 ediff-keep-tmp-versions nil
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

;;----------------------------------------------------------------------------
;; Hooks

(add-hook 'ediff-before-setup-hook #'cc/stash-window-configuration-for-ediff)
;; !!!: CC Note: Why this is not `ediff-quit-hook' I do not know. But this works
;; for cleaning up ancillary buffers on quitting an Ediff session.
(add-hook 'ediff-after-quit-hook-internal #'cc/restore-window-configuration-for-ediff)

(provide 'mod-vc)
;;; mod-vc.el ends here
