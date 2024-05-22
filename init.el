;;; package -- Summary
;;; Commentary:
;;;
;;; Initialize the Emacs.
;;;
;;; Code:
;;;

;;==============================================================================
;; Load modules

;; Define directories
(defvar emacs-dir (file-name-directory load-file-name)
  "Top level Emacs directory.")
(defvar module-dir (expand-file-name "modules" emacs-dir)
  "Personal configuration.")
(defvar lang-dir (expand-file-name "lang" module-dir)
  "Language configuration.")

;; Install/Update packages

(if (eq system-type 'windows-nt)
    (setq package-check-signature nil))

;; (load (concat emacs-dir "pkg-setup.el"))

;; Add to load path
(add-to-list 'load-path module-dir)
(add-to-list 'load-path lang-dir)

;; Load the modules
(mapc 'load (directory-files module-dir nil "^[a-z0-9].*\.el$"))
(mapc 'load (directory-files lang-dir nil "^lang-.*\.el$"))

;;==============================================================================
;; Load in profile

;; Set the user details
(setq user-full-name "Alexander Brown")
(setq user-mail-address "a01704744@usu.edu")

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "*** Emacs loaded in %s with %d garbage collections."
            (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
            gcs-done)))

(provide 'init)
;;; init.el ends here
