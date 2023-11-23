;;; package -- Summary
;;; Commentary:
;;; Initialize the Emacs.
;;;
;;; Code:
;;;

;; ;;------------------------------------------------------------------------------
;; ;; `use-package'
;;
;; ;; Initialize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;;
;; (require 'use-package)
;; (setq use-package-always-ensure t)

;;==============================================================================
;; Load in modules

;; Define directories
(defvar emacs-dir (file-name-directory load-file-name) "Top level Emacs directory.")
(defvar module-dir (expand-file-name "modules" emacs-dir) "Personal configuration.")
(defvar lang-dir (expand-file-name "modules/lang" emacs-dir) "Language configuration.")

;; Install/Update packages

(if (eq system-type 'windows-nt)
    (setq package-check-signature nil))

(load (concat emacs-dir "pkg-setup.el"))

;; Add to load path
(add-to-list 'load-path module-dir)
(add-to-list 'load-path lang-dir)

;; Require *.el files in modules directory
;; TODO: change regex to ignore temporary files
(unless (file-exists-p module-dir) (make-directory module-dir))
(mapc 'load (directory-files module-dir nil "^[^#].*el$"))
(mapc 'load (directory-files lang-dir nil "^[^#].*el$"))

;;==============================================================================
;; Load in profile

;; Change in custom file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; (load custom-file 'noerror 'nomessage)

;; Set the user details
(setq user-full-name "Alexander Brown")
(setq user-mail-address "a01704744@usu.edu")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'init)
;;; init.el ends here
