;;==============================================================================
;; Configure packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;==============================================================================
;; Load in modules

;; Define directories
(defvar emacs-dir (file-name-directory load-file-name) "Top level Emacs dir")
(defvar module-dir (expand-file-name "modules" emacs-dir) "Personal config")

;; Add to load path
(add-to-list 'load-path module-dir)

;; Require *.el files in modules directory
(unless (file-exists-p module-dir) (make-directory module-dir))
(mapc 'load (directory-files module-dir nil "^[^#].*el$"))

;;==============================================================================
;; Load in profile

;; Set the user details
(setq user-full-name "Alexander Brown")
(setq user-mail-address "alex.brown7711@gmail.com")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Defines the user configuration var and etc folders and ensure they exist
(defvar config-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "User's etc/ directory")
(defvar config-var-dir (expand-file-name "var/" user-emacs-directory)
  "User's var/ directory")

;; Change in custom file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Create dump directories
(mkdir config-etc-dir t)
(mkdir config-var-dir t)
