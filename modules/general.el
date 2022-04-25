;;==============================================================================
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;==============================================================================
;; Modes

;;------------------------------------------------------------------------------
;; Disable tool bar
(menu-bar-mode -1)

;;------------------------------------------------------------------------------
;; Disable tool bar
(tool-bar-mode -1)

;;------------------------------------------------------------------------------
;; Disable scroll bar
(scroll-bar-mode -1)

;;------------------------------------------------------------------------------
;; Auto reload files
(global-auto-revert-mode t)

;;------------------------------------------------------------------------------
;; Auto reload dired buffers
(customize-set-variable 'global-auto-revert-non-file-buffers nil)

;;------------------------------------------------------------------------------
;; Turn on recentf mode (remembers recent files)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;------------------------------------------------------------------------------
;; Enable tar-bar mode
(tab-bar-mode 1)

;;------------------------------------------------------------------------------
;; Disable GUI dialog boxes
(setq use-dialog-box nil)

;;------------------------------------------------------------------------------
;; Disable backup files
(setq make-backup-files nil)

;;------------------------------------------------------------------------------
;; Hide temporary and backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;------------------------------------------------------------------------------
;; Require 'y' or 'n' instead of 'yes' or 'no' response
(defalias 'yes-or-no-p 'y-or-n-p)

;;------------------------------------------------------------------------------
;; Set default directory to root
(setq default-directory "~/")

;;------------------------------------------------------------------------------
;; Allow copy/paste with x clipboard
(setq x-select-enable-clipboard t)

;;==============================================================================
;; Scripting

;; Make shebang file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
