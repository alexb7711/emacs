;;==============================================================================
;; Load in profile

;; Set the user details
(setq user-full-name "Alexander Brown")
(setq user-mail-address "alex.brown7711@gmail.com")

;;  Display startup time
(add-hook 'emacs-startup-hook
(lambda ()
            (message "Rational Emacs loaded in %s."
                     (emacs-init-time))))

;; Defines the user configuration var and etc folders and ensure they exist
(defvar config-etc-dir (expand-file-name "etc/" user-emacs-directory)
  "User's etc/ directory")
(defvar config-var-dir (expand-file-name "var/" user-emacs-directory)
  "User's var/ directory")

;; Create dump directories
(mkdir config-etc-dir t)
(mkdir config-var-dir t)

;;==============================================================================
;; General setup

;;------------------------------------------------------------------------------
;; Modes

;; Disable tool bar
;;(menu-bar-mode -1)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Auto reload files on save
(global-auto-revert-mode t)

;; Auto reload dired buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Turn on recentf mode (remembers recent files)
(add-hook 'after-init-hook #'recentf-mode)

;; Viper mode
(setq viper-mode t)
(require 'viper)

(define-key viper-vi-global-user-map (kbd "C-c wb") 'buffer-menu-other-window)
(define-key viper-vi-global-user-map (kbd "C-c wd") 'dired-other-window)
(define-key viper-vi-global-user-map (kbd "C-c wf") 'find-file-other-window)
(define-key viper-vi-global-user-map (kbd "C-c wh") 'split-window-right)
(define-key viper-vi-global-user-map (kbd "C-c wk") 'kill-buffer-and-window)
(define-key viper-vi-global-user-map (kbd "C-c ws") 'split-window-below)

(define-key viper-vi-global-user-map (kbd "C-c h") 'windmove-left)
(define-key viper-vi-global-user-map (kbd "C-c j") 'windmove-down)
(define-key viper-vi-global-user-map (kbd "C-c k") 'windmove-up)
(define-key viper-vi-global-user-map (kbd "C-c l") 'windmove-right)

;; Emacs mode
(global-set-key (kbd "C-c wb") 'buffer-menu-other-window)
(global-set-key (kbd "C-c wd") 'dired-other-window)
(global-set-key (kbd "C-c wf") 'find-file-other-window)
(global-set-key (kbd "C-c wh") 'split-window-right)
(global-set-key (kbd "C-c wk") 'kill-buffer-and-window)
(global-set-key (kbd "C-c ws") 'split-window-below)

(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

;;------------------------------------------------------------------------------
;; IDO
(require 'ido)

;; Enable fuzzy searching
(setq ido-enable-flex-matching t)

;; Enable ido in all minibuffers
(setq ido-everywhere t)

;; An 'intelligent' system for opening files and urls
(setq ido-use-filename-at-point 'guess)

;; Prompt to create new buffer if the file does not exist
(setq ido-create-new-buffer 'prompt)

;; Set a search priority order
(setq ido-file-extensions-order
      '(".c" ".cpp" ".h" ".hpp" ".py" ".emacs" ".el"))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote
       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]")))

;; Enable ido
(ido-mode 1)

;;==============================================================================
;; Editing

;;------------------------------------------------------------------------------
;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode 1)

;;------------------------------------------------------------------------------
;; 80 character marker
(setq-default
 whitespace-style '(face indentation trailing lines-tail)
 whitespace-line-column 80)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;------------------------------------------------------------------------------
;; Scrolling
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;;------------------------------------------------------------------------------
;; Brackets

;; Auto add coupled brackets
(electric-pair-mode 1)

;; Turn on parenthesis match highlighting
(setq show-paren-delay 0.0)
(show-paren-mode 1)

;;------------------------------------------------------------------------------
;; General

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;==============================================================================
;; Scripting

;; Make shebang file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
