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

;; Create dump directories
(mkdir config-etc-dir t)
(mkdir config-var-dir t)

;;==============================================================================
;; General setup

;;------------------------------------------------------------------------------
;; Extra packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"  . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;------------------------------------------------------------------------------
;; Variables
(setq leader "SPC")

;;------------------------------------------------------------------------------
;; Unbind keys
(global-set-key (kbd (concat "C-" leader)) nil)

;;------------------------------------------------------------------------------
;; Escape cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;------------------------------------------------------------------------------
;; General

;; Disable backup files
(setq make-backup-files nil)

;; Hide temporary and backup files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Require 'y' or 'n' instead of 'yes' or 'no' response
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set default directory to root
(setq default-directory "~/")

;;------------------------------------------------------------------------------
;; Modes

;; Enable cua mode
(cua-mode 1)

;; Disable tool bar
(menu-bar-mode -1)

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

;; Enable tar-bar mode
(tab-bar-mode 1)

;; Viper mode
(setq viper-mode t)
(require 'viper)

(define-key viper-vi-global-user-map (kbd (concat "C-" leader " wb")) 'buffer-menu-other-window)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " wd")) 'dired-other-window)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " wf")) 'find-file-other-window)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " wh")) 'split-window-right)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " wk")) 'kill-buffer-and-window)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " d")) 'delete-window)
(define-key viper-vi-global-user-map (kbd (concat "C-" leader " ws")) 'split-window-below)

(define-key viper-vi-global-user-map (kbd (concat"C-" leader " h")) 'windmove-left)
(define-key viper-vi-global-user-map (kbd (concat"C-" leader " j")) 'windmove-down)
(define-key viper-vi-global-user-map (kbd (concat"C-" leader " k")) 'windmove-up)
(define-key viper-vi-global-user-map (kbd (concat"C-" leader " l")) 'windmove-right)

;; Emacs mode
(global-set-key (kbd "C-SPC wb") 'buffer-menu-other-window)
(global-set-key (kbd "C-SPC wd") 'dired-other-window)
(global-set-key (kbd "C-SPC wf") 'find-file-other-window)
(global-set-key (kbd "C-SPC wh") 'split-window-right)
(global-set-key (kbd "C-SPC wk") 'kill-buffer-and-window)
(global-set-key (kbd "C-SPC ws") 'split-window-below)

(global-set-key (kbd "C-SPC h") 'windmove-left)
(global-set-key (kbd "C-SPC j") 'windmove-down)
(global-set-key (kbd "C-SPC k") 'windmove-up)
(global-set-key (kbd "C-SPC l") 'windmove-right)

(global-set-key (kbd (concat "C-" leader " f")) 'ido-find-file)

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

;; Use virtual buffers.
;; Say you are visiting a file and
;; the buffer gets cleaned up by midnight.el.  Later, you want to
;; switch to that buffer, but find it’s no longer open.  With virtual
;; buffers enabled, the buffer name stays in the buffer list (using
;; the ‘ido-virtual’ face, and always at the end), and if you select
;; it, it opens the file back up again.  This allows you to think
;; less about whether recently opened files are still open or not.
;; Most of the time you can quit Emacs, restart, and then switch to
;; a file buffer that was previously open as if it still were.

;; Set a search priority order
(setq ido-file-extensions-order
      '(".c" ".cpp" ".h" ".hpp" ".py" ".emacs" ".el"))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote
       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]")))

;; Remap next/previous match keys
(add-hook 'ido-set-hook 'ido-my-keys)

(defun ido-my-keys()
          "Add my keybindings for ido"
          (define-key ido-completions-map (kbd "C-n") 'ido-next-match)
          (define-key ido-completions-map (kbd "C-p") 'ido-prev-match))

;; Enable ido
(ido-mode 1)

;;------------------------------------------------------------------------------
;; Flyspell
;; https://www.emacswiki.org/emacs/FlySpell

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.
   Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings
   and comments get checked.  All other buffers get `flyspell-mode' to check all
   text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        (if (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode))
          ;; else
          (progn
            (message "Flyspell on (text)")
            (flyspell-mode 1)))
        ;; I tried putting (flyspell-buffer) here but it didn't seem to work
        )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses
   `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; Enable flyspell automatically
(add-hook 'after-change-major-mode-hook 'flyspell-on-for-buffer-type)

;;==============================================================================
;; Aesthetics

;;------------------------------------------------------------------------------
;; Theme
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;;------------------------------------------------------------------------------
;; Transparent Emacs
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;;------------------------------------------------------------------------------
;; Font
(set-face-attribute 'default nil :font "Hurmit Nerd Font Mono" :height 102)
(set-face-attribute 'fixed-pitch nil :font "Hurmit Nerd Font Mono" :height 102)

;;------------------------------------------------------------------------------
;; Modeline

;; Display column number in modeline
(column-number-mode)

;;==============================================================================
;; Editing

;;------------------------------------------------------------------------------
;; Misc key bindings
(global-set-key (kbd "RET") 'newline-and-indent)

;; https://www.emacswiki.org/emacs/CommentingCode
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)

;; Highlighted regions are commented
(define-key viper-vi-global-user-map (kbd "C-;") 'comment-or-uncomment-region)

;; Backspace deletes characters and goes to previous line
(define-key viper-vi-global-user-map [backspace] 'backward-delete-char-untabify)
(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)

;;------------------------------------------------------------------------------
;; Auto completion

;; Create completion ido menu
;; https://www.emacswiki.org/emacs/HippieExpand#h5o-11
(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (while (progn
                   (funcall hippie-expand-function nil)
                   (setq last-command 'my-hippie-expand-completions)
                   (not (equal he-num -1))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string he-tried-table)))

(defun my-ido-hippie-expand-with (hippie-expand-function)
  "Offer ido-based completion using the specified hippie-expand function."
  (let* ((options (my-hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (ido-completing-read "Completions: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found"))))

(defun my-ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (my-ido-hippie-expand-with 'hippie-expand))

(global-set-key (kbd "C-/") 'my-ido-hippie-expand)

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
;; Highlighting text

;; If text is highlighted and a character is typed, delete the text
(delete-selection-mode t)

(transient-mark-mode t)

;; Allow copy/paste with x clipboard
(setq x-select-enable-clipboard t)

(global-set-key (kbd "C-v") #'set-mark-command)
(define-key viper-vi-global-user-map (kbd "v") #'set-mark-command)

;;------------------------------------------------------------------------------
;; Highlight current line
(global-hl-line-mode +1)

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
;; Indicate end of file
(setq-default indicate-empty-lines t)
(add-hook 'prog-mode-hook 'toggle-indicate-empty-lines)

;;------------------------------------------------------------------------------
;; General

;; Set tab width
(setq tab-width 2)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;==============================================================================
;; ipynb files
(use-package code-cells)

;; Use pandoc to convert
(setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
                                       ("pandoc" "--to" "org" "--from" "ipynb")
                                       org-mode))
;; Some nice keys
(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))

;;==============================================================================
;; Debugging
(add-hook 'debugger-mode-hook tool-bar-mode)

;;==============================================================================
;; C/C++
(add-hook 'pre-command-hook
          (lambda ()
            (if (or (get-buffer "*gud-pdb*") (get-buffer "*gud-gdb*"))
                (tool-bar-mode 1)
              (tool-bar-mode -1))))

;;==============================================================================
;; Scripting

;; Make shebang file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fast-but-imprecise-scrolling t)
 '(global-auto-revert-non-file-buffers t)
 '(package-selected-packages '(code-cells doom-themes use-package))
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
