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

;;==============================================================================
;; General setup

;;------------------------------------------------------------------------------
;; Extra packages
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

;; Disable GUI dialog boxes
(setq use-dialog-box nil)

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
;;(cua-mode 1)

;; Disable tool bar
(menu-bar-mode -1)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Auto reload files
(global-auto-revert-mode t)

;; Auto reload dired buffers
(customize-set-variable 'global-auto-revert-non-file-buffers nil)

;; Turn on recentf mode (remembers recent files)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(setq recentf-max-menu-items 25)

(setq recentf-max-saved-items 25)
(global-set-key (kbd (concat "C-" leader " rf")) 'recentf-ido-find-file)

;; Enable tar-bar mode
(tab-bar-mode 1)

;;------------------------------------------------------------------------------
;; Terminal

;; Scroll to bottom of buffer
(setq term-show-maximum-output 1)
(setq term-scroll-to-bottom-on-output 1)

;;------------------------------------------------------------------------------
;; Compile
(setq compilation-scroll-output 1)

;; Auto Compile
(add-hook 'after-save-hook #'compiler-script)

;; Compile script
(defun compiler-script ()
  "Run compile command on currently opened buffer"
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))

;;------------------------------------------------------------------------------
;; Ctags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" "ctags" (directory-file-name dir-name))))

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
(add-hook 'ido-setup-hook #'bind-ido-keys)

(defun bind-ido-keys()
          "Add my keybindings for ido"
          (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
          (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

;; Enable ido
(ido-mode 1)

;;------------------------------------------------------------------------------
;; Flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires:
;; 	- ispell

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.
   Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings
   and comments get checked.  All other buffers get `flyspell-mode' to check all
   text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        ;; When programming
        (when (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode)))
        ;; When editing a document type text file
        (when (or (derived-mode-p 'latex-mode) (derived-mode-p 'markdown-mode))
          (progn
            (message "Flyspell on (text)")
            (flyspell-mode 1)))
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

;;------------------------------------------------------------------------------
;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode 1)
(add-hook 'latex-mode-hook #'display-line-numbers-mode 1)

;;------------------------------------------------------------------------------
;; 80 character marker
(setq-default
 whitespace-style '(face indentation trailing lines-tail)
 whitespace-line-column 80)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;------------------------------------------------------------------------------
;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;------------------------------------------------------------------------------
;; Highlighting text

;; If text is highlighted and a character is typed, delete the text
(delete-selection-mode t)

(transient-mark-mode t)

;; Allow copy/paste with x clipboard
(setq x-select-enable-clipboard t)

;;------------------------------------------------------------------------------
;; Align text
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

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
(add-hook 'latex-mode-hook 'toggle-indicate-empty-lines)

;;------------------------------------------------------------------------------
;; General

;; Set tab width
(setq tab-width 2)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Display which function you are under
(add-hook 'prog-mode-hook 'which-function-mode)

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
(add-hook 'pre-command-hook
          (lambda ()
            (if (or (get-buffer "*gud-pdb*") (get-buffer "*gud-gdb*"))
                (tool-bar-mode 1)
              (tool-bar-mode -1))))

;;==============================================================================
;; C/C++

;; Set C/C++ style
(setq c-default-style "bsd")
(setq c-basic-offset 2)

;;==============================================================================
;; Scripting

;; Make shebang file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;==============================================================================
;; Version control

;;------------------------------------------------------------------------------
;; Git gutter
(use-package git-gutter
  :hook
  (prog-mode     . git-gutter-mode)
  (shell-mode    . git-gutter-mode)
  (latex-mode    . git-gutter-mode)
  (markdown-mode . git-gutter-mode)
  :config
  (global-git-gutter-mode +1))

;;------------------------------------------------------------------------------
;; Flymake
;; Requires:
;; 	- Python: python-pyflakes
(add-hook 'prog-mode-hook 'flymake-mode t)

;;------------------------------------------------------------------------------
;; EVIL Mode
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffers-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(global-set-key (kbd "C-j") 'evil-scroll-line-down)
(global-set-key (kbd "C-k") 'evil-scroll-line-up)

(define-key evil-normal-state-map (kbd (concat leader " sv")) 'split-window-right)

(define-key evil-normal-state-map (kbd (concat leader " wb")) 'buffer-menu-other-window)
(define-key evil-normal-state-map (kbd (concat leader " b")) 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd (concat leader " wd")) 'dired-other-window)
(define-key evil-normal-state-map (kbd (concat leader " wf")) 'ido-find-file-other-window)
(define-key evil-normal-state-map (kbd (concat leader " wk")) 'kill-buffer-and-window)
(define-key evil-normal-state-map (kbd (concat leader " d")) 'delete-window)

(define-key evil-normal-state-map (kbd (concat leader " h")) 'windmove-left)
(define-key evil-normal-state-map (kbd (concat leader " j")) 'windmove-down)
(define-key evil-normal-state-map (kbd (concat leader " k")) 'windmove-up)
(define-key evil-normal-state-map (kbd (concat leader " l")) 'windmove-right)

;; Emacs mode
(global-set-key (kbd (concat "C-" leader " wb")) 'buffer-menu-other-window)
(global-set-key (kbd (concat "C-" leader " b")) 'ido-switch-buffer)
(global-set-key (kbd (concat "C-" leader " wd")) 'dired-other-window)
(global-set-key (kbd (concat "C-" leader " wf")) 'ido-find-file-other-window)
(global-set-key (kbd (concat "C-" leader " wk")) 'kill-buffer-and-window)
(global-set-key (kbd (concat "C-" leader " d")) 'delete-window)

(global-set-key (kbd (concat "C-" leader " sv")) 'split-window-right)
(global-set-key (kbd (concat "C-" leader " sh")) 'split-window-below)

(global-set-key (kbd (concat "C-" leader " h")) 'windmove-left)
(global-set-key (kbd (concat "C-" leader " j")) 'windmove-down)
(global-set-key (kbd (concat "C-" leader " k")) 'windmove-up)
(global-set-key (kbd (concat "C-" leader " l")) 'windmove-right)

(global-set-key (kbd (concat "C-" leader " tn")) 'tab-new)
(global-set-key (kbd (concat "C-" leader " tc")) 'tab-close)
(global-set-key (kbd (concat "C-" leader " tl")) 'tab-next)
(global-set-key (kbd (concat "C-" leader " th")) 'tab-previous)

(global-set-key (kbd (concat "C-" leader " f")) 'ido-find-file)

;; Highlighted regions are commented
(define-key evil-normal-state-map (kbd "C-;") 'comment-or-uncomment-region)

;;------------------------------------------------------------------------------
;; Undo-Fu
(use-package undo-fu
  :after evil
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;;------------------------------------------------------------------------------
;; Which key
(use-package which-key
    :config
    (which-key-mode))

;;------------------------------------------------------------------------------
;; LSP
;; TODO: Finish lsp support
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         (cc-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp-deferred))

;;------------------------------------------------------------------------------
;; Company (Auto Completion)
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;------------------------------------------------------------------------------
;; Auto Compile
(add-hook 'after-save-hook 'compiler-script)

;; Compile script
(defun compiler-script()
  "Run compile command on currently opened buffer"
  (interactive)
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))
