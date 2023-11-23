;;; mod-keybindings.el --- Summary
;;; Commentary:
;;; All the custom key bindings in my Emacs con fig.
;;;
;;; Code:

(require 'bibtex nil t)
(require 'eglot nil t)
(require 'flyspell nil t)
(require 'gnus-topic nil t)
(require 'ibuffer nil t)
(require 'org nil t)
(require 'pdf-tools nil t)
(require 'rust-mode nil t)
(require 'tex-mode nil t)
(require 'vc-dir nil t)
(require 'windmove nil t)
(require 'yaml-mode nil t)

;;==============================================================================
;; Package specific custom key-bindings
;;
;; Each section is labeled with the package and then the file(s) where the
;; configuration(s) can be found in.
;;
;; EXAMPLE:
;;
;; `package-name' (`file-name-1.el', `file-name-2.el',...)

;;------------------------------------------------------------------------------
;; `bibtex' (`mod-org.el')
(define-key bibtex-mode-map (kbd "C-<return>") 'bib/open-bibtex-pdf)
(define-key bibtex-mode-map (kbd "C-c F") 'bib/format-bib-file)

;;------------------------------------------------------------------------------
;; `pdf-tools'
(define-key pdf-view-mode-map (kbd "<return>") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "x") 'pdf-annot-delete)

;;------------------------------------------------------------------------------
;; `dired' (`mod-dired.el')
(global-set-key (kbd "C-c b") 'mod/sidebar-toggle)

;;------------------------------------------------------------------------------
;; `eglot' (`mod-buffer-completion.el')
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
(define-key eglot-mode-map (kbd "<f5>") 'xref-find-definitions)

;;------------------------------------------------------------------------------
;; `eshell' (`mod-shell.el')
(global-set-key (kbd "C-c C-s") 'eshell)

(add-hook 'eshell-mode-hook 'mod-eshell-keybindings)

(defun mod/eshell-keybindings ()
  "Set keybindings for Eshell."
  (with-eval-after-load "eshell-mode" ; Disable default keybindings
    (global-unset-key (kbd "C-c C-d")))
  (local-set-key (kbd "C-c h") 'mod/eshell-complete-history) ; Complete history
  (local-set-key (kbd "C-c d") 'mod/eshell-complete-recent-dir)) ; Complete recent dirs

;;------------------------------------------------------------------------------
;; `flyspell' (`mod-text-editing.el')
(add-hook (or 'text-mode-hook 'prog-mode-hook) 'mod/flyspell-keybindings)

(defun mod/flyspell-keybindings ()
  "Key bindings for `Flyspell'."
  (define-key flyspell-mode-map (kbd "C-'") 'flyspell-auto-correct-previous-word))

;;------------------------------------------------------------------------------
;; `ibuffer'
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;------------------------------------------------------------------------------
;; `icomplete' (`mod-minibuffer-completions.el')
(define-key icomplete-minibuffer-map (kbd "<backtab>") 'icomplete-force-complete) ; Complete the next item in the
; selection list
(define-key
 icomplete-minibuffer-map (kbd "C-<return>") ; Force the next complete and exit
 'icomplete-force-complete-and-exit)

;;------------------------------------------------------------------------------
;; `imenu' (`mod-imenu.el')
(global-set-key (kbd "C-c m") #'imenu-list-smart-toggle)

(add-hook
 'pdf-outline-minor-mode-hook
 #'(lambda () (define-key pdf-outline-minor-mode-map (kbd "<tab>") 'imenu-list-smart-toggle)))

;;-----------------------------------------------------------------------------
;; `org-agenda' (`mod-org.el')
(global-set-key (kbd "C-c o c") 'org-capture)

;;-----------------------------------------------------------------------------
;; `org' (`mod-org.el')
(define-key org-mode-map (kbd "C-c ]") 'org-cite-insert)
(define-key org-mode-map (kbd "C-c i") 'latex-insert-block)
(define-key org-mode-map (kbd "<f1>") 'org-latex-export-to-pdf)
(define-key prog-mode-map (kbd "<f2>") 'compile)

;;------------------------------------------------------------------------------
;; `recentf' (`mod-minibuffer-completion.el')
;; Open `recenctf-ido-find-file' function
(global-set-key (kbd "C-c r") 'recentf-open)

;;------------------------------------------------------------------------------
;; `rust-mode'
(define-key rust-mode-map (kbd "C-c C-c r") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-c t") 'rust-test)
(define-key rust-mode-map (kbd "C-c C-c c") 'rust-compile)

;;------------------------------------------------------------------------------
;; `windmove'
(global-set-key (kbd "C-S-J") 'windmove-down)
(global-set-key (kbd "C-S-K") 'windmove-up)
(global-set-key (kbd "C-S-L") 'windmove-right)
(global-set-key (kbd "C-S-H") 'windmove-left)
;;(with-demoted-errors "%s" (windmove-default-keybindings))

;;------------------------------------------------------------------------------
;; `yaml-mode'
(define-key yaml-mode-map (kbd "<tab>") 'yaml-indent-line)

;;------------------------------------------------------------------------------
;; `vc' (`vc-el.org')
(with-eval-after-load "vc-hooks"
  (define-key vc-prefix-map (kbd "=") #'vc-diff)
  (define-key vc-prefix-map (kbd "V") #'vc-version-diff)
  (define-key vc-prefix-map (kbd "Z") #'mod/gh-pull-request-url))
(define-key vc-dir-mode-map (kbd "=") #'vc-diff)
(define-key vc-dir-mode-map (kbd "e") #'vc-ediff)
(define-key vc-dir-mode-map (kbd "x") #'mod/vc-update-and-clean)
(define-key vc-dir-mode-map (kbd "V") #'vc-version-diff)
(define-key vc-dir-mode-map (kbd "Z") #'mod/gh-pull-request-url)

;;==============================================================================
;; My key-bindings

;;------------------------------------------------------------------------------
;; Compile
(define-key prog-mode-map (kbd "<f1>") 'compile)
(define-key prog-mode-map (kbd "<f2>") 'recompile)
(define-key text-mode-map (kbd "<f1>") 'compile)
(define-key text-mode-map (kbd "<f2>") 'recompile)

(with-eval-after-load 'tex
  (define-key TeX-mode-map (kbd "<f1>") 'compile)
  (define-key TeX-mode-map (kbd "<f2>") 'recompile))

;;------------------------------------------------------------------------------
;; Movement
(define-key text-mode-map (kbd "M-n") 'forward-paragraph)
(define-key text-mode-map (kbd "M-p") 'backward-paragraph)

;;------------------------------------------------------------------------------
;; Comments (`mod-text-editing.el')
(add-hook 'prog-mode-hook 'mod/comment-keybindings)
(add-hook 'yaml-mode-hook 'mod/comment-keybindings)
(add-hook 'text-mode-hook 'mod/comment-keybindings)

(defun mod/comment-keybindings ()
  "Key bindings for commenting text/code."
  (eval-after-load (or "flyspell" "markdown" "org") ; Disable default keybindings
    '(define-key flyspell-mode-map (kbd "C-;") nil))
  (local-set-key (kbd "C-;") 'mod/comment-dwim)
  (local-set-key (kbd "M-;") 'comment-kill)
  (local-set-key (kbd "C-x C-;") 'comment-box))

;;------------------------------------------------------------------------------
;; pomodoro timer
(global-set-key (kbd "C-c o t") 'org-timer-set-timer)

;;------------------------------------------------------------------------------
;; Scrolling
(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

;;------------------------------------------------------------------------------
;; `viper'
(define-key viper-vi-global-user-map (kbd "C-h") 'help)
(define-key viper-insert-global-user-map (kbd "<backspace>") 'backward-delete-char-untabify)
(define-key viper-vi-global-user-map (kbd "C-<up>") 'enlarge-window)
(define-key viper-vi-global-user-map (kbd "C-<down>") 'shrink-window)

;; Change windows
(define-key viper-vi-global-user-map (kbd "J") 'windmove-down)
(define-key viper-vi-global-user-map (kbd "K") 'windmove-up)
(define-key viper-vi-global-user-map (kbd "L") 'windmove-right)
(define-key viper-vi-global-user-map (kbd "H") 'windmove-left)

;; Eshell
(setq mod/eshell-viper-map (make-sparse-keymap))
(define-key mod/eshell-viper-map (kbd "<return>") 'eshell-send-input)
(viper-modify-major-mode 'eshell-mode 'insert-state mod/eshell-viper-map)

;;------------------------------------------------------------------------------
;; Window management
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "<f12>") 'window-toggle-side-windows)

;;------------------------------------------------------------------------------
;; Misc key bindings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Unbind this god forsaken key
(global-unset-key (kbd "C-z"))

;;; mod-keybindings.el ends here
