;;; mod-keybindings.el --- Summary
;;; Commentary:
;;; All the custom key bindings in my Emacs con fig.
;;;
;;; Code:

(require 'bibtex nil t)
(require 'gnus-topic nil t)
(require 'org nil t)
(require 'pdf-tools nil t)
(require 'tex-mode nil t)
(require 'vc-dir nil t)
(require 'ox-beamer nil t)

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
;; Rebind leader key
(global-unset-key (kbd "C-SPC"))
(keymap-set global-map "C-SPC" mod/space-prefix-keymap)

;;------------------------------------------------------------------------------
;; `bibtex' (`mod-org.el')
(define-key bibtex-mode-map (kbd "C-<return>") 'bib/open-bibtex-pdf)
(define-key bibtex-mode-map (kbd "C-c F") 'bib/format-bib-file)

;;------------------------------------------------------------------------------
;; `pdf-tools'
(use-package
 pdf-tools
 :ensure t
 :defer t

 :bind (:map pdf-view-mode-map ("<return>" . pdf-annot-add-highlight-markup-annotation) ("x" . pdf-annot-delete)))

;;------------------------------------------------------------------------------
;; `dired' (`mod-dired.el')
(use-package
 dired-sidebar
 :ensure t
 :defer t
 :config
 (keymap-set dired-mode-map "SPC" mod/dired-space-prefix-keymap)
 (keymap-set dired-mode-map "C-SPC" mod/dired-space-prefix-keymap)
 :bind ("C-c b" . mod/sidebar-toggle))

;;------------------------------------------------------------------------------
;; `eglot' (`mod-buffer-completion.el')
(use-package
 eglot
 :ensure t
 :defer t

 :bind
 (:map
  eglot-mode-map
  ("C-c e r" . eglot-rename)
  ("C-c e o" . eglot-code-action-organize-imports)
  ("C-c e h" . eldoc)
  ("C-c e d" . xref-find-definitions)))

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

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Functions

(defun mod/flyspell-keybindings ()
  "Key bindings for `Flyspell'."
  (define-key flyspell-mode-map (kbd "C-'") 'flyspell-auto-correct-previous-word))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configuration

(use-package
 flyspell
 :ensure t
 :defer t

 :hook (text-mode . mod/flyspell-keybindings) (text-mode-hook . mod/flyspell-keybindings))

;;------------------------------------------------------------------------------
;; `ibuffer'
(use-package
 ibuffer
 :defer t

 :bind ("C-x C-b" . ibuffer))

;;------------------------------------------------------------------------------
;; `icomplete' (`mod-minibuffer-completions.el')
(define-key icomplete-minibuffer-map (kbd "<backtab>") 'icomplete-force-complete)
(define-key icomplete-minibuffer-map (kbd "C-<return>") 'icomplete-force-complete-and-exit)

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
(use-package
 org
 :defer t
 :ensure t

 :bind
 (:map org-mode-map ("C-c ]" . org-cite-insert) ("C-c i" . latex-insert-block) ("<f1>" . mod/compile-from-root-dir) ("<f2>" . mod/recompile-from-root-dir))
 (:map org-beamer-mode-map ("C-c C-c" . org-beamer-export-to-pdf))
 ("C-c o t" . org-timer-set-timer))

;;------------------------------------------------------------------------------
;; `recentf' (`mod-minibuffer-completion.el')
;; Open `recenctf-ido-find-file' function
(use-package
 recentf
 :defer t

 :bind ("C-c r" . recentf-open))

;;------------------------------------------------------------------------------
;; `rust-mode'
(use-package
 rust-mode
 :ensure t
 :defer t

 :bind (:map rust-mode-map ("C-c C-c r" . rust-run) ("C-c C-c t" . rust-test) ("C-c C-c c" . rust-compile)))

;;------------------------------------------------------------------------------
;; `windmove'
(use-package
 windmove
 :defer t

 :bind ("C-S-J" . windmove-down) ("C-S-K" . windmove-up) ("C-S-L" . windmove-right) ("C-S-H" . windmove-left))

;;------------------------------------------------------------------------------
;; `yaml-mode'
(use-package
 yaml-mode
 :ensure t
 :defer t

 :bind (:map yaml-mode-map ("<tab>" . 'yaml-indent-line)))

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
  (define-key TeX-mode-map (kbd "<f1>") 'mod/compile-from-root-dir)
  (define-key TeX-mode-map (kbd "<f2>") 'mod/recompile-from-root-dir))

;;------------------------------------------------------------------------------
;; Movement
(define-key text-mode-map (kbd "M-n") 'forward-paragraph)
(define-key text-mode-map (kbd "M-p") 'backward-paragraph)

;;------------------------------------------------------------------------------
;; Comments (`mod-text-editing.el')
(add-hook 'prog-mode-hook 'mod/comment-keybindings)
(add-hook 'text-mode-hook 'mod/comment-keybindings)

(defun mod/comment-keybindings ()
  "Key bindings for commenting text/code."
  (eval-after-load (or "flyspell" "markdown" "org") ; Disable default keybindings
    '(define-key flyspell-mode-map (kbd "C-;") nil))
  (local-set-key (kbd "C-;") 'mod/comment-dwim)
  (local-set-key (kbd "M-;") 'comment-kill)
  (local-set-key (kbd "C-x C-;") 'comment-box))

;;------------------------------------------------------------------------------
;; Scrolling
(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

;;------------------------------------------------------------------------------
;; `viper'
(define-key viper-insert-global-user-map (kbd "<backspace>") 'backward-delete-char-untabify)
(define-key viper-vi-global-user-map (kbd "C-<up>") 'enlarge-window)
(define-key viper-vi-global-user-map (kbd "C-<down>") 'shrink-window)

;; Change windows
(define-key viper-vi-global-user-map (kbd "J") 'windmove-down)
(define-key viper-vi-global-user-map (kbd "K") 'windmove-up)
(define-key viper-vi-global-user-map (kbd "L") 'windmove-right)
(define-key viper-vi-global-user-map (kbd "H") 'windmove-left)

;; Rectangles
(define-key viper-vi-global-user-map (kbd "v") 'set-mark-command)
(define-key viper-vi-global-user-map (kbd "V") 'set-mark-command)

;; Eshell
(setq mod/eshell-viper-map (make-sparse-keymap))
(define-key mod/eshell-viper-map (kbd "<return>") 'eshell-send-input)
(viper-modify-major-mode 'eshell-mode 'insert-state mod/eshell-viper-map)

;; Viper VI prefix key
(keymap-set viper-vi-global-user-map "SPC" mod/space-prefix-keymap)

;; Viper minibuffer
;; (define-key viper-minibuffer-map (kbd "<return>") 'icomplete-force-complete-and-exit)
;; (define-key viper-minibuffer-map (kbd "C-<return>") 'viper-exit-minibuffer)

;;------------------------------------------------------------------------------
;; Window management
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "<f12>") 'window-toggle-side-windows)
(global-set-key (kbd "C-x <left>") 'tab-bar-history-back)
(global-set-key (kbd "C-x <right>") 'tab-bar-history-forward)

;;------------------------------------------------------------------------------
;; Misc key bindings
(global-set-key (kbd "C-<return>") 'set-mark-command)

;; Unbind these god forsaken key
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-c"))

;;; mod-keybindings.el ends here
