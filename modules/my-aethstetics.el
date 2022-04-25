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
