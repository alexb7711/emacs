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


;; Display which function you are under
(add-hook 'prog-mode-hook 'which-function-mode)
