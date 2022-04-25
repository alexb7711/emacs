;;------------------------------------------------------------------------------
;; Variables
(setq leader "SPC")

;;------------------------------------------------------------------------------
;; Unbind keys
(global-set-key (kbd (concat "C-" leader)) nil)

;;------------------------------------------------------------------------------
;; Escape cancels all
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Open ido-find-file function
(global-set-key (kbd (concat "C-" leader " rf")) 'recentf-ido-find-file)

;;------------------------------------------------------------------------------
;; Misc key bindings
(global-set-key (kbd "RET") 'newline-and-indent)

;; https://www.emacswiki.org/emacs/CommentingCode
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
