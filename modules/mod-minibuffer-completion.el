;;; mod-minibuffer-completion.el -- Summary
;;; Commentary:
;;; This encompasses all the mini-buffer completions for Emacs newer and older
;;; that version 28.1.
;;;
;;; More info can be found at:
;;; `http://xahlee.info/emacs/emacs/emacs_icomplete_mode.html'
;;;
;;; Code:

(require 'minibuffer nil t)
(require 'icomplete nil t)

;;------------------------------------------------------------------------------
;; `'minibuffer'
(setq
 completion-styles '(partial-completion substring initials flex)
 completions-format 'horizontal
 completion-show-help nil
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 read-answer-short t ; Allow `y/n' responses all the time
 resize-mini-windows t)

;;------------------------------------------------------------------------------
;; `'Icomplete'
(icomplete-mode 1)
(icomplete-vertical-mode t)

;; Default parameters
(setq
 fido-mode -1
 icomplete-compute-delay 0
 icomplete-delay-completions-threshold 0
 icomplete-in-buffer nil
 icomplete-max-delay-chars 0
 icomplete-prospects-height 1
 icomplete-show-matches-on-no-input t
 icomplete-with-completion-tables t)

;;------------------------------------------------------------------------------
;; `Icomplete' functions


(provide 'mod-minibuffer-completion.el)
;;; mod-minibuffer-completion.el ends here
