;;; mod-viper.el -- Summary
;;; Commentary:
;;;
;;; VIPER
;;;
;;; Code:

;;==============================================================================
;; Functions

(defun mod/remove-viper-checkout (orig-fun &rest var)
  "Dummy function call to replace `viper-maybe-checkout'"
  (interactive))

;;==============================================================================
;; Configuration

(use-package
 viper
 :init
 (setq-default
  viper-auto-indent t ; Enable auto indenting
  )

 (setq
  viper-mode t ; Enable `viper'
  viper-electric-mode t ; Enable electric mode in viper
  viper-inhibit-startup-message 't ; Don't ask to start `viper'
  viper-want-ctl-h-help t ; `C-h' is for help
  viper-ex-style-motion nil ; Move to start/end of lines
  viper-ex-style-editing nil ; Edit to start/end of lines
  viper-shift-width 4 ; > and < shift amount
  viper-smart-suffix-list '("" "tex" "c" "cc" "el" "p" "py" "rs" "sh")
  viper-expert-level '3 ; Set level of `viper'
  )
 (require 'viper))

;;==============================================================================
;; Advice
(advice-add 'viper-maybe-checkout :around #'mod/remove-viper-checkout)

(provide 'mod-viper)
;;; viper.el ends here
