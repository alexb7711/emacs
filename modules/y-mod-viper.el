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

 ;; Enable `viper'
 (setq
  viper-always t                        ; Set viper when appropriate
  viper-inhibit-startup-message 't      ; Don't ask to start `viper'
  viper-expert-level '3                 ; Set level of `viper'
  viper-ex-style-motion nil             ; Move to start/end of lines
  viper-ex-style-editing nil            ; Edit to start/end of lines
  viper-mode t)

 (require 'viper)

 ;; Setup defaults
 (setq-default
  viper-auto-indent t                   ; Enable auto indenting
  )

 ;; Setup configuration
 (setq
  viper-electric-mode t                 ; Enable electric mode in viper
  viper-want-ctl-h-help t               ; `C-h' is for help
  viper-shift-width 4                   ; > and < shift amount
  viper-smart-suffix-list
  '("" "tex" "c" "cc" "C" "java" "el" "html" "htm" "xml" "pl" "flr" "P" "p" "h" "H")))

;;==============================================================================
;; Advice
(advice-add 'viper-maybe-checkout :around #'mod/remove-viper-checkout)

(provide 'mod-viper)
;;; viper.el ends here
