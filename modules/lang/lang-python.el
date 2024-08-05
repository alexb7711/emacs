;;; lang-python.el -- Summary
;;; Commentary:
;;;
;;; Python configurations
;;;
;;; Code:

;;==============================================================================
;; Functions

;;==============================================================================
;; Configuration

;; Add `treesitter' when available
;; Python mode
(use-package
 python
 :defer t
 :init
 (setq
  ;; Comment filling
  comment-column 70 ; Set the comment column outside the code space
  comment-fill-column 90 ; Max comment column

  ;; Python specific configuration
  python-shell-interpreter "python3" ; Default shell interpreter
  python-indent-guess-indent-offset t ; Guess the indentation level

  ;; PEP8
  tab-width 4 ; Set tab width
  ))


;;==============================================================================
;; Python formatting
;;
;; Requirements: `python-black'
;;
(use-package
 python-black
 :ensure t
 :defer t
 :after (python)

 :hook (python-ts-mode . python-black-on-save-mode))

(provide 'lang-python)
;;; lang-text-editing.el ends here
