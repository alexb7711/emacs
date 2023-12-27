;;; mod-project.el --- Summary
;;; Commentary:
;;;
;;; The behavior Emacs has with version controlled projects.
;;;
;;; Code:

(require 'vc nil t)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;; Set the `pwd' of the buffer to its file location
(defun mod/set-pwd-vc-root ()
  "Set the `pwd' to the base of the version controlled project. If a
`vc' cannot be found, then set `pwd' to the location of the file."
  (interactive)
  (when (buffer-file-name) ; When the buffer has a file name
    (if (vc-root-dir) ; Try to find the root of `vc'
        (cd (vc-root-dir)) ; Set the dir to vc dir
      (cd (file-name-directory buffer-file-name))))) ; Otherwise use the buffer location

;;------------------------------------------------------------------------------
;;
(defun mod/vc-ibuffer-filter ()
  "Filter `ibuffer' by project."
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

;;==============================================================================
;; Hooks

(add-hook 'window-state-change-hook #'mod/set-pwd-vc-root)
(add-hook 'ibuffer-hook #'mod/vc-ibuffer-filter)
(add-hook 'ibuffer-sidebar-mode-hook #'mod/vc-ibuffer-filter)

;;; mod-project.el ends here
