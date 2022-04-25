;;------------------------------------------------------------------------------
;; Flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires:
;; 	- ispell

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.
   Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings
   and comments get checked.  All other buffers get `flyspell-mode' to check all
   text.  If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
      (progn
        ;; When programming
        (when (derived-mode-p 'prog-mode)
            (progn
              (message "Flyspell on (code)")
              (flyspell-prog-mode)))
        ;; When editing a document type text file
        (when (or (derived-mode-p 'latex-mode) (derived-mode-p 'markdown-mode))
          (progn
            (message "Flyspell on (text)")
            (flyspell-mode 1)))
        )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses
   `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn ; flyspell is on, turn it off
        (message "Flyspell off")
        (flyspell-mode -1))
                                        ; else - flyspell is off, turn it on
    (flyspell-on-for-buffer-type)))

;; Enable flyspell automatically
(add-hook 'after-change-major-mode-hook 'flyspell-on-for-buffer-type)
