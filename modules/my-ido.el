;;------------------------------------------------------------------------------
;; IDO
(require 'ido)

;; Enable fuzzy searching
(setq ido-enable-flex-matching t)

;; Enable ido in all minibuffers
(setq ido-everywhere t)

;; An 'intelligent' system for opening files and urls
(setq ido-use-filename-at-point 'guess)

;; Prompt to create new buffer if the file does not exist
(setq ido-create-new-buffer 'prompt)

;; Set a search priority order
(setq ido-file-extensions-order
      '(".c" ".cpp" ".h" ".hpp" ".py" ".emacs" ".el"))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote
       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]")))

;; Remap next/previous match keys
(add-hook 'ido-setup-hook #'bind-ido-keys)

(defun bind-ido-keys()
          "Add my keybindings for ido"
          (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
          (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

;; Enable ido
(ido-mode 1)

;;------------------------------------------------------------------------------
;; IDO functions
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
