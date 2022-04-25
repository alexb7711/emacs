;;------------------------------------------------------------------------------
;; Ctags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" "ctags" (directory-file-name dir-name))))

;;------------------------------------------------------------------------------
;; Auto Compile
(add-hook 'after-save-hook 'compiler-script)

;; Compile script
(defun compiler-script()
  "Run compile command on currently opened buffer"
  (interactive)
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))
