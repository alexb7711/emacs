;;------------------------------------------------------------------------------
;; Compile
(setq compilation-scroll-output 1)

;; Auto Compile
(add-hook 'after-save-hook #'compiler-script)

;; Compile script
(defun compiler-script ()
  "Run compile command on currently opened buffer"
  (call-process-shell-command (concat "compile " (buffer-file-name)) nil 0))
