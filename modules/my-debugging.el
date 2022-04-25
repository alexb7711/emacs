;;==============================================================================
;; Debugging
(add-hook 'pre-command-hook
          (lambda ()
            (if (or (get-buffer "*gud-pdb*") (get-buffer "*gud-gdb*"))
                (tool-bar-mode 1)
              (tool-bar-mode -1))))
