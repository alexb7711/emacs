;;------------------------------------------------------------------------------
;; Git gutter
(use-package git-gutter
  :hook
  (prog-mode     . git-gutter-mode)
  (shell-mode    . git-gutter-mode)
  (latex-mode    . git-gutter-mode)
  (markdown-mode . git-gutter-mode)
  :config
  (global-git-gutter-mode +1))
