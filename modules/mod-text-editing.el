;;; mod-text-editing.el -- Summary
;;; Commentary:
;;;
;;; This section covers the basics of how Emacs behaves when editing
;;; generic text files and program specific files.
;;;
;;; Code:

(require 'display-line-numbers nil t)
(require 'flycheck nil t)
(require 'flyspell nil t)
(require 'hl-line nil t)
(require 'langtool nil t)
(require 'newcomment nil t)
(require 'org-ref nil t)
(require 'rainbow-delimiters nil t)

;;==============================================================================
;; General (Natural language and programming)

;;------------------------------------------------------------------------------
;; Better Newlines
(defun mod/force-newline ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;------------------------------------------------------------------------------
;; Better Beginning of line
(defun mod/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  ;; Accept an integer value
  (interactive "^p")

  ;; If `arg' is `nil', set `arg' to 1. Otherwise keep `arg' as is
  (setq arg (or arg 1))

  ;; If `arg' != 1
  (when (/= arg 1)
    ;; Move `arg' number of lines
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)               ; Go to the first character
    (when (= orig-point (point))        ; If the current point matches the indentation
      (move-beginning-of-line 1))))     ; Go to the beginning of the line

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Highlight TODO/FIXME/BUG flags in text

;; Functions

;; https://emacs.stackexchange.com/questions/37963/highlight-a-specific-phrase-in-any-emacs-buffer-regardless-of-mode
;; `C-h f font-lock-add-keywords'
(defun mod/highlight-flags ()
  (font-lock-add-keywords nil '(("\\<\\(TODO\\):" 1 'warning prepend) ("\\<\\(FIXME\\|BUG\\):" 1 'error prepend))))

;; Hooks

(add-hook 'text-mode-hook #'mod/highlight-flags)
(add-hook 'prog-mode-hook #'mod/highlight-flags)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Truncate Lines and Visual lines mode

;; Configuration

(setq line-move-visual t) ; Move by visual lines

;; Hooks

(add-hook 'text-mode-hook #'(lambda ()
                              (visual-line-mode 1)
                              (toggle-truncate-lines 1)))

(add-hook 'prog-mode-hook #'(lambda ()
                              (visual-line-mode 0)
                              (toggle-truncate-lines 1)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Symbol recognition

;; Autoloaded Functions

(global-superword-mode 1) ; Recognize hyphenated words as a single

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Folding

(use-package hideshow :hook (latex-mode . hs-minor-mode) (prog-mode . hs-minor-mode))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Line numbers

;; Functions
(defun mod/toggle-absolute-relative-linum ()
  "Toggles the absolute and relative line numbers depending on the
viper mode. If the viper mode is in `viper-vi-state-mode' then
relative line number shall be displayed. If the mode is
`viper-insert-state-mode' then absolute line numbers shall be
displayed."

  (when (derived-mode-p 'prog-mode)
    (if (or (eq viper-current-state 'vi-state) (eq viper-current-state 'replace-state))
        (setq display-line-numbers 'relative)
      (setq display-line-numbers t))))

;; Hooks

(eval-after-load "viper" '(add-hook 'viper-post-command-hooks #'mod/toggle-absolute-relative-linum))
(add-hook 'prog-mode-hook #'display-line-numbers-mode 1)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; `diff-hl'

;; Hooks
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-dir-mode 1))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Highlight current line

(use-package
 hl-line
 :init (set-face-attribute 'hl-line nil :inherit nil :box t :underline nil)
 :hook
 (prog-mode . hl-line-mode)
 (package-menu-mode . hl-line-mode)
 (text-mode . hl-line-mode))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Searching

;; Configuration

(setq
 isearch-lazy-count t ; Enable lazy counting
 lazy-count-suffix-format nil ; Disable text after search
 lazy-count-prefix-format "(%s/%s) ") ; Text format for lazy search

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Scrolling

;; Configuration

;; Cursor scrolling
(setq
 scroll-conservatively 100000
 scroll-preserve-screen-position 'always)

;; Mouse Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;------------------------------------------------------------------------------
;; Formatting

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Auto insert

;; Autoloaded Functions

(auto-insert-mode t)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Set the width for automatic break line

;; Hooks

(add-hook 'prog-mode-hook #'(lambda () (setq-default fill-column 120)))
(add-hook 'text-mode-hook #'(lambda () (setq-default fill-column 120)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Indenting

;; Defaults

;; Use spaces instead of tabs
(setq-default
 indent-tabs-mode nil ; Space instead of tabs
 tab-always-indent 'complete ; Pressing TAB will attempt a completion
 tab-width 2 ; Set tab width
 )

;; Configuration
(setq
 tab-always-indent 'complete ; Pressing TAB will attempt a completion
 )

;; Hooks

(add-hook 'prog-mode-hook 'electric-layout-mode) ; Auto indent with special character

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Delete trailing `whitespace'

;; Functions
(defun mod/delete-trailing-whitespace ()
  "Function wrapper for deleting trailing whitespace."
  (require 'whitespace nil t)

  (when (not (eq major-mode 'fundamental-mode))
    (delete-trailing-whitespace)))

;; Hooks

(add-hook 'before-save-hook 'mod/delete-trailing-whitespace)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Text regions

;; Autoloaded Functions

(delete-selection-mode t) ; Delete region if text is modified

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Braces

;; Defaults

(setq show-paren-delay 0.0) ; Set delay to highlight parenthesis

;; Hooks

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; Chang the color of matched parenthesis

;; Autoloaded Functions

(electric-pair-mode 1) ; Auto-close braces
(show-paren-mode 1) ; Highlight matching parenthesis

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Comments

;; Defaults

(setq-default
 comment-column 80 ; Start comments at column 80
 comment-fill-column 120) ; Wrap comments at column 120

;; Functions

;; My DWIM alternative
(defun mod/comment-dwim (&optional arg)
  "Alternative to `comment-dwim' offers a single wrapper around `comment-line',
`comment-indent', and `comment-dwim'.

If the region is active, then toggle the comment status of the region or, if
the major mode defines as much, of all the lines implied by the region
boundaries.

If point is at the absolute beginning or end of the line, append a comment to
the line.  If a comment already exists, it will be indented using the
appropriate heuristics of (i) context, or (ii) indent column.

If somewhere inside the line, toggle the comment status of the entire line."
  (interactive "*P")
  (if (use-region-p)
      (comment-dwim arg)
    (progn
      (if (or (eq (point) (point-at-bol)) (eq (point) (point-at-eol)))
          (comment-indent)
        (save-excursion (comment-line arg))))))

;; Configuration

(setq
 comment-empty-lines t ; Include comments on blank lines
 comment-multi-line t ; Continue comment on `comment-indent-newline'
 comment-style 'indent)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Filling

;; Auto fill in comments
(add-hook
 'prog-mode-hook
 #'(lambda ()
     (setq-local comment-auto-fill-only-comments t)
     (auto-fill-mode 1)))

;;------------------------------------------------------------------------------
;; `langtool'

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; `langtool' function
(defun mod/langtool ()
  "Function to reload `langtool' after each save."
  (if (eq system-type 'windows-nt)
      (setq langtool-java-bin "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
    (setq langtool-language-tool-jar "C:/msys64/usr/share/languagetool/languagetool-commandline.jar")
    (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

  ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ;;
  (defun mod/langtool-wrapper ()
    "Function wrapper for deleting trailing whitespace."
    (when (or (eq major-mode 'org-mode) (eq major-mode 'latex-mode) (eq major-mode 'markdown-mode))
      (langtool-check)))


  (add-hook 'after-save-hook 'mod/langtool-wrapper) ; Check after save
  (require 'langtool nil t))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Configure
(use-package
 langtool
 :ensure t
 :defer t

 :hook
 (latex-mode . mod/langtool)
 (markdown-mode . mod/langtool)
 (org-mode . mod/langtool)
 (latex-mode . mod/langtool)
 (markdown-mode . mod/langtool)
 (org-mode . mod/langtool))


;;------------------------------------------------------------------------------
;; Flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires:
;; 	- ispell/aspell
;;

;; Configuration
(use-package
 flyspell
 :defer t

 :init
 ;; Add correction to abbreviation table.
 (setq
  flyspell-abbrev-p t
  flyspell-issue-message-flag nil
  flyspell-issue-welcome-flag nil)

 :config
 ;; Specify path to `hunspell' on Windows
 (if (eq system-type 'windows-nt)
     (progn
       (setq
        ispell-program-name "hunspell.exe"
        ispell-local-dictionary "en_US")
       (setenv "LANG" "en_US")
       (ispell-hunspell-add-multi-dic "en_US"))
   (setq ispell-program-name "aspell"))

 ;; Ignore source code blocks in org mode
 (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
 (add-to-list 'ispell-skip-region-alist '("~" . "~"))
 (add-to-list 'ispell-skip-region-alist '("=" . "="))

 :hook
 (latex-mode . flyspell-mode)
 (markdown-mode . flyspell-mode)
 (org-mode . flyspell-mode)
 (prog-mode . flyspell-prog-mode))
;; (after-save . flyspell-buffer))

;;==============================================================================
;; Natural language

;;------------------------------------------------------------------------------
;; General

;; Configuration

(setq sentence-end-double-space nil) ; Sentences end with a single space

;; Hooks

(add-hook 'text-mode-hook 'abbrev-mode) ; Enable `abbrev' in non-programming modes

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Unfill Paragraph
(defun mod/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Automatic table detection
(when (not (eq (buffer-file-name) "*vc-log*"))

  ;; Configuration
  (setq prettify-symbols-unprettify-at-point 'right-edge) ; Make symols devolve to plain text to

  ;; Hooks

  (add-hook 'text-mode-hook 'prettify-symbols-mode)) ; Make sybols look nice to read
; edit when at point

;;==============================================================================
;; Programming modes

;;------------------------------------------------------------------------------
;; Buffer feedback

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 80 character marker

;; Configuration

(setq
 display-fill-column-indicator-column t
 display-fill-column-indicator-character "U+2502")

;; Hooks

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode) ; Turn on marker

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Display tabs and trailing `whitespace'

;; Functions

(defun mod/load-whitespace (&optional frame)
  "Function to load `whitespace' parameters in FRAME when running daemon."

  (require 'whitespace nil t)

  (whitespace-mode 1)

  (mod/load-face-with-daemon 'whitespace-indentation nil
                             :inherit nil
                             :background nil
                             :foreground nil
                             :foreground "light gray"
                             :strike-through t))
;; Defaults

(setq-default whitespace-style '(face tabs indentation::tab trailing))

;; Hooks

(add-hook 'prog-mode-hook #'mod/load-whitespace)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Display which function you are under in the `modeline'

;; Hooks

(add-hook 'prog-mode-hook 'which-function-mode)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Flycheck
;; Requirements:
;;    - C/C++: `gcc or clang'
;;    - Python: `python-pylint'
;;    - Rust: `rust-cargo'
;;

(use-package
 flycheck
 :defer t
 :hook
 (prog-mode . flycheck-mode))

(provide 'mod-text-editing)
;;; mod-text-editing.el ends here
