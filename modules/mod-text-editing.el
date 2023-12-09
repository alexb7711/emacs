;;; mod-text-editing.el -- Summary
;;; Commentary:
;;;
;;; This section covers the basics of how Emacs behaves when editing
;;; generic text files and program specific files.
;;;
;;; Code:

(require 'display-line-numbers nil t)
(require 'flyspell nil t)
(require 'hideshow nil t)
(require 'hl-line nil t)
(require 'langtool nil t)
(require 'newcomment nil t)
(require 'org-ref nil t)
(require 'whitespace nil t)

;;==============================================================================
;; General (Natural language and programming)

;;------------------------------------------------------------------------------
;; Buffer feedback

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
;; Visual lines mode

;; Configuration

(setq-default truncate-lines t)

(setq line-move-visual t) ; Move by visual lines

;; Hooks

(add-hook 'text-mode-hook #'(lambda () (visual-line-mode 1)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Symbol recognition

;; Autoloaded Functions

(global-superword-mode 1) ; Recognize hyphenated words as a single

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Folding

;; Hooks

(add-hook 'latex-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

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
;; Git gutter

;; Hooks

(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'text-mode-hook 'git-gutter-mode)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Highlight current line

;; Set visual attributes

(set-face-attribute 'hl-line nil :inherit nil :box "light grey") ; Set a box around line to not break

;; Hooks

(add-hook 'prog-mode-hook 'hl-line-mode) ; Enable when in text mode
(add-hook 'text-mode-hook 'hl-line-mode) ; and when programming.
; syntax highlighting.
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
(setq scroll-conservatively 100000)

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
 tab-always-indent nil ; When at beginning of line indent,
 ; otherwise tab
 tab-width 2 ; Set tab width
 )

;; Configuration
(setq
 tab-always-indent 'complete ; Pressing TAB will attempt a completion
 )

;; Hooks

(add-hook 'prog-mode-hook 'electric-layout-mode) ; Auto indent with special character

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Delete trailing whitespace

;; Hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Hooks

;; Auto fill in comments
(add-hook
 'prog-mode-hook
 #'(lambda ()
     (setq-local comment-auto-fill-only-comments t)
     (auto-fill-mode 1)))

;;------------------------------------------------------------------------------
;; `langtool'

(add-hook 'latex-mode-hook #'mod/langtool)
(add-hook 'markdown-mode-hook #'mod/langtool)
(add-hook 'org-mode-hook #'mod/langtool)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; `langtool' function
(defun mod/langtool ()
  "Function to reload `langtool' after each save."
  (if (eq system-type 'windows-nt)
      (setq langtool-java-bin "C:/Program Files (x86)/Common Files/Oracle/Java/javapath/java.exe")
    (setq langtool-language-tool-jar "C:/msys64/usr/share/languagetool/languagetool-commandline.jar")
    (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

  (add-hook 'after-save-hook 'langtool-check nil 'make-it-local) ; Check after save
  (require 'langtool nil t))
(add-hook 'latex-mode-hook #'mod/langtool)
(add-hook 'markdown-mode-hook #'mod/langtool)
(add-hook 'org-mode-hook #'mod/langtool)

;;------------------------------------------------------------------------------
;; Flyspell
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires:
;; 	- ispell/aspell
;;

;; Configuration

;; Specify path to aspell on Windows
(if (eq system-type 'windows-nt)
    (setq ispell-program-name "C:/msys64/mingw64/bin/aspell.exe")
  (setq ispell-program-name "aspell"))

;; Add correction to abbreviation table.
(setq
 flyspell-abbrev-p t
 flyspell-issue-message-flag nil
 flyspell-issue-welcome-flag nil)

;; Ignore source code blocks in org mode
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("~" . "~"))
(add-to-list 'ispell-skip-region-alist '("=" . "="))

;; Hooks

(add-hook 'latex-mode-hook #'flyspell-mode)
(add-hook 'markdown-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'before-save-hook #'flyspell-buffer)
(add-hook 'before-save-hook #'flyspell-buffer)

;;==============================================================================
;; Natural language

;;------------------------------------------------------------------------------
;; General

;; Configuration

(setq sentence-end-double-space nil) ; Sentences end with a single space

;; Hooks

(add-hook 'text-mode-hook 'abbrev-mode) ; Enable `abbrev' in non-programming modes

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
;; Display tabs and trailing whitespace

;; Functions

(defun mod/load-whitespace (&optional frame)
  "Function to load whitspace when running daemon (or not, I'm cool with whatever)."
  (interactive)
  ;; (select-frame frame)
  (set-face-attribute 'whitespace-indentation nil :background "black" :foreground "dim gray" :strike-through t)
  (add-hook 'prog-mode-hook #'whitespace-mode))

;; Defaults

(setq-default whitespace-style '(face tabs indentation::tab trailing))

;; Hooks

(if (daemonp)
    (add-hook 'after-make-frame-functions #'mod/load-whitespace)
  (funcall #'mod/load-whitespace frame-initial-frame))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Display which function you are under in the modeline

;; Hooks

(add-hook 'prog-mode-hook 'which-function-mode)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Flycheck
;; Requirements:
;;    - C/C++: `gcc or clang'
;;    - Python: `python-pylint'
;;    - Rust: `rust-cargo'
;;

;; Autoloaded Functions
(flycheck-mode 1)

(provide 'mod-text-editing)
;;; mod-text-editing.el ends here
