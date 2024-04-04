;;; lang-org.el --- Summary                          -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;;

;;; Commentary:
;;; `org-mode' configurations.

;;; Code:

(require 'cl-lib nil t)
(require 'color nil t)
(require 'doi-utils nil t)
(require 'oc nil t)
(require 'org-caldav nil t)
(require 'org-compat nil t)
(require 'org-tempo nil t)
(require 'ox nil t)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;; Code blocks
(defun org/load-face-attributes (&optional frame)
  "Set the face attributes."
  (interactive)

  ;; Header
  (custom-theme-set-faces 'user
                          `(org-level-1 ((t (:inherit outline-1 :height 1.55))))
                          `(org-level-2 ((t (:inherit outline-2 :height 1.45))))
                          `(org-level-3 ((t (:inherit outline-3 :height 1.25))))
                          `(org-level-4 ((t (:inherit outline-4 :height 1.1))))
                          `(org-document-title ((t (:height 2.0 :underline t)))))

  ;; Body text configuration
  (custom-theme-set-faces 'user
                          '(org-block ((t (:inherit fixed-pitch))))
                          '(org-table ((t (:inherit fixed-pitch))))
                          '(org-formula ((t (:inherit fixed-pitch))))
                          '(org-code ((t (:inherit (shadow fixed-pitch)))))
                          '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
                          '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
                          '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
                          '(org-checkbox ((t (:inherit fixed-pitch))))))

;;------------------------------------------------------------------------------
;; List `org-agenda' files
(defvar org/org-agenda-files nil
  "List of files that are in the `org-agenda.'")
(defvar org/org-agenda-files-base-path "~/Documents/agenda"
  "Base path of `org-agenda' files.")

(defun org/get-org-agenda-files (path)
  "Dynamically set the `org-agenda-files' based on the base PATH."
  (interactive "DDirectory: ")
  (setq org/org-agenda-files (directory-files path t ".org$" nil nil)))

;;------------------------------------------------------------------------------
;; Find path to `org-agend-files' dynamically
(defun org/get-org-agenda-path (str)
  "Return path of STR agenda file."
  (car (cl-member str org/org-agenda-files :test #'string-match)))

;;==============================================================================
;; `org'

;;------------------------------------------------------------------------------
;; Functions

(defun mod/update-citiation-database-path ()
  "If the current buffer is an `org' document and the directory is
in version controlled directory, try to set the absolute path of
the bibliography directory to `./citiation-database."
  (interactive)
  (when (vc-root-dir)
    (setq org-cite-global-bibliography
          `(,(concat (expand-file-name (vc-root-dir)) "citation-database/misc.bib")
            ,(concat (expand-file-name (vc-root-dir)) "citation-database/lit-ref.bib")
            ,(concat (expand-file-name (vc-root-dir)) "citation-database/lib-ref.bib")))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun mod/async-latex-pdf-export ()
  "Asynchronously export `org' document to PDF."
  (interactive)
  (org-latex-export-to-pdf t))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(defun org/load-configuration ()
  "Load configuration for `org-mode'"
  (visual-line-mode)
  (variable-pitch-mode 1))

;;------------------------------------------------------------------------------
;; Configuration
(use-package
 org
 :ensure t
 :defer t

 :init
 (setq
  org-cite-global-bibliography
  `(,(concat (getenv "HOME") "/Documents/citation-database/lit-ref.bib")
    ,(concat (getenv "HOME") "/Documents/citation-database/lib-ref.bib"))
  org-hide-emphasis-markers t
  org-startup-with-inline-images t ; Display images by default
  org-display-remote-inline-images 'download ; Download web images
  org-export-with-smart-quotes t ; Put the correct quotes
  org-export-headline-levels 5 ; Max level that `org' will export a
  org-latex-prefer-user-labels t ; Use user labels, not generated ones
  org-confirm-babel-evaluate nil ; Just run the code
  org-image-actual-width nil ; Don't use actual image size when
  ; header to LaTeX
  search-invisible t ; Include links in `isearch'
  org-highlight-latex-and-related '(native latex script entities)
  org-latex-prefer-user-labels 1 ; Use user labels, not gereated ones
  org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
 (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (latex . t) (octave . t) (plantuml . t)))

 :hook (org-mode . org-indent-mode) (org-mode . org/load-configuration))

;;------------------------------------------------------------------------------
;; Visuals

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; `latex'
(setq org-format-latex-options
      '(:foreground
        default
        :background default
        :scale 1.7
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.5
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Hooks
(add-hook 'org-mode-hook 'org/load-face-attributes)

;;==============================================================================
;; `org-agenda'

(use-package
 org-agenda
 :defer t
 :after (org)

 :init
 (setq
  org-agenda-start-with-log-mode t
  org-log-done 'time
  org-log-into-drawer t
  org-agenda-files (org/get-org-agenda-files org/org-agenda-files-base-path)
  org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "|" "COMPLETED(c)" "CANCEL(k@)")
    (sequence "OPEN" "|" "CLOSED"))
  org-refile-targets
  `((,(org/get-org-agenda-path "agenda") :maxlevel . 1)
    (,(org/get-org-agenda-path "work") :maxlevel . 1)
    (,(org/get-org-agenda-path "routine") :maxlevel . 1)
    (,(org/get-org-agenda-path "self") :maxlevel . 1)
    (,(org/get-org-agenda-path "exercise") :maxlevel . 1)
    (,(org/get-org-agenda-path "archive") :maxlevel . 1)
    (,(org/get-org-agenda-path "self-study") :maxlevel . 1))
  org-agenda-custom-commands
  '(("d"
     "Dashboard"
     ((alltodo "" ((org-agenda-overriding-header "Active Tasks"))) (agenda "" ((org-deadline-warning-days 7)))))
    ("w" "Workflow Status"
     ((todo
       "BACKLOG"
       ((org-agenda-overriding-header "Task Backlog")
        (org-agenda-todo-list-sublevels nil)
        (org-agenda-files org-agenda-files)))
      (todo "READY" ((org-agenda-overriding-header "Ready for Work") (org-agenda-files org-agenda-files)))
      (todo "ACTIVE" ((org-agenda-overriding-header "Active Tasks") (org-agenda-files org-agenda-files)))
      (todo "COMPLETED" ((org-agenda-overriding-header "Completed Tasks") (org-agenda-files org-agenda-files)))
      (todo "CANCELED" ((org-agenda-overriding-header "Canceled Tasks") (org-agenda-files org-agenda-files))))))
  org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt"
     "Task"
     entry
     (file+olp ,(org/get-org-agenda-path "agenda") "Inbox")
     "* TODO %?\n  %U\n  %a\n  %i"
     :empty-lines 1)
    ("tw"
     "Work"
     entry
     (file+olp ,(org/get-org-agenda-path "work") "Inbox")
     "* BACKLOG %?\n  %U\n  %a\n  %i"
     :empty-lines 1)

    ("j" "Journal Entries")
    ("jj"
     "Journal"
     entry
     (file+olp+datetree ,(org/get-org-agenda-path "work"))
     "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
     :clock-in
     :clock-resume
     :empty-lines 1))
  org-ellipsis " ▾"
  org-archive-location (concat (org/get-org-agenda-path "archive") "::"))

 :config
 (advice-add 'org-refile :after 'org-save-all-org-buffers) ; Save buffers after refiling
 )

;;==============================================================================
;; `org-caldav'
(use-package
 org-caldav
 :ensure t
 :defer t
 :after (org)

 :init
 (setq
  org-caldav-calendars ; Calendars to sync with `org-caldav'
  `((:calendar-id "routine" :inbox ,(org/get-org-agenda-path "routine.org"))
    (:calendar-id "self" :inbox ,(org/get-org-agenda-path "self.org"))
    (:calendar-id "exercise" :inbox ,(org/get-org-agenda-path "exercise.org"))
    (:calendar-id "work" :inbox ,(org/get-org-agenda-path "work.org"))
    (:calendar-id "self-study" :inbox ,(org/get-org-agenda-path "self-study.org")))
  org-caldav-todo-percent-states '((0 "TODO") (100 "DONE") (0 "BACKLOG") (0 "WAIT") (25 "ACTIVE") (100 "COMPLETED"))
  org-tag-alist '(("agenda" . ?a) ("journal" . ?j) ("note" . ?n) ("idea" . ?i))
  org-icalendar-include-todo 'all ; Include todo's in export
  org-caldav-sync-todo t ; Sync todos
  org-caldav-url "https://nc.pootpower.myds.me/remote.php/dav/calendars/alex" ; Set base address of CalDav
  org-caldav-calendar-id "self" ; Set the calendar to sync
  org-caldav-inbox (org/get-org-agenda-path "work") ; Set the org file to sync to
  org-caldav-files `(,(org/get-org-agenda-path "agenda")) ; set the caldav files
  org-caldav-delete-calendar-entries 'ask ; Never delete remote items
  org-caldav-delete-org-entries 'ask) ; Never ask to delete items

 :config
 ;; Add advice to save `org' buffers after sync
 (advice-add 'org-caldav-sync :after (lambda (&rest args) (org-save-all-org-buffers))))

;;==============================================================================
;; `org-sync'

(use-package
 org-sync
 :ensure t
 :defer t
 :after (org)

 :init
 (setq org-sync-github-auth `("alexb7711" . ,(org/get-org-agenda-path "github")))
 (add-to-list 'load-path "path/to/org-sync")
 (mapc 'load '("org-sync" "org-sync-bb" "org-sync-github" "org-sync-redmine")))

;;==============================================================================
;; `org' to `LaTeX' conversion
;;
;; Requirements:
;; - `latexmk'

(use-package
 ox-latex
 :defer t
 :after (org)

 :init
 ;; Set compile command
 (setq org-latex-pdf-process '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

 :config
 ;; Create new `article' class for `org-latex-classes'
 (add-to-list
  'org-latex-classes
  '("dummy"
    ""
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list
  'org-latex-classes
  '("FrontiersinHarvard"
    "\\documentclass[utf8]{FrontiersinHarvard}"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list
  'org-latex-classes
  '("usuproposal"
    "\\documentclass[ee,proposal]{usuthesis}"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list
  'org-latex-classes
  '("usudissertation"
    "\\documentclass[ee,dissertation]{usuthesis}"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list
  'org-latex-classes
  '("article"
    "\\documentclass[11pt,a4paper,final]{article}
\\usepackage[a4paper, total={7in, 10in}]{geometry}
\\usepackage{algorithm2e}
\\usepackage{booktabs}
\\usepackage{subcaption}
\\usepackage{graphicx}
\\usepackage{tikz}"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 ;; Create new `ebook' class for `org-latex-classes'
 (add-to-list
  'org-latex-classes
  '("ebook"
    "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}"))))

(provide 'lang-org)
;;; lang-org.el ends here
