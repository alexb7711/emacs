;;; lang-bibtex.el --- Bibtex configuration.         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <a01704744@usu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'bibtex nil t)
(require 'bibtex-completion nil t)
(require 'reftex nil t)
(require 'reftex-cite nil t)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;; https://www.anghyflawn.net/blog/2014/emacs-give-a-doi-get-a-bibtex-entry/
(defun bib/get-bibtex-from-doi (doi)
  "Get a BibTeX entry from the DOI."
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://dx.doi.org/%s" (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (setq bibtex-entry (buffer-substring (string-match "@" (buffer-string)) (point)))
      (kill-buffer (current-buffer))))
  (insert (decode-coding-string bibtex-entry 'utf-8))
  (bibtex-fill-entry))

;;------------------------------------------------------------------------------
;; https://www.reddit.com/r/emacs/comments/beyn9p/clean_a_bibliography_file_with_bibtexmode/
;;
(defun bib/format-bib-file ()
  "Given a bibliography buffer, clean all the entries to a certain format."
  (interactive)
  (let
      ((buf (buffer-name))) ; Get the current buffer
    (with-current-buffer buf ; Use the contents of the current buffer
      (bibtex-map-entries 'lambda (key start end) (bibtex-clean-entry t)) ; Update the entries
      (save-buffer)))) ; Save the buffer

;;------------------------------------------------------------------------------
;; https://github.com/jkitchin/org-ref/blob/26c06912c7833104c7b4c7b96b8f200e98067a68/org-ref-bibtex.el#L761`'
;;
(defun bib/open-bibtex-pdf ()
  "Open the pdf associated with the bibtex entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "file" entry))
           (pdf
            (cl-loop
             for cpath in bibtex-completion-library-path ; Search for pdf with `:file' key
             when (and (> (length key) 0) (file-exists-p (concat cpath key))) return (concat cpath key))))
      (if pdf
          (org-link-open-from-string (format "[[file:%s]]" pdf)) ; Open pdf with `:file' key
        (bibtex-completion-open-pdf (list (bibtex-completion-get-key-bibtex))))))) ; Open pdf with entry title

;;==============================================================================
;; `bibtex'

;;------------------------------------------------------------------------------
;; Configuration
(setq
 bibtex-autokey-year-length 4
 bibtex-autokey-name-year-separator "-"
 bibtex-autokey-year-title-separator "-"
 bibtex-autokey-titleword-separator "-"
 bibtex-autokey-titlewords 2
 bibtex-autokey-titlewords-stretch 1
 bibtex-autokey-titleword-length 5
 bibtex-completion-notes-path "~/NextCloud/Documents/notes/content/doc-notes/"
 bibtex-completion-bibliography '("~/NextCloud/Documents/literature/ref.bib" "~/NextCloud/Documents/library/ref.bib")
 bibtex-completion-library-path '("~/NextCloud/Documents/library/" "~/NextCloud/Documents/literature/")
 bibtex-completion-notes-template-multiple-files "* ${title}, ${author-or-editor}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

 bibtex-completion-additional-search-fields '(keywords)
 bibtex-completion-display-formats
 '((article . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
   (inbook . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
   (incollection . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
   (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
   (t . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))

;;==============================================================================
;; `reftex'

;;------------------------------------------------------------------------------
;; Configuration

;; Activate nice interface between RefTeX and AUCTeX
(setq
 reftex-plug-into-AUCTeX t
 reftex-default-bibliography '("~/Documents/citation-database/lit-ref.bib" "~/Documents/citation-database/lib-ref.bib"))

;;------------------------------------------------------------------------------
;; Hooks

;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Assistance managing references
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook
 'LaTeX-mode-hook
 #'(lambda ()
     (auto-fill-mode 1)
     (toggle-truncate-lines 1)))


(provide 'lang-bibtex)
;;; lang-bibtex.el ends here
