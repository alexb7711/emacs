;;; helm-bibtex-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from helm-bibtex.el

(autoload 'helm-bibtex "helm-bibtex" "\
Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications).

(fn &optional ARG LOCAL-BIB INPUT)" t)
(autoload 'helm-bibtex-with-local-bibliography "helm-bibtex" "\
Search BibTeX entries with local bibliography.

If none is found the global bibliography is used instead.  With a
prefix ARG the cache is invalidated and the bibliography
reloaded.

(fn &optional ARG)" t)
(autoload 'helm-bibtex-with-notes "helm-bibtex" "\
Search BibTeX entries with notes.

With a prefix ARG the cache is invalidated and the bibliography
reread.

(fn &optional ARG)" t)
(autoload 'helm-bibtex-follow "helm-bibtex" "\


(fn &optional CITATION ARGS)" t)
(register-definition-prefixes "helm-bibtex" '("helm-"))

;;; End of scraped data

(provide 'helm-bibtex-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; helm-bibtex-autoloads.el ends here
