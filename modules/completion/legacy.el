;;; legacy.el --- Legacy buffer completion method.   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Brown

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
(require 'cedet-global)
(require 'etags)
(require 'semantic)
(require 'semantic/db)
(require 'semantic/idle)
(require 'semantic/complete)
(require 'imenu)

;;==============================================================================
;; Functions

;;------------------------------------------------------------------------------
;;
(defun mod/get-project-root(path)
    "Return the project root if it exists given PATH."
    (ignore path)
    (last (project-current)))

;;------------------------------------------------------------------------------
;;
(defun mod/get-vc-root(path)
    "Return the vc root if it exists given PATH."
    (ignore path)
    (vc-root-dir))

;;------------------------------------------------------------------------------
;;
(defun mod/semantic-hook ()
  "Include `semantic' data in `imenu'"
  (imenu-add-to-menubar "TAGS"))

;;------------------------------------------------------------------------------
;;
(defun mod/refresh-etags ()
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (let ((tags-revert-without-query t)
        (default-directory (vc-root-dir)))  ; don't query, revert silently
    (shell-command (format "ctags -f TAGS -e -R %s" default-directory))
    (visit-tags-table default-directory nil)))

;;==============================================================================
;; Configuration

(setq
 ;; Default submodes
 semantic-default-submodes '(global-semantic-idle-scheduler-mode
                             global-semanticdb-minor-mode
                             global-semantic-idle-summary-mode
                             global-semantic-idle-completions-mode
                             global-semantic-highlight-func-mode
                             global-semantic-decoration-mode
                             global-semantic-stickyfunc-mode
                             global-semantic-idle-local-symbol-highlight-mode)

 ;; SemanticDB -- caches results of parsing source code
 semanticdb-find-default-throttle '(local project unloaded system recursive) ; How aggressive the semanticdb searches are
 semanticdb-project-root-functions '(mod/get-vc-root mod/get-project-root)

 ;; Idle Scheduler -- Performs various operations while RuneMacs is idle
 semantic-idle-scheduler-verbose-flag nil
 semantic-idle-scheduler-max-buffer-size -1 ; Re-parse file regardless of size if required
 semantic-idle-scheduler-work-idle-time 30  ; Wait X seconds before scheduling the time consuming tasks

 ;; Analyzer -- Library for performing context analysis
 semantic-complete-inline-analyzer-displayer-class 'semantic-displayer-ghost
 )

;; Enable GNU Global
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (semanticdb-enable-gnu-global-databases 'python-mode)
  (semanticdb-enable-gnu-global-databases 'elisp-mode))

;; Enable semantic
(semantic-mode 1)

;;==============================================================================
;; Hooks
(add-hook 'semantic-init-hooks 'mod/semantic-hook)
(add-hook 'after-save-hook 'mod/refresh-etags)

;;==============================================================================
;; Advice
(defadvice find-tag (before mod/refresh-etags compile)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (condition-case err
      ad-do-it
    (error (and (buffer-modified-p)
                (not (ding))
                (y-or-n-p "Buffer is modified, save it? ")
                (save-buffer))
           (mod/refresh-etags)
           ad-do-it)))

(provide 'legacy)
;;; legacy.el ends here
