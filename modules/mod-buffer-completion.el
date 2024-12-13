;;; mod-buffer-completions.el --- Configure the buffer completion environment.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: faces

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

;; https://company-mode.github.io/
;; https://emacs-lsp.github.io/lsp-mode/

;;; Code:

;;==============================================================================
;; `abbrev'

;;------------------------------------------------------------------------------
;; Configuration

;; Automatically add spelling corrections into `abbrev' file
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;;------------------------------------------------------------------------------
;; `*Completions*'

(use-package
 completion
 :defer t

 :init
 (setq
  completion-auto-help 'visible
  completion-auto-select 'second-tab
  completions-max-height 4
  completions-header-format nil))

;;==============================================================================
;; Load in completion frameworks

(if (or (version< emacs-version "29.0") mod/force-legacy)
    (load-file (concat module-dir "/completion/legacy.el"))
  (load-file (concat module-dir "/completion/modern.el")))

(provide 'mod-buffer-completion)
;;; mod-buffer-completion.el ends here
