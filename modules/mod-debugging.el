;;; mod-debugging.el --- My debugging configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
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

;;==============================================================================
;; General

;; Enable/Disable `tool-bar-mode' when a debugger is running
(if (or (get-buffer "*gud*") (get-buffer "*gud-pdb*") (get-buffer "*gud-gdb*"))
    (tool-bar-mode 1)
  (tool-bar-mode -1))

;;==============================================================================
;; Configuration

(setq
 gdb-many-windows t
 gdb-debuginfod-enable-setting t
 gdb-restore-window-configuration-after-quit t)

(provide 'mod-debugging)
;;; mod-debugging.el ends here
