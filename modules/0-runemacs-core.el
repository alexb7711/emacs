;;; 3-runemacs-core.el --- Core custom commands for RuneMacs  -*- lexical-binding: t; -*-

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

(defun mod/load-face-with-daemon(face frame &rest args)
    "Set FACE from ARGS for FRAME when loading Emacs with or without the daemon.
This function is a wrapper for `set-face-attribute'. It is meant to generalize
method that faces are set for RuneMacs.

NOTE: This function should be called from a function that is
hooked to `after-make-frame-functions'."
    ;; If a frame was not supplied, get the current frame
    (unless frame
      (setq frame (selected-frame)))

    ;; Set the face attribute
      (with-selected-frame frame
        (apply #'set-face-attribute face frame args)))

(provide '3-runemacs-core)
;;; 3-runemacs-core.el ends here
