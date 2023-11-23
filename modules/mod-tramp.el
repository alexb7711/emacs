;;; mod-tramp.el --- Configuration to edit files over ssh easily.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Alexander Brown

;; Author: Alexander Brown <alex.brown7711@gmail.com>
;; Keywords: terminals, unix,

;;==============================================================================
;; Default
(require 'tramp nil t)

(setq tramp-default-method "ssh")
