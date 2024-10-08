;;;; husky.el --- Huskey framework 🐾           -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 

;;; Code:


(require 'husky-tools)
(require 'husky-org)
(require 'husky-fold)
(require 'husky-lsp)
(require 'husky-edit)
(require 'husky-navigation)
(require 'husky-treesit)
(require 'husky-buffers)
(require 'husky-window-manager)
(require 'husky-project)

(provide 'husky)
;;; husky.el ends here
