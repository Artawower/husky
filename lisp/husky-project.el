;;;; husky-project.el --- Collection of useful functions to work with project and projectile           -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ((emacs "29.1") (husky-tools "0.0.1"))
;; Version: 0.0.4

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
;; Collection of useful actions to work with project and projectile.

;;; Code:

(require 'husky-tools)

;;;###autoload
(defun hp-find-file ()
  "Find file in project."
  (cond ((fboundp 'consult-projectile-find-file) (call-interactively 'consult-projectile-find-file))
        ((fboundp 'projectile-find-file) (call-interactively 'projectile-find-file))
        ((fboundp 'project-find-file) (call-interactively 'project-find-file))))

(provide 'husky-project)

;; Local Variables:
;; read-symbol-shorthands: (("hp-" . "husky-project-"))
;; End:

;;; husky-project.el ends here
