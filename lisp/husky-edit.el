;;;; husky-edit.el --- Common functions for edit text          -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ((emacs "29.1") (husky-tools "0.0.1"))
;; Version: 0.0.3

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
;; Collection of functions for edit text via different ways like meow, evil etc.

;;; Code:

(require 'husky-tools)

(defcustom he-branch-regexp "\\(?1:[A-Za-z0-9]+\/\\)\\(?2:VW-[0-9]+\\)"
  "Regexp for branch name."
  :group 'husky-edit
  :type 'regexp)

(defcustom he-branch-regexp-match-number 2
  "Number of match in `he-branch-regexp' for task number."
  :group 'husky-edit
  :type 'number)

;;;###autoload
(defun he-insert ()
  "Go to insert mode."
  (interactive)
  (husky-tools-call-first-present-function
   '((meow-insert)
     (evil-insert))))

;;;###autoload
(defun he-insert--todo-by-current-git-branch (todo-type)
  "Insert todo with TODO-TYPE name for current git branch."
  (let* ((branch-name (husky-tools-call-function-if-exists magit-get-current-branch))
         (task (string-match he-branch-regexp branch-name))
         (task-number (match-string he-branch-regexp-match-number branch-name))
         (todo-msg (or task-number branch-name)))
    (insert (format "%s: %s " todo-type todo-msg))
    (comment-line 1)
    (previous-line)
    (end-of-line)
    (indent-according-to-mode)
    (he-insert)))


;;;###autoload
(defun he-insert-todo-by-current-git-branch ()
  "Insert todo for current git branch."
  (interactive)
  (he-insert--todo-by-current-git-branch "TODO"))

;;;###autoload
(defun he-insert-debug-by-current-git-branch ()
  "Insert debug for current git branch."
  (interactive)
  (he-insert--todo-by-current-git-branch "DEBUG"))

;;;###autoload
(defun he-insert-note-by-current-git-branch ()
  "Insert note for current git branch."
  (interactive)
  (he-insert--todo-by-current-git-branch "NOTE"))

;;;###autoload
(defun he-kill-word-backward ()
  "If `point' is followed by whitespace kill that.
Otherwise call `kill-word'"
  (interactive)
  (if (looking-back "[ \t\n]")
      (let ((pos (point)))
        (re-search-backward "[^ \t\n]" nil t)
        (forward-char)
        (kill-region pos (point)))
    (backward-kill-word 1)))

(provide 'husky-edit)

;; Local Variables:
;; read-symbol-shorthands: (("he-" . "husky-edit-"))
;; End:

;;; husky-edit.el ends here
