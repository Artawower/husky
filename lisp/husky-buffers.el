;;;; husky-buffers.el --- Collection of useful functions to work with buffers           -*- lexical-binding: t; -*-
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
;; Collection of useful actions to work with buffers.

;;; Code:

(require 'husky-tools)

;;;###autoload
(defun hb-kill-invisible-buffers ()
  "Kill all buffers that are invisible in the current project."
  (interactive)
  (dolist (buf  (project-buffers (project-current)))
    ;; when buffer name doesn't start with a *
    (when (and (not (string-prefix-p "*" (buffer-name buf)))
               ;; and buffer is not visible
               (not (get-buffer-window buf 'visible)))
      ;; kill buffer
      (kill-buffer buf))))

(defun hb-open-here (buffer-name &optional callback)
  "Open buffer with BUFFER-NAME in the current window.
Run CALLBACK after buffer opened."
  (interactive)
  (let ((python-repl-buffer-name buffer-name))
    (if (get-buffer python-repl-buffer-name)
        (switch-to-buffer python-repl-buffer-name)
      (progn
        (switch-to-buffer python-repl-buffer-name)
        (run-python)))))

;;;###autoload
(defun hb-open-messages ()
  "Open *Messages* buffer."
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (pop-to-buffer "*Messages*"))

;;;###autoload
(defun hb-open-clear-messages ()
  "Open *Messages* buffer and clear it."
  (interactive)
  (if (one-window-p)
      (split-window-horizontally))
  (pop-to-buffer "*Messages*")
  (read-only-mode -1)
  (erase-buffer)
  (read-only-mode 1))

;;;###autoload
(defun hb-delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          (kill-buffer buf))))))

(provide 'husky-buffers)

;; Local Variables:
;; read-symbol-shorthands: (("hb-" . "husky-buffers-"))
;; End:

;;; husky-buffers.el ends here
