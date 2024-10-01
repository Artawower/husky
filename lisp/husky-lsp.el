;;;; husky-lsp.el --- Collection of useful lsp actions           -*- lexical-binding: t; -*-
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
;; Collection of useful lsp actions.

;;; Code:

(require 'husky-tools)


;;;###autoload
(defun hl-find-definition ()
  "Find lsp definition when lsp exist and enabled, or find xref definition."
  (interactive)
  (cond ((and (bound-and-true-p lsp-bridge-mode) (fboundp 'lsp-bridge-find-def)) (lsp-bridge-find-def))
        ((and (bound-and-true-p eglot--managed-mode) eglot--managed-mode) (call-interactively 'xref-find-definitions))
        ((and (bound-and-true-p lsp-mode)
              (bound-and-true-p lsp-ui-mode)
              lsp-ui-mode
              (fboundp 'lsp-ui-peek-find-definitions))
         (lsp-ui-peek-find-definitions))
        ((and (bound-and-true-p lsp-mode)
              lsp-mode
              (fboundp 'lsp-find-definition))
         (lsp-find-definition))
        ((and (bound-and-true-p evil-mode)
              (fboundp 'evil-goto-definition))
         (evil-goto-definition))
        (t (call-interactively 'xref-find-definitions))))

;; TODO: master navigation
;;;###autoload
(defun hl-avy-go-to-definition ()
  "Call `avy-goto-word-1' and then `hl-find-definition'."
  (interactive)
  (when (fboundp 'avy-goto-word-1)
    (call-interactively 'avy-goto-word-1)
    (hl-find-definition)))

;;;###autoload
(defun hl-copy-to-register-1 ()
  "Copy current line or selection to register 1.

See also:
`hl-copy-to-register-1'
`hl-paste-from-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2012-07-17 2022-10-03 2023-04-07"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)))

;;;###autoload
(defun hl-paste-from-register-1 ()
  "Paste text from register 1.
See also: `hl-copy-to-register-1', `insert-register'.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))


;;;###autoload
(defun hl-repeat-consult-search-forward ()
  "Repeat last search forward."
  (interactive)
  (when (boundp 'consult--line-history)
    (search-forward (car consult--line-history))))


;;;###autoload
(defun hl-repeat-consult-search-backward ()
  "Repeat last search backward."
  (interactive)
  (when (boundp 'consult--line-history)
    (search-backward (car consult--line-history))))


(provide 'husky-lsp)

;; Local Variables:
;; read-symbol-shorthands: (("hl-" . "husky-lsp-"))
;; End:

;;; husky-lsp.el ends here
