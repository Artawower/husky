;;;; husky-navigation.el --- Functions for better navigation           -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ()
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
;; Collection of functions for better navigation with zero dependencies.

;;; Code:

(defun hn--forward-or-backward-sexp ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

(defun hn--jump-to-matching-tag ()
  "Jump to the matching opening/closing tag."
  (interactive)
  (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (bacward-p (string-match-p "</" line-text))
         (forward-p (string-match-p "<" line-text)))

    (cond
     ((and bacward-p (fboundp 'sgml-skip-tag-backward))
      (sgml-skip-tag-backward 1))
     ((and forward-p (fboundp 'sgml-skip-tag-forward))
      (sgml-skip-tag-forward 1))
     (t nil))))

(defun bh--jump-to-parenthesis ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t nil)))

;;;###autoload
(defun hn-bounce-paren ()
  "Move to the matching parenthesis or tag if on parenthesis."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((eq major-mode 'web-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'html-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'html-ts-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'ng2-html-mode) (hn--jump-to-matching-tag))
          ((member (following-char) '(?\) ?\] ?\} ?\>)) (bh--jump-to-parenthesis))
          ((string-match "[[{(<]" next-char) (bh--jump-to-parenthesis))
          ((string-match "[\]})>]" prev-char) (bh--jump-to-parenthesis))

          (t (progn (message "%s" "Not on a paren, brace, or bracket")
                    nil)))))
;;;###autoload
(defun hn-mark-avy-goto-word-1 (&optional symbol-p)
  "Go to the first word within avy and mark it if mode allow.
When SYMBOL-P specified, mark symbol instead of word."
  (interactive)
  (when (fboundp 'avy-goto-word-1)
    (call-interactively 'avy-goto-word-1)
    (when (and (bound-and-true-p meow-normal-mode)
               (fboundp 'meow-mark-word)
               (fboundp 'meow-mark-symbol))
      (funcall (if symbol-p #'meow-mark-symbol #'meow-mark-word) 0)))
  )

(provide 'husky-navigation)

;; Local Variables:
;; read-symbol-shorthands: (("hn-" . "husky-navigation-"))
;; End:

;;; husky-navigation.el ends here
