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
  "Jump between <tag> and </tag> for the tag at point."
  (interactive)
  (let (name closing beg end qin c)
    (unless (eq (char-after) ?<)
      (unless (search-backward "<" nil t)
        (user-error "No tag at point")))
    (unless (looking-at "<\\/?[[:alpha:]][[:alnum:]:._-]*\\b")
      (user-error "No tag at point"))
    (setq closing (looking-at "</"))
    (re-search-forward "<\\/?\\([[:alpha:]][[:alnum:]:._-]*\\)\\b")
    (setq name (match-string-no-properties 1))
    (goto-char (match-beginning 0))
    (save-excursion
      (setq beg (point))
      (goto-char (1+ beg))
      (while (and (not end) (not (eobp)))
        (setq c (char-after))
        (cond
         ((and qin (eq c ?\\)) (forward-char 1))
         ((or (eq c ?\") (eq c ?\'))
          (if (eq qin c) (setq qin nil) (unless qin (setq qin c))))
         ((and (not qin) (eq c ?>)) (setq end (point))))
        (forward-char 1)))
    (if closing
        (let ((depth 1) (rx (format "</?%s\\b" (regexp-quote name))))
          (goto-char beg)
          (while (and (> depth 0)
                      (re-search-backward rx nil t))
            (goto-char (match-beginning 0))
            (setq depth (if (looking-at (format "</%s\\b" (regexp-quote name)))
                            (1+ depth) (1- depth))))
          (when (= depth 0) (goto-char (match-beginning 0))))
      (let ((depth 1) (rx (format "</?%s\\b" (regexp-quote name))))
        (goto-char end)
        (while (and (> depth 0)
                    (re-search-forward rx nil t))
          (goto-char (match-beginning 0))
          (setq depth (if (looking-at (format "</%s\\b" (regexp-quote name)))
                          (1- depth) (1+ depth)))
          (goto-char (match-end 0)))
        (when (= depth 0) (goto-char (match-beginning 0)))))))

(defun bh--jump-to-parenthesis ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond
   ((looking-at "\\s(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s)") (forward-char 1) (backward-list 1))
   ((or (looking-at "<") (eq (char-after) ?<)
        (looking-at ">") (eq (char-before) ?>))
    (with-syntax-table (copy-syntax-table (syntax-table))
      (modify-syntax-entry ?< "(")
      (modify-syntax-entry ?> ")")
      (cond
       ((or (looking-at "<") (eq (char-after) ?<))
        (forward-list 1) (backward-char 1))
       ((or (looking-at ">") (eq (char-before) ?>))
        (forward-char 1) (backward-list 1)))))
   (t nil)))

;; (defun bh--jump-to-parenthesis ()
;;   "Go to the matching parenthesis if on parenthesis."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t nil)))

(defun hn--in-tag-p ()
  "Return non-nil if point is inside a valid HTML/XML tag."
  (let ((pos (point)) beg end qin c)
    (save-excursion
      (cond
       ((eq (char-after) ?<) (setq beg (point)))
       ((search-backward "<" nil t) (setq beg (point))))
      (when (and beg (looking-at "<\\([!?/]\\|[[:alpha:]]\\)"))
        (goto-char (1+ beg))
        (while (and (not end) (not (eobp)))
          (setq c (char-after))
          (cond
           ((and (not qin) (eq c ?<)) (setq beg nil))
           ((and qin (eq c ?\\)) (forward-char 1))
           ((or (eq c ?\") (eq c ?\'))
            (if (eq qin c) (setq qin nil) (unless qin (setq qin c))))
           ((and (not qin) (eq c ?>)) (setq end (point))))
          (forward-char 1))))
    (and beg end (<= beg pos) (<= pos end))))

;;;###autoload
(defun husky-navigation-bounce-paren ()
  "Move to the matching parenthesis or tag if on parenthesis."
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((hn--in-tag-p) (hn--jump-to-matching-tag))
          ((member (following-char) '(?\) ?\] ?\} ?\>)) (bh--jump-to-parenthesis))
          ((string-match "[[{(<]" next-char) (bh--jump-to-parenthesis))
          ((string-match "[\]})>]" prev-char) (bh--jump-to-parenthesis))
          ((eq major-mode 'web-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'html-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'html-ts-mode) (hn--jump-to-matching-tag))
          ((eq major-mode 'ng2-html-mode) (hn--jump-to-matching-tag))

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
