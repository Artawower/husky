;;;; husky-fold.el --- Universal folding methods          -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: (("emacs" "29.1") ("husky-tools" "0.0.2"))
;; Version: 0.0.6

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
;; Methods for folding text depends on mode and available libraries.

;;; Code:

(require 'husky-tools)
(require 'husky-org)

(defconst hf--no-folding-mode-msg "No folding mode found.")

;;;###autoload
(defun hf-close-all ()
  "Close all folds."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-close-all-folds)
      (hf--origami-fold-fn-called-p 'origami-close-all-nodes (current-buffer))
      (hf--treesit-fold-fn-called-p 'treesit-fold-close-all)
      (hf--no-folding-provided-msg)))

;;;###autoload
(defun hf-close ()
  "Close the fold under the cursor."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-close-fold)
      (hf--origami-fold-fn-called-p 'origami-close-node (current-buffer) (point))
      (hf--treesit-fold-fn-called-p 'treesit-fold-close)
      (hf--no-folding-provided-msg)))


;;;###autoload
(defun hf-open-all ()
  "Open all folds."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-open-all-folds)
      (hf--origami-fold-fn-called-p 'origami-open-all-nodes (current-buffer))
      (hf--treesit-fold-fn-called-p 'treesit-fold-open-all)
      (hf--evil-fold-fn-called-p 'evil-open-folds)
      (hf--no-folding-provided-msg)))

;;;###autoload
(defun hf-open ()
  "Open the fold under the cursor."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-open-fold)
      (hf--origami-fold-fn-called-p 'origami-open-node (current-buffer) (point))
      (hf--treesit-fold-fn-called-p 'treesit-fold-open)
      (hf--evil-fold-fn-called-p 'evil-open-fold)
      (hf--no-folding-provided-msg)))

;;;###autoload
(defun hf-toggle-all ()
  "Toggle all folds."

  (interactive)
  (or (hf--org-fold-fn-called-p 'ignore)
      (hf--origami-fold-fn-called-p 'origami-toggle-all-nodes (current-buffer))
      (hf--treesit-fold-fn-called-p 'treesit-fold-toggle-all)
      (hf--evil-fold-fn-called-p 'evil-toggle-folds)
      (hf--no-folding-provided-msg)))

;;;###autoload
(defun hf-toggle ()
  "Toggle fold at point."

  (interactive)
  (save-excursion
    (end-of-line)
    (or (hf--org-fold-fn-called-p 'husky-org-toggle-fold)
        (hf--origami-fold-fn-called-p 'origami-toggle-node (current-buffer) (point))
        (hf--treesit-fold-fn-called-p 'treesit-fold-toggle)
        (hf--evil-fold-fn-called-p 'evil-toggle-fold)
        (hf--no-folding-provided-msg))))

;;;###autoload
(defun hf-next ()
  "Go to the next fold."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-next-fold)
      (hf--origami-fold-fn-called-p 'origami-next-fold (current-buffer) (point))
      (hf--treesit-fold-fn-called-p 'treesit-fold-next)
      (hf--no-folding-provided-msg)))

;;;###autoload
(defun hf-previous ()
  "Go to the previous fold."

  (interactive)
  (or (hf--org-fold-fn-called-p 'husky-org-previous-fold)
      (hf--origami-fold-fn-called-p 'origami-previous-fold (current-buffer) (point))
      (hf--treesit-fold-fn-called-p 'treesit-fold-previous)
      (hf--no-folding-provided-msg)))


(defun hf--org-fold-fn-called-p (fn &rest args)
  "Return t and call FN with ARGS when active mode is `org-mode' and FN present."
  (when (and (equal major-mode 'org-mode) (fboundp fn))
    (apply fn args)
    t))

(defun hf--origami-fold-fn-called-p (fn &rest args)
  "Return t and call FN with ARGS when FN present."
  (when (and (bound-and-true-p origami-mode) (fboundp fn))
    (apply fn args)
    t))

(defun hf--treesit-fold-fn-called-p (fn &rest args)
  "Return t and call FN with ARGS when FN present."
  (when (and (bound-and-true-p treesit-mode) (fboundp fn))
    (apply fn args)
    t))

(defun hf--evil-fold-fn-called-p (fn &rest args)
  "Return t and call FN with ARGS when FN present."
  (when (and (bound-and-true-p evil-mode) (fboundp fn))
    (apply fn args)
    t))

(defun hf--no-folding-provided-msg ()
  "Return message about no folding provided."
  (message hf--no-folding-mode-msg))


(provide 'husky-fold)

;; Local Variables:
;; read-symbol-shorthands: (("hf-" . "husky-fold-"))
;; End:

;;; husky-fold.el ends here
