;;;; husky-fold.el --- Universal folding methods          -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: (("emacs" "29.1") ("husky-tools" "0.0.2"))
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
;; Methods for folding text depends on mode and available libraries.

;;; Code:

(require 'husky-tools)
(require 'husky-org)

(defconst hf--no-folding-mode-msg "No folding mode found.")

;;;###autoload
(defun hf-close-all ()
  "Close all folds."

  (interactive)
  (cond ((equal major-mode 'org-mode) (husky-org-close-all-folds))
        ((hf--origami-fold-fn-present-p origami-close-all-nodes) (call-interactively 'origami-close-all-nodes))
        ((hf--treesit-fold-fn-present-p treesit-fold-close-all) (treesit-fold-close-all))
        ((husky-tools-mode-fn-exist-p evil-mode 'evil-close-folds) (evil-close-folds))
        (t (message hf--no-folding-mode-msg))))

;;;###autoload
(defun hf-close ()
  "Close the fold under the cursor."

  (interactive)
  (cond ((equal major-mode 'org-mode) (husky-org-close-fold))
        ((hf--origami-fold-fn-present-p origami-close-node) (call-interactively 'origami-close-node))
        ((hf--treesit-fold-fn-present-p treesit-fold-close) (treesit-fold-close))
        ((husky-tools-mode-fn-exist-p evil-mode 'evil-close-fold) (evil-close-fold))
        (t (message hf--no-folding-mode-msg))))


;;;###autoload
(defun hf-open-all ()
  "Open all folds."

  (interactive)
  (cond ((equal major-mode 'org-mode) (husky-org-open-all-folds))
        ((hf--origami-fold-fn-present-p origami-open-all-nodes) (call-interactively 'origami-open-all-nodes))
        ((hf--treesit-fold-fn-present-p treesit-fold-open-all) (treesit-fold-open-all))
        ((husky-tools-mode-fn-exist-p evil-mode 'evil-open-folds) (evil-open-folds))
        (t (message hf--no-folding-mode-msg))))

;;;###autoload
(defun hf-open ()
  "Open the fold under the cursor."

  (interactive)
  (cond ((equal major-mode 'org-mode) (husky-org-open-fold))
        ((hf--origami-fold-fn-present-p origami-open-node) (call-interactively 'origami-open-node))
        ((hf--treesit-fold-fn-present-p treesit-fold-open) (treesit-fold-open))
        ((husky-tools-mode-fn-exist-p evil-mode 'evil-open-fold) (evil-open-fold))
        (t (message hf--no-folding-mode-msg))))

;;;###autoload
(defun hf-toggle-all ()
  "Toggle all folds."

  (interactive)
  (cond ((equal major-mode 'org-mode) (husky-org-toggle-fold))
        ((hf--origami-fold-fn-present-p origami-toggle-all-nodes) (call-interactively 'origami-toggle-all-nodes))
        ((hf--treesit-fold-fn-present-p treesit-fold-toggle-all) (treesit-fold-toggle-all))
        ((husky-tools-mode-fn-exist-p evil-mode 'evil-toggle-folds) (evil-toggle-folds))
        (t (message hf--no-folding-mode-msg))))

;;;###autoload
(defun hf-toggle ()
  "Toggle fold at point."

  (interactive)
  (save-excursion
    (end-of-line)
    (cond ((equal major-mode 'org-mode) (husky-org-toggle-fold))
          ((hf--origami-fold-fn-present-p origami-toggle-node) (origami-toggle-node (current-buffer) (point)))
          ((hf--treesit-fold-fn-present-p treesit-fold-toggle) (treesit-fold-toggle))
          ((husky-tools-mode-fn-exist-p evil-mode 'evil-toggle-fold) (evil-toggle-fold))
          (t (message hf--no-folding-mode-msg)))))

;;;###autoload
(defun hf-next ()
  "Go to the next fold."

  (interactive)
  (cond ((hf--origami-fold-fn-present-p origami-next-fold) (call-interactively 'origami-next-fold))
        ((hf--treesit-fold-fn-present-p treesit-fold-next) (treesit-fold-next))
        (t (message hf--no-folding-mode-msg))))

;;;###autoload
(defun hf-previous ()
  "Go to the previous fold."

  (interactive)
  (cond ((hf--origami-fold-fn-present-p origami-previous-fold) (call-interactively 'origami-previous-fold))
        ((hf--treesit-fold-fn-present-p treesit-fold-previous) (treesit-fold-previous))
        (t (message hf--no-folding-mode-msg))))


(defmacro hf--treesit-fold-fn-present-p (fn)
  "Return t if `treesit-fold-mode' is enabled and FN present."
  `(and (bound-and-true-p treesit-fold-mode) (fboundp ',fn)))

(defmacro hf--origami-fold-fn-present-p (fn)
  "Return t if `origami-mode' is enabled and FN present."
  `(and (bound-and-true-p origami-mode) (fboundp ',fn)))


(defun hf--test-run1 ()
  "Just a test run."
  (interactive)
  (message "macroexpand fn: %s" (macroexpand-all `(hf--origami-fold-fn-present-p origami-toggle-node)))
  (message "toggle present:? %s" (hf--origami-fold-fn-present-p origami-toggle-node))
  (call-interactively 'origami-toggle-node))

(provide 'husky-fold)

;; Local Variables:
;; read-symbol-shorthands: (("hf-" . "husky-fold-"))
;; End:

;;; husky-fold.el ends here
