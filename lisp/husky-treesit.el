;;;; husky-treesit.el --- Collection of useful functions to work with treesit           -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ((emacs "29.1") (husky-tools "0.0.1"))
;; Version: 0.0.1

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
(defun ht-setup-html-highlights ()
  "Setup Html highlights via treesit."
  (interactive)

  (setq html--treesit-settings
        (treesit-font-lock-rules
         :language 'html
         :feature 'tag
         :override t
         '((tag_name) @web-mode-html-tag-custom-face)
         
         :language 'html
         :feature 'properties
         :override t
         '((attribute_name) @font-lock-function-name-face)

         :language 'html
         :feature 'properties
         :override t
         '(((attribute_name) @font-lock-type-face (:match "\\`\\[.*\\]\\'" @font-lock-type-face)))
         

         :language 'html
         :feature 'properties
         :override t
         '((attribute_value) @font-lock-string-face)

         :language 'html
         :feature 'comment
         :override t
         '((comment) @font-lock-comment-face)

         
         :language 'html
         :feature 'properties
         :override t
         '(((attribute_name) @font-lock-number-face (:match "\\`(.*)\\'" @font-lock-number-face)))))

  (setq-local treesit-font-lock-settings (append treesit-font-lock-settings html--treesit-settings))
  ;; (setq-local treesit-font-lock-settings html--treesit-settings)
  (setq-local treesit-font-lock-rules html--treesit-settings)
  (setq-local treesit-font-lock-feature-list
              '((tag properties comment)))

  (treesit-major-mode-setup))

;;;###autoload
(defun ht-setup-typescript-highlights ()
  "Setup TypeScript highlights via treesit."
  (interactive)

  (setq typescript--treesit-settings
        (treesit-font-lock-rules
         :language 'typescript
         :feature 'decorator-identifier
         :override t
         '((decorator (call_expression function: (identifier) @font-lock-number-face)))


         :language 'typescript
         :feature 'decorator-identifier
         :override t
         '((decorator (identifier) @font-lock-number-face))


         :language 'typescript
         :feature 'named-import
         :override t
         '((import_specifier) @font-lock-number-face)

         :language 'typescript
         :feature 'this
         :override t
         '((member_expression object: (this) @font-lock-builtin-face))

         :language 'typescript
         :feature 'predefined
         :override t
         '((predefined_type) @font-lock-builtin-face)

         :language 'typescript
         :feature 'property
         :override t
         '((object (pair key: (property_identifier) @font-lock-escape-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (string) @italic-string-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (true) @italic-string-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (false) @italic-builtin-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (true) @italic-builtin-face)))


         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (null) @italic-builtin-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (undefined) @italic-builtin-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (number) @italic-builtin-face)))

         :language 'typescript
         :feature 'object-values
         :override t
         '((object (pair key: (property_identifier) value: (identifier) @italic)))))

  (setq-local treesit-font-lock-settings (append treesit-font-lock-settings typescript--treesit-settings))
  (setq-local treesit-font-lock-rules typescript--treesit-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment declaration identifier class-property)
                (keyword string escape-sequence named-import decorator-identifier type-annotation predefined_type decorator)
                (constant expression identifier number pattern property predefined this)
                (operator function bracket delimiter object-values)))

  (treesit-major-mode-setup))



;;;###autoload
(define-minor-mode husky-treesit-mode
  "Husky treesit mode for better higlight.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `ht-mode' is enabled, treesit highlight with custom faces will be configured
for matched languages."
  :init-value nil
  :global nil
  :lighter nil
  :group 'husky-treesit
  (if husky-treesit-mode
      (progn
        (when (member major-mode '(typescript-ts-mode) (ht-setup-typescript-highlights)))
        (when (member major-mode '(html-ts-mode) (ht-setup-html-highlights)))
        (add-hook 'typescript-ts-mode-hook #'ht-setup-typescript-highlights)
        (add-hook 'html-ts-mode-hook #'ht-setup-html-highlights))
    (remove-hook 'typescript-ts-mode-hook #'ht-setup-typescript-highlights)
    (remove-hook 'html-ts-mode-hook #'ht-setup-html-highlights)))

;;;###autoload
(define-globalized-minor-mode
  global-husky-treesit
  husky-treesit-mode
  (lambda ()
    (unless husky-treesit-mode
      (husky-treesit-mode)))
  :group 'husky-treesit)

(provide 'husky-treesit)

;; Local Variables:
;; read-symbol-shorthands: (("ht-" . "husky-treesit-"))
;; End:

;;; husky-treesit.el ends here
