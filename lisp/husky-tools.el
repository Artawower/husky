;;;; husky-tools.el --- Colleciton of helper functions and tools          -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Artur Yaroshenko
;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/husky
;; Package-Requires: ()
;; Version: 0.0.2

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

;;;###autoload
(defmacro ht-call-function-if-exists (function &rest args)
  "Call FUNCTION with ARGS if it exists."
  `(if (fboundp ',function)
       (apply ',function ,@args)))


;;;###autoload
(defmacro ht-call-first-present-function (function-with-args)
  "Call first present function from FUNCTION-WITH-ARGS.
Where FUNCTION-WITH-ARGS is a list of cons cells where car is a
function and cdr is a list of arguments."
  
  `(let ((result nil))
     (dolist (function-with-args ,function-with-args)
       (let ((function (car function-with-args))
             (args (cdr function-with-args)))
         (if (fboundp function)
             (progn
               (setq result (apply function args))
               (return))))
       result)))

(defmacro ht-mode-fn-exist-p (mode fn)
  "Check if FN exists and MODE is is active."
  `(and (fboundp ,fn)
        (bound-and-true-p ,mode)))

(provide 'husky-tools)

;; Local Variables:
;; read-symbol-shorthands: (("ht-" . "husky-tools-"))
;; End:

;;; husky-tools.el ends here
