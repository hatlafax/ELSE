;;; else-cl.el --- Emacs Language Sensitive Editor (ELSE)
;;
;; Copyright (C) 1997 - 2017 Peter Milliken
;;
;; Author: Peter Milliken <peter.milliken@gmail.com>
;; Version: 2.1.0
;; Package Requires: ((popup "0.5.3") (emacs "25.1"))
;; Keywords: language sensitive abbreviation template placeholder
;; URL: https://github.com/peter-milliken/ELSE
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; This package provides some common functions for the various completion
;; functions.
;;

(defun  else-nth-element (element xs)
  "Return zero-indexed position of ELEMENT in list XS, or nil if absent.
The list XS is expected to be a list of lists. The head if the inner list
is tested against the ELEMENT."
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x  xs)
        (when (equal element (car x)) (throw 'nth-elt idx))
        (setq idx  (1+ idx)))
      nil)))

(provide 'else-cl)

;;; else-cl.el ends here
