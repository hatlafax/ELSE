;;; else-popup-2.el --- Emacs Language Sensitive Editor (ELSE)
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
;; This package provides provides an alternative implementation of the
;; default else-default-display-menu function. Both use the same popup-
;; menu selection mechanism. This alternation does use the :isearch
;; attribute on default.
;;
(require 'popup)
(require 'cl-lib)
(require 'else-cl)
(require 'else-mode)

(defcustom else-popup-2-height 50
  "The max height of the popup-menu."
  :type 'integer
  :group 'ELSE)

(defun else-popup-2-display-menu (placeholders descriptions)
  "This is the 'default' menu selector used by ELSE. It uses the
  popup package. the user can replace this function using
  else-alternate-menu-picker in the customisation variables."
  (let ((index 0)
        (menu-list nil)
        (value nil)
        (descr nil)
        (max-desc-len 25))              ; use a default max length for
                                        ; descriptions
    (dotimes (index (length placeholders))
      (setq value (nth index placeholders))
      (setq descr (nth index descriptions))

      (push (popup-make-item value :summary descr) menu-list))

    (setq menu-list (reverse menu-list))
    ; Limit the maximum width to 80% of the window - some DESCRIPTIONs can be
    ; very long
    (popup-menu* menu-list :height else-popup-2-height :max-width 0.8 :keymap else-menu-mode-map :isearch t)))

(defun else-use-display-menu-popup-2 ()
  "Use the popup-2 menu selector."
  (interactive)
  (setq else-alternate-menu-picker "else-popup-2-display-menu"))

(setq else-alternate-menu-picker "else-popup-2-display-menu")

(provide 'else-popup-2)

;;; else-popup-2.el ends here
