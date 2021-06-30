;;; else-ivy.el --- Emacs Language Sensitive Editor (ELSE)
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
;; default else-default-display-menu function. This implemetation uses
;; the ivy completion framework for selection.
;;
(require 'cl-lib)
(require 'else-cl)
(require 'else-mode)

(defvar else-preselect--ivy-menu nil)

(declare-function ivy-read "ext:ivy.el" t t)
(declare-function ivy-set-display-transformer "ext:ivy.el" t t)
(declare-function ivy-configure "ext:ivy.el" t t)

(if (featurep 'ivy)
    (ivy-configure 'else-ivy-display-menu
      :display-transformer-fn #'else-display-transformer--ivy-menu
      )
)

(defun else-ivy-display-menu (placeholders descriptions)
  "This is the 'ivy' menu selector provided by ELSE. It uses the
  ivy package. The user can replace this function using
  else-alternate-menu-picker in the customisation variables."
  (if (featurep 'ivy)
        (let ((menu-list)
              (element nil)
              (preselect nil)
              (value nil)
              (descr nil)
              (max-len 0))

          (dotimes (index (length placeholders))
            (setq value (nth index placeholders))
            (setq descr (nth index descriptions))

            (when (> (length value) max-len)
              (setq max-len (length value)))
            (push `(,value . ,descr) menu-list)
          )

          (setq element
                (if (= 1 (length menu-list))
                    (car (car menu-list))
                  ;; else
                  (if (and else-preselect--ivy-menu
                           (else-nth-element else-preselect--ivy-menu menu-list))
                      (setq preselect else-preselect--ivy-menu)
                    ;; else
                    (setq preselect 0))

                  (defun else-display-transformer--ivy-menu (key)
                    (with-current-buffer (window-buffer (minibuffer-window))
                      (let* ((cell (assoc key menu-list))
                             (val (cdr cell))
                             (offset (round (* (window-width (minibuffer-window)) 0.3)))
                             (column (max (+ max-len 10) offset))
                             (num-spc (- column (length key)))
                             (filler (make-string num-spc ? ))
                            )
                        (if val
                            (format "%s%s%s" key filler (ivy-append-face val 'ivy-remote))
                          ;; else
                          key))))

                  (ivy-read "Select element: " menu-list :require-match t :preselect preselect :caller 'else-display--ivy-menu)
                ))
            (setq else-preselect--ivy-menu element)
            element
        )
      ;;else
      (throw 'else-runtime-error "ivy package is not available!")
  ))

(defun else-use-display-menu-ivy ()
  "Use the ivy menu selector."
  (interactive)
  (setq else-alternate-menu-picker "else-ivy-display-menu"))

(setq else-alternate-menu-picker "else-ivy-display-menu")

(provide 'else-ivy)

;;; else-ivy.el ends here
