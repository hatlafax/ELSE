;;; else-mode.el --- Emacs Language Sensitive Editor (ELSE)
;;
;; Copyright (C) 1997 - 2017, 2021 Peter Milliken
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
;; This package provides a minor mode to generate text (usually language
;; specific but is certainly not limited to languages) templates.  It uses
;; persistent markers that allow easy navigation that are transparently deleted
;; when the user inserts information at the marker.  It maintains separate
;; template libraries for each major mode in which it is activated.  To activate,
;; just type M-x else-mode.
;;
(require 'popup)
(require 'cl-lib)
(require 'eieio)
(require 'else-structs)
(require 'else-template)
;; include the menu selection commands so the user doesn't have to.
(require 'else-select-menu-system)


(define-error 'else-loading-error "Loading template error")
(define-error 'else-compile-error "Compile error")

(defvar else-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map popup-menu-keymap)
    (define-key map "q" 'keyboard-quit)
    (define-key map "Q" 'keyboard-quit)
    (define-key map "s" 'popup-select)
    (define-key map "S" 'popup-select)
    map))

(defvar else-mode-key-map
  (let ((mode-map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap))
        (command-map (make-sparse-keymap)))
    (define-key mode-map "\C-c" prefix-map)
    (define-key prefix-map "/" command-map)
    (define-key command-map "e" 'else-expand)
    (define-key command-map "k" 'else-kill)
    (define-key command-map "n" 'else-next)
    (define-key command-map "p" 'else-previous)
    mode-map))

;; syntax table used for word-search purposes. Most programming languages
;; use/allow the '-' and '_' characters to be part of a variable name. The
;; following syntax table makes them part of the 'word' constituent so that
;; 'words' can be easily extracted for completion purposes
(defvar else-expand-table (copy-syntax-table (standard-syntax-table))
  "‘else-expand-abbreviation’ uses this table to locate possible candidates.")
(modify-syntax-entry ?- "w" else-expand-table)
(modify-syntax-entry ?_ "w" else-expand-table)

(cl-defstruct p-struct
  "Structure to hold placeholder text instance information."
  (name "")                             ; placeholder name (string)
  (definition nil)                      ; placeholder definition
  (start 0)                             ; start of placeholder text
  (end 0)                               ; end of placeholder text
  (column-start-position)               ; column of start of the placeholder text
  (please-duplicate nil)                ; should placeholder be duplicated?
  (mandatory nil))                      ; is placeholder mandatory?

(cl-defstruct auto-sub-pair
  "Pair of markers to define the start/end of an auto-substitution area."
  (start (make-marker))
  (end (make-marker)))

(cl-defstruct auto-sub
  "Define how many substitutions are active, the original substitution region
and a list of active substitution regions."
  (active-count 0)
  (origin-markers nil)                  ; pair defining the origin text
  (list-of-markers nil))                ; list of subtitute pairs

(cl-defstruct placeholder-marker
    "Pair of markers that embrace a placeholder."
    (m1 (make-marker))
    (m2 (make-marker)))

(defun else-create-placeholder-marker (p1 p2)
  "Create a placeholder-marker object from positions p1 and p2."
  (let ((m (make-placeholder-marker)))
    (set-marker (placeholder-marker-m1 m) p1 (current-buffer))
    (set-marker (placeholder-marker-m2 m) p2 (current-buffer))
    (set-marker-insertion-type (placeholder-marker-m1 m) t)
    (set-marker-insertion-type (placeholder-marker-m2 m) t)
    m))

(defvar else-Auto-Sub-Markers (make-auto-sub)
  "Used in the Auto-Substitution process.")

(defvar else-Current-Language nil
  "Holds the Language definition (instance of class ‘else-language’) for the current buffer.")

(make-variable-buffer-local 'else-Auto-Sub-Markers)
(make-variable-buffer-local 'else-Current-Language)

(defmacro else-preserve-hook-excursion (&rest body)
  "Disable and restore the change-hook functions providing a protected form for BODY."
  (declare (debug (&rest form)))
  `(let ()
     (remove-hook 'before-change-functions 'else-before-change t)
     (remove-hook 'after-change-functions  'else-after-change  t)
     (condition-case err
         (progn
           ,@body)
       (error nil))
     (add-hook 'before-change-functions 'else-before-change t t)
     (add-hook 'after-change-functions  'else-after-change  nil t)))

(defmacro else-run-when-active (&rest body)
  "Provide a wrapper for BODY that check if ‘else-mode’ is enabled."
  (declare (debug (&rest form)))
  `(if (not else-mode)
       (error "ELSE mode not enabled")
     (progn
       ,@body)))

(defun else-activate-mode ()
  "Activate ELSE mode."
  (let ()
    ;; catch any errors during the activation process and display an error
    ;; message to the user.
    (condition-case err
        (progn
          ;; clear the auto-substitution markers
          (setf (auto-sub-active-count else-Auto-Sub-Markers) 0)
          (when (else-load-language)
            (else-setup-change-hooks)
            ;; check if the key binding(s) for else-expand need to be added to
            ;; the else-menu-mode-map - this allows the user to select the menu
            ;; entry using whatever key binding is active for else-expand
            (dolist (key (where-is-internal 'else-expand))
              (unless (eq (lookup-key else-menu-mode-map key) 'popup-select)
                (define-key else-menu-mode-map key 'popup-select)))
            ;; if the buffer is empty then insert the initial string specified
            ;; in the language template
            (when (= (buffer-size) 0)
              ;; put a space at the end of the initial-string, this helps
              ;; else-next with boundary conditions
              (insert (oref else-Current-Language :initial-string))
              (goto-char (point-min))
              (else-next))))
      ((else-loading-error else-compile-error)
       (setq else-mode nil)
       (cl-case (nth 0 err)
         (else-compile-error
          (message (format "Compile aborted - %s" (nth 1 err)))
          (set-buffer (nth 2 err)))
         (else-loading-error
          (message (nth 1 err))))))))

(defun else-after-change (begin end length)
  "Repeat any auto-substitutions (if required)."
  (let ((origin (auto-sub-origin-markers else-Auto-Sub-Markers))
        (auto-sub-list (auto-sub-list-of-markers else-Auto-Sub-Markers))
        (this-pair nil))
    (unless undo-in-progress
      (when (> (auto-sub-active-count else-Auto-Sub-Markers) 0)
        (when (and (>= begin (auto-sub-pair-start origin))
                   (< begin (auto-sub-pair-end origin)))
          (save-excursion
            (else-preserve-hook-excursion
             ;; iterate the change over all of the markers
             (dotimes (index (auto-sub-active-count else-Auto-Sub-Markers))
               (setq this-pair (nth index (auto-sub-list-of-markers else-Auto-Sub-Markers)))
               (delete-region (marker-position (auto-sub-pair-start this-pair))
                              (1- (marker-position (auto-sub-pair-end this-pair))))
               (goto-char (marker-position (auto-sub-pair-start this-pair)))
               (insert (buffer-substring (marker-position (auto-sub-pair-start origin))
                                         (1- (marker-position
                                              (auto-sub-pair-end origin)))))))))))))

(defun else-before-change (begin end)
  "Before any change, delete any enclosing placeholder."
  (let ((this-pos)
        (dup-direction nil)
        (action-struct)
        (marker-index)
        (entity-details nil)
        (started-auto-sub nil))
    (save-match-data
      (catch 'this-command-is-nil
        ;; auto-complete (?) sets this-command to nil when it is populating and
        ;; presenting the completion menu - skip out early if this is the case
        (when (eq this-command nil)
          (throw 'this-command-is-nil nil))

        (setq entity-details (else-in-placeholder))
        (when entity-details
          ;; Protect against the editor being in overwrite mode - it just
          ;; doesn't make sense to run ELSE in overwrite mode, so toggle the
          ;; mode back into insert mode. Note: only do this if point is within a
          ;; placeholder i.e. the placeholder (and any auto-repeat sites) are
          ;; about to be changed.
          (when overwrite-mode
            (message "Changing to Insert mode - ELSE isn't compatible with overwrite")
            (overwrite-mode -1))

          (setq started-auto-sub (else-initialise-auto-subst-markers entity-details))

          (else-delete-placeholder-text-and-duplicate entity-details))

        ;; turn off auto-substitute?
        (when (and (> (auto-sub-active-count else-Auto-Sub-Markers) 0)
                   (not started-auto-sub)
                   (not (and (>= begin (auto-sub-pair-start
                                        (auto-sub-origin-markers else-Auto-Sub-Markers)))
                             (< begin (auto-sub-pair-end
                                       (auto-sub-origin-markers else-Auto-Sub-Markers))))))
          (setf (auto-sub-active-count else-Auto-Sub-Markers) 0))))))

(defun else-create-potential-completion-list (&optional all)
  "List of all placeholder names in the language set that are NONTERMINAL.

If optional argument ALL is nil, then terminal placeholders are filtered.
"
  (let ((names (get-names else-Current-Language))
        (result nil))
    (if all
        names
      (progn
        (dolist (name names)
          (unless (else-terminal-placeholder-p (lookup else-Current-Language name t))
           (push name result))))
      result)))

(defun else-deactivate-mode ()
  "Turn off ELSE Mode."
  (setf (auto-sub-active-count else-Auto-Sub-Markers) 0)
  (remove-hook 'before-change-functions 'else-before-change t)
  (remove-hook 'after-change-functions 'else-after-change t))

(defun else-delete-placeholder ()
  "Delete the placeholder at `point'.
Clean up syntactically."
  (let ((separator nil)
        (separator-region-end nil)
        (had-left-space nil)
        (had-right-space nil)
        (here nil)
        (search-limit nil)
        (string-index nil)
        (new-separator-search nil)
        (position nil)
        (anchor nil)
        (characters-skipped 0)
        (entity-details nil))
    (else-preserve-hook-excursion
     (setq entity-details (else-in-placeholder))
     (when entity-details
       (delete-region (p-struct-start entity-details) (p-struct-end
                                                       entity-details)))
     (when (p-struct-please-duplicate entity-details)
       (delete-char 3))

     ;; Check what "spacing" surrounded the deleted placeholder.
     (setq had-left-space (char-equal (preceding-char) ?\ ))
     (setq had-right-space (char-equal (following-char) ?\ ))

     ;; If the placeholder specified a "separator" then locate and delete the
     ;; separator
     (setq separator (oref (p-struct-definition entity-details) :separator))
     (when (> (length separator) 0)
       (setq separator-region-end (point))
       ;; Set a limit as the beginning of the previous line, this is a
       ;; worst case situation.
       (forward-line -1)
       (setq search-limit (point))
       (goto-char separator-region-end)

       (if (looking-back (regexp-quote separator) search-limit t)
           (delete-char (* -1 (length separator)))
         ;; separator strings was not found. Any trailing space may have been
         ;; removed i.e. (delete-trailing-space) may have been run on the buffer
         ;; as part of a hook command, even further spaces may have been added
         ;; (somehow). Extract the separator text and create a regex that
         ;; includes 1 or more trailing spaces - but ONLY if the preceeding
         ;; character is a space!
         (if (and (> (current-column) 0) ; not at the beginning of the line
                  (char-equal (preceding-char) ?\ ))
             (progn
               (setq separator (car (split-string separator " ")))
               (setq separator (concat separator " +"))
               (setq anchor (point))
               ;; Now try the search.
               (when (re-search-backward separator search-limit t)
                 ;; found
                 (delete-region (point) anchor))))))

     (setq position (point))

     ;; If this leaves the line blank, then delete the entire line
     (beginning-of-line)
     (setq characters-skipped (skip-chars-forward " \t" (line-end-position)))
     ;; mutliple possibilities:
     ;; 1. the line is blank up to (previous) point,
     ;; 2. line is entirely blank; or
     ;; 3. some number of characters before (previous) point are spaces that
     ;; need to be deleted.
     (cond ((eq (point) (line-end-position))     ; case 2
            (beginning-of-line)
            (kill-line))
           ((eq (point) position)         ; case 1
            (delete-char (* -1 characters-skipped))
            (unless (eq (point-min) (point))
              (delete-char -1))
            ;; go back to end of previous line?
            )
           (t                         ; case 3
            ;; line is not blank - perform some clean-up
            (goto-char position)
            ;; If there was a space before and after the placeholder then
            ;; "clean-up" by deleting one more space under
            ;; point. Groan.... only if there is more than one space!
            (when (and had-right-space had-left-space
                       (char-equal (preceding-char) ?\ )
                       (char-equal (following-char) ?\ ))
              (delete-char 1))))
     ;; Finally, if it is a "punctuation" character of the language then
     ;; make sure there is no preceding space. But if it's not at the start
     ;; of a line
     (when (aref (oref else-Current-Language :punctuation-characters)
                 (following-char))
       (while (char-equal (preceding-char) ?\ )
         (delete-char -1))))))

(defun else-delete-placeholder-text-and-duplicate (entity-details)
  "Delete the placeholder and duplicate if necessary."
  (let ((this-pos nil)
        (dup-direction nil))
    (if (p-struct-please-duplicate entity-details)
        (delete-region (p-struct-start entity-details)
                       (+ (p-struct-end entity-details) 3))
      (delete-region (p-struct-start entity-details) (p-struct-end entity-details)))

    ;; if the placeholder is/was trailed by a duplication request
    ;; indicator (...) then replicate it
    (when (p-struct-please-duplicate entity-details)
      (save-excursion
        (setq this-pos (point))
        ;; Work out the duplication requirements only if the
        ;; placeholder definition is context_dependent i.e. it is
        ;; not being overridden explicitly in the definition.
        (setq dup-direction (oref (p-struct-definition entity-details)
                                  :duplication))

        (when (eq dup-direction 'context-dependent)
          (if (not (re-search-backward "[^ \t]" (line-beginning-position) t))
              (setq dup-direction 'vertical)
            (setq dup-direction 'horizontal)))

        (goto-char this-pos)
        (else-replicate-placeholder-string dup-direction
                                           (- this-pos (line-beginning-position))
                                           entity-details)))))

(defun else-default-display-menu (placeholders descriptions)
  "This is the 'default' menu selector used by ELSE. It uses the
  popup package. the user can replace this function using
  else-alternate-menu-picker in the customisation variables."
  (let ((index 0)
        (menu-list nil)
        (value nil)
        (desc-filled '())
        (place-filled '())
        (selected nil)
        (menu-height-limit 15)  ; this magic number is used by popup-menu*
        (actual-menu-height 0)
        (room-above nil)
        (room-below nil)
        (cur-row (cdr (posn-col-row (posn-at-point))))) ; row # in window

    (defun equalize-strings (the-list)
      "Create a new list where each element is left-justified and blank filled
      so that all the list elements are the same length i.e. the longest"
      (let ((returned-list '())
            (length-of-longest 0))
        ; Find the length of the longest string in the list
        (setq length-of-longest (cl-loop for el in (mapcar 'length the-list) maximize el))

        ; only continue if the entire list is not nil
        (if (> length-of-longest 0)
            ; left-justify and blank fill each menu item
            (setq returned-list
                  (mapcar (lambda (x)
                            (concat x (make-string (- length-of-longest (length x)) ? )))
                          the-list)))
        returned-list))

    ;; popup right justifies the DESCRIPTION text, which is annoying, to force
    ;; it to left justify, make each DESCRIPTION the same length by filling
    ;; spaces at the end of the short(er) strings
    (setq desc-filled (equalize-strings descriptions))
    ; then, to keep the placeholder and descriptive text in neat columns, do the
    ; same to the placeholder list
    (setq place-filled (equalize-strings placeholders))

    ; create each menu entry for popup to display
    (dotimes (index (length place-filled))
      (setq value (nth index place-filled))
      (push (popup-make-item value
                             :summary (nth index desc-filled))
            menu-list))
    ; Keep the menu in the same order as the original template definition
    (setq menu-list (reverse menu-list))

    ; popup may either display the menu above or below point, it depends on the
    ; menu length and the current line number in thw window. popup-menu* has a
    ; default height of 15 (hard-coded). Determine if (a) there is sufficient
    ; room above/below the current line to display all 15 menu lines - if not
    ; reduce the height to popup-menu* and (b) if a scroll-bar should be display
    ; i.e. if the menu length/height is greater than the selected display
    ; height.
    (setq actual-menu-height (min menu-height-limit (length menu-list)))
    (setq room-above (< actual-menu-height cur-row))
    (setq room-below (< actual-menu-height (- (window-text-height)
                                              actual-menu-height)))
    ; if there is not enough room above OR below then adjust the max height to
    ; fit (either above or below)
    (if (not (or room-above room-below))
        (setq actual-menu-height (- (max cur-row (- (window-text-height) cur-row))
                                    2)))
    (setq value (popup-menu* menu-list
                             :scroll-bar (< actual-menu-height (length menu-list))
                             :height actual-menu-height
                             :max-width 0.8
                             :keymap else-menu-mode-map))
    ; the 'value' returned is the placeholder text that was left-justified and
    ; filled, so search for the original placeholder and return it to the
    ; caller.
    (dolist (placeholder placeholders)
      (if (string= placeholder (substring value 0 (length placeholder)))
          (setq selected placeholder)))
    selected))

(defun else-display-menu (possible-matches &optional momentary-only)
  "Display a list of choices to the user, 'possible-matches is a
  list of menu-item's. This function creates two lists to be
  passed to the menu picker function. The first list is the
  actuall placeholder strings and the second list is the
  descriptions (if they were defined) for each placeholder. So
  the called menu picker should iceally display each line
  as '<placeholder> <description>'."
  (let ((placeholders nil)
        (descriptions nil)
        (selection nil))
     (if momentary-only
         (popup-tip possible-matches)
       (dolist (item possible-matches)
         (push (menu-item-text    item) placeholders)
         (push (menu-item-summary item) descriptions))
       (setq placeholders (reverse placeholders)
             descriptions (reverse descriptions))
       (setq selection (funcall
                        (intern-soft else-alternate-menu-picker)
                        placeholders descriptions)))
     selection))

(defun else-expand ()
  "Expand the placeholder or any preceeding abbreviation at point."
  (interactive)
  (let ((dup-direction nil)
        (deleted-column nil)
        (insert-position nil)
        (entity-details nil)
        (else-runtime-error-msg nil)
        (ph-marker nil))
    (else-run-when-active
     (setq else-runtime-error-msg
           (catch 'else-runtime-error
             (setq entity-details (or (else-in-placeholder)
                                      (else-expand-abbreviation)))
             (when entity-details
               (setq insert-position (1- (p-struct-start entity-details)))

               (setq ph-marker (else-create-placeholder-marker (p-struct-start entity-details) (p-struct-end entity-details)))

               (execute-before (p-struct-definition entity-details) (placeholder-marker-m1 ph-marker))

               (expand
                   (p-struct-definition entity-details)
                   (p-struct-column-start-position entity-details)
                   (placeholder-marker-m1 ph-marker)
                   (placeholder-marker-m2 ph-marker))

               (execute-after (p-struct-definition entity-details) (placeholder-marker-m2 ph-marker))

               (goto-char insert-position)

               (unless (else-next 1 :leave-window nil)
                 (goto-char (marker-position (placeholder-marker-m2 ph-marker)))))

             nil))
     (when else-runtime-error-msg
       (message else-runtime-error-msg)))))

(defun else-expand-abbreviation ()
  "Expand the abbreviated text at point."
  (let ((entity-details nil)
        (all-placeholder-names nil)
        (abbreviated-string nil)
        (matched-placeholder nil)
        (expansion-candidates nil)
        (selected-index nil)
        (menu-list nil))
    ;; get all of the placeholder names, but filter out any placeholders that
    ;; are TERMINAL i.e. it is pointless to expand a TERMINAL placeholder (at
    ;; least as an abbreviation)
    (setq all-placeholder-names (else-create-potential-completion-list))
    (setq abbreviated-string (else-extract-abbreviation-at-point))
    (let ((completion-ignore-case t))
      (setq expansion-candidates (sort
                                  (all-completions abbreviated-string all-placeholder-names)
                                  'string-lessp)))
    (when expansion-candidates
      ;; check for an exact match i.e. if the abbreviation is "def" and the
      ;; possible matches are "def" and "defparameter" then select "def",
      ;; otherwise put up the selection menu.
      (if (= (length expansion-candidates) 1)
          (setq matched-placeholder (car expansion-candidates))
        (dolist (name expansion-candidates)
          (setq menu-list (append menu-list (list (make-menu-item :text name
                                                                  :summary (oref (lookup else-Current-Language name t) :description))))))
        (setq matched-placeholder (else-display-menu menu-list)))
      (when matched-placeholder
        (delete-char (* -1 (length abbreviated-string)))
        (insert (concat "[" matched-placeholder "]"))
        (setq entity-details (else-previous))))
    entity-details))

(defun else-extract-abbreviation-at-point ()
  "Extract the abbreviated text at point.
Uses else syntax table."
  (let ((completion-ignore-case t)
        (initial-string nil)
        (here (point)))
    (with-syntax-table else-expand-table
      (backward-word 1)
      (setq initial-string (buffer-substring-no-properties (point) here))
      (goto-char here))
    initial-string))

(defun else-in-placeholder ()
  "Test if point is situated within a (valid) placeholder returning details.
Point may be several levels of placeholder deep i.e. [as {name}]
  - the desired placeholder is 'as {name}', no matter where point
  is within the brackets."
  (let ((origin (point))
        (end-line-limit (line-end-position))
        (test-text nil)
        (candidates nil)
        (start nil)
        (start-list nil)
        (placeholder-details nil)
        (is-in-placeholder nil))
    (save-match-data
      (save-excursion
        ;; find all the possible candidates for a match
        (beginning-of-line)
        (catch 'create-candidate-loop
          (while t
            (skip-chars-forward "^]}[{" end-line-limit)
            (cond ((or (char-equal (following-char) ?\[)
                       (char-equal (following-char) ?\{))
                   (setq start (point))
                   (setq start-list (push start start-list)))
                  ((or (char-equal (following-char) ?\])
                       (char-equal (following-char) ?\}))
                   (when (> (length start-list) 0)
                     (setq start (pop start-list))
                     (when (and (> origin start)
                                (< origin (point)))
                       (setq candidates (push (list (1+ start) (point)) candidates)))))
                  (t
                   (throw 'create-candidate-loop nil)))
            (forward-char)))
        ;; the 'outer-most' pairs are the first encountered - stop at the first
        ;; 'match'
        (catch 'loop-over-candidates
          (dolist (pair candidates)
            (setq test-text (buffer-substring (car pair) (cadr pair)))
            (setq is-in-placeholder (lookup else-Current-Language test-text))
            (when is-in-placeholder
              (setq placeholder-details (make-p-struct :name test-text
                                                       :definition is-in-placeholder
                                                       :start (1- (car pair))
                                                       :end (1+ (cadr pair))
                                                       :column-start-position (- (1- (car pair)) (line-beginning-position))
                                                       :mandatory (char-equal (char-before (car pair)) ?\{)
                                                       :please-duplicate (progn
                                                                           (goto-char (1+ (cadr pair)))
                                                                           (looking-at (regexp-quote "...")))))
              (throw 'loop-over-candidates nil))))))
    placeholder-details))

(defun else-initialise-auto-subst-markers (entity-details)
  "Init 'else-Auto-Sub-Marker' using contents of 'entity-details."
  (let ((auto-sub-search-string nil)
        (sub-counter 0)
        (case-fold-search t)
        (this-pair nil)
        (dummy-pair nil)
        (search-limit (point-max)))
    (when (eq (oref (p-struct-definition entity-details) :substitution) 'auto-substitute)
      (save-excursion
        (setq sub-counter (oref (p-struct-definition entity-details) :substitution-count))
        (setf (auto-sub-active-count else-Auto-Sub-Markers) sub-counter)
        (setq auto-sub-search-string (concat "[[{]" (p-struct-name entity-details) "[]}]"))
        ;; first paired entry is the origin set
        (setf (auto-sub-origin-markers else-Auto-Sub-Markers)
              (make-auto-sub-pair :start (copy-marker (p-struct-start entity-details))
                                  :end (copy-marker (1+ (p-struct-end entity-details))))
              (auto-sub-list-of-markers else-Auto-Sub-Markers) (list (make-auto-sub-pair)))
        (dotimes (count (1- sub-counter))
          (setq dummy-pair (list (make-auto-sub-pair)))
          (setf (auto-sub-list-of-markers else-Auto-Sub-Markers)
                (append (auto-sub-list-of-markers else-Auto-Sub-Markers) dummy-pair)))
        (goto-char (p-struct-end entity-details))
        (dotimes (count sub-counter)
          (when (re-search-forward auto-sub-search-string search-limit t)
            (setq this-pair (nth count (auto-sub-list-of-markers
                                        else-Auto-Sub-Markers)))
            (setf (auto-sub-pair-start this-pair)
                  (copy-marker (match-beginning 0))
                  (auto-sub-pair-end this-pair)
                  (copy-marker (1+ (match-end 0))))
            (delete-region (match-beginning 0) (match-end 0))))))
    (> sub-counter 0)))

(defun else-kill (&optional force)
  "Kill the placeholder at point."
  (interactive "P")
  (let ((here (point))
        (err-msg nil)
        (entity-details nil))
    (else-run-when-active
     (setq entity-details (else-in-placeholder))
     (when entity-details
       (when (and (p-struct-mandatory entity-details)
                  (not force))
         (error "Can't delete, mandatory entry required.  Precede command with ^u to force deletion"))
       (else-delete-placeholder)
       ;; Check to see whether we should auto-position to the next
       ;; placeholder or not.
       (when (and else-kill-proceed-to-next-placeholder
                  (called-interactively-p 'any))
         (setq here (point))
         (else-next 1 :no-error-msg t)
         ;; only move point if not restricted to remaining in the current window
         (unless (and else-only-proceed-within-window
                      (pos-visible-in-window-p))
           (goto-char here)))))))

(defun else-insert-placeholder (&optional arg)
  "List all valid placeholders and insert or expand selected one into buffer at point.

If this function is called without a prefix-argument the selected placeholder is immediaately expanded.

If it is called with prefix-argument C-u, the selected placeholder is inserted as '[placeholder]' at
buffer point and point is moved to the end of the new placeholder.

If it is called with prefix-argument C-u C-u, then '{placeholder}' is inserted into the buffer instead."
  (interactive "p")
  (else-run-when-active
   (let ((all-placeholder-names nil)
         (matched-placeholder nil)
         (candidates nil)
         (menu-list nil)
         (without-terminals nil))

     (cl-case arg
       (4  (setq without-terminals t))
       (16 (setq without-terminals t))
       )

     (setq all-placeholder-names (else-create-potential-completion-list without-terminals))

     (let ((completion-ignore-case t))
       (setq candidates (sort all-placeholder-names 'string-lessp))
       (when candidates
         (dolist (name candidates)
           (setq name (downcase name))
           (setq menu-list
                 (append menu-list (list
                                    (make-menu-item
                                     :text name
                                     :summary (oref
                                               (lookup
                                                else-Current-Language
                                                name
                                                t)
                                               :description))))))
         (setq matched-placeholder (else-display-menu menu-list)))
       (when matched-placeholder
         (cl-case arg
           (1  (insert (concat "[" matched-placeholder "]"))
               (else-previous)
               (else-expand))
           (4  (insert (concat "[" matched-placeholder "]")))
           (16 (insert (concat "{" matched-placeholder "}")))
         )))
     )))

(defun else-load-template (&optional language-template-name)
  "Load a template file into the Template library."
  (interactive "P")
  (let ((language-name language-template-name)
        (language-file-names nil))
    ;; if a file name has not been passed in then prompt the user for a name
    (if (not language-name)
        (setq language-name (read-string "Language name: ")))
    ;; if the language is not already loaded then load and compile it
    (if (not (access-language else-Language-Repository language-name))
        (progn
          (setq language-file-names (else-locate-language-file language-name))
          (if (> (length language-file-names) 0)
              (else-load-file-and-compile language-name language-file-names)
            ;; otherwise, raise an error, unable to locate template files
            (message "Unable to locate file names associated with requested language")))
      ;; otherwise, message that the language is already loaded
      (message (format "Language %s is already loaded" language-name)))))

(defun else-switch-templates (&optional template-name)
  "Switch templates being used by local buffer.  New template is
  either specified by 'template-name or provided by user selction
  from a list of currently loaded template."
  (interactive "P")
  (let ((language-name template-name)
        (languages-loaded (get-language-names else-Language-Repository))
        (menu-display nil)
        (language-name nil))
    ;; It doesn't make any sense to run this command unless ELSE is active for
    ;; the current buffer.
    (else-run-when-active
     (if (not language-name)
         (progn
           ;; Re-use the else-display-menu function so that the choice goes
           ;; through whatever menu/completion function the user has
           ;; configured. We need to turn the list of languages loaded into a
           ;; list of 'menu-items for compatability
           (dolist (language-name languages-loaded)
             (push (make-menu-item :text language-name :summary nil) menu-display))
           (setq menu-display (reverse menu-display))
           (setq language-name (else-display-menu menu-display))
           (if language-name
               (setq else-Current-Language (access-language else-Language-Repository language-name))))))))

;;;###autoload
(define-minor-mode else-mode
  "Toggle ELSE on/off.

Key bindings:
\\{else-mode-key-map}"
  :lighter " ELSE"
  :init-value nil
  :keymap else-mode-key-map
  :after-hook
  (if else-mode
      (else-activate-mode)
    (else-deactivate-mode)))

(cl-defun else-next (&optional n &key (no-error-msg nil) (leave-window t))
  "Move 'point' to the 'next' placeholder"
  (interactive "p")
  (let ((count (or n 1))
        (err-msg nil)
        (last-valid nil))
    (else-run-when-active
     (save-match-data
       (save-excursion
         ;; step outside of any current placeholder
         (when (else-in-placeholder)
           (goto-char (p-struct-end (else-in-placeholder))))
         (catch 'exit-loop-early
           (while (> count 0)
             (if (re-search-forward "[[{]" nil t)
                 (progn
                   (when (and (not leave-window)
                              (not (pos-visible-in-window-p)))
                     (throw 'exit-loop-early nil))
                   (setq last-valid (else-in-placeholder))
                   (when last-valid
                     (setq count (1- count))
                     (goto-char (p-struct-end last-valid))))
               (if last-valid
                   (setq err-msg "Stopped (short) at last placeholder")
                 (setq err-msg "There are no more placeholders"))
               (throw 'exit-loop-early t)))))
       (when last-valid
         (goto-char (+ (p-struct-start last-valid)
                       (/ (- (p-struct-end last-valid)
                             (p-struct-start last-valid)) 2))))
       (unless (and no-error-msg
                    (booleanp err-msg))
         (message err-msg))))
    last-valid))

(cl-defun else-previous (&optional n &key (d 1) (no-error-msg nil))
  "Move `point' to the (nth) previous placeholder."
  (interactive "p")
  (let ((count (or n 1))
        (err-msg nil)
        (last-valid nil)
        (early-exit nil))
    (else-run-when-active
     (save-match-data
       (save-excursion
         ;; if currently in a placeholder then skip out of it
         (when (else-in-placeholder)
           (goto-char (1- (p-struct-start (else-in-placeholder)))))
         (catch 'exit-loop-early
           (while (> count 0)
             (if (re-search-backward "[{[]" nil t)
                 (progn
                   (forward-char)
                   (if (else-in-placeholder)
                       (progn
                         (setq last-valid (else-in-placeholder))
                         (setq count (1- count))
                         (goto-char (p-struct-start last-valid)))
                     (backward-char)))
               (setq early-exit t)
               (if last-valid
                   (setq err-msg "Stopped (short) at earliest placeholder")
                 (setq err-msg "There are no more placeholders"))
               (throw 'exit-loop-early t))))))
     (when last-valid
       (goto-char (+ (p-struct-start last-valid)
                     (/ (- (p-struct-end last-valid)
                           (p-struct-start last-valid)) 2))))
     (when (and (not no-error-msg)
                (not (booleanp err-msg)))
       (message err-msg)))
    last-valid))

(defun else-replicate-placeholder-string (duplication-type indent-column entity-details)
  "Duplicate the placeholder."
  (let ((separator nil)
        (cur-column indent-column))
    (setq separator (oref (p-struct-definition entity-details) :separator))
    (when (> (length separator) 0)
      (insert separator))
    (when (eq duplication-type 'vertical)
      (newline)
      (indent-to cur-column))
    (insert (concat "[" (p-struct-name entity-details) "]..."))))

(defun else-setup-change-hooks ()
  "Setup the before and after change hooks."
  (add-hook 'before-change-functions 'else-before-change t t)
  (add-hook 'after-change-functions 'else-after-change nil t))

(defun else-show-placeholder-names ()
  "Display info on all Placeholders in the current language template set.
Sort them alphabetically and display in a temporary buffer."
  (interactive)
  (let ((placeholder-length 0)
        (filename-length 0)
        (line-number-length 0)
        (item nil))
    (else-run-when-active
     (with-output-to-temp-buffer "*Available Placeholders*"
       ;; Nice to have good formatting for the output - determine the longest
       ;; placeholder name and incorporate the length into the format
       ;; string. Start with a "default" length of the column header
       (setq placeholder-length (length "Placeholder Name")
             filename-length (length "File")
             line-number-length (length "Line #"))
       (dolist (item-name (get-names else-Current-Language))
         (setq placeholder-length (max (length item-name) placeholder-length))
         (setq item (lookup else-Current-Language item-name t))
         (setq filename-length (max (length (oref item :file-name)) filename-length))
         (setq line-number-length (max (length (format "%s" (oref item :definition-line-number))) line-number-length)))

       ;; Now insert them into the buffer at point? Attempt some nice
       ;; formatting at the same time
       (princ (format (concat "%" (number-to-string placeholder-length)
                              "s %s ******")
                      "****** Placeholders for"
                      (oref else-Current-Language :name)))
       (terpri)
       (terpri)
       (princ (concat "Placeholder Name" (make-string (+ 2 (- placeholder-length (length "Placeholder Name"))) ? )))
       (princ (concat "Line #" (make-string (+ 2 (- line-number-length (length "Line #"))) ? )))
       (princ (concat "File" (make-string (+ 2 (- filename-length (length "File"))) ? )))
       (princ "Description")
       (terpri)
       (dolist (item-name (get-names else-Current-Language))
         (setq item (lookup else-Current-Language item-name t))
         (princ (concat item-name (make-string (+ 2 (- placeholder-length (length item-name))) ? )))
         (princ (concat (format "%s" (oref item :definition-line-number)) (make-string (+ 2 (- line-number-length (length (format "%s" (oref item :definition-line-number))))) ? )))
         (princ (concat (oref item :file-name) (make-string (+ 2 (- filename-length (length (oref item :file-name)))) ? )))
         (princ (format "%s" (oref item :description)))
         (terpri))))))

(defgroup ELSE nil
  "Custom variables for Emacs Language Sensitive Editor"
  :tag "Emacs LSE"
  :prefix "else"
  :group 'tools)

(defcustom else-kill-proceed-to-next-placeholder t
  "Should ‘else-kill’ goto next placeholder after a kill(t) or not(nil)."
  :type 'boolean
  :group 'ELSE)

(defcustom else-only-proceed-within-window t
  "Move on kill only if the next placeholder is visible in the current window.
This flag controls jumps when they are part of a composite action by ELSE
i.e. in kill-placeholder, if the kill-proceed flags is set then this flag
allows the move to the next placeholder only if it is visible in the current
window."
  :type 'boolean
  :group 'ELSE)

(defcustom else-menu-linking-default t
  "Menu linking default is either FOLLOW(t) or NOFOLLOW(nil). The
attributes /FOLLOW and /NOFOLLOW can be used to specifically
override this default option."
  :type 'boolean
  :group 'ELSE)

(defcustom else-fast-load-directory user-emacs-directory
  "Directory where fast load files (.esl) are stored."
  :type 'string
  :group 'ELSE)

(defcustom else-alternate-menu-picker "else-default-display-menu"
  "Place the name of your desired menu display/completion function here.
Refer to else-display-menu for an example of processing and what arguments are
  expected."
  :type 'string
  :group 'ELSE)

(provide 'else-mode)

;;; else-mode.el ends here
