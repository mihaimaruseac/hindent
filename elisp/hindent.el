;;; hindent.el --- Indent haskell code using the "hindent" program

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/hindent
;; Package-Requires: ((cl-lib "0.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defcustom hindent-style
  "fundamental"
  "The style to use for formatting."
  :group 'haskell
  :type 'string
  :safe #'stringp)

(defcustom hindent-process-path
  "hindent"
  "Location where the hindent executable is located."
  :group 'haskell
  :type 'string
  :safe #'stringp)

;;;###autoload
(defun hindent/reformat-decl ()
  "Re-format the current declaration by parsing and pretty
  printing it. Comments are preserved, although placement may be
  funky."
  (interactive)
  (let ((start-end (hindent-decl-points)))
    (when start-end
      (let ((original (current-buffer))
            (orig-str (buffer-substring-no-properties (car start-end)
                                                      (cdr start-end))))
        (with-temp-buffer
          (let ((temp (current-buffer)))
            (with-current-buffer original
              (let ((ret (apply #'call-process-region
                                (append (list (car start-end)
                                              (cdr start-end)
                                              hindent-process-path
                                              nil ; delete
                                              temp ; output
                                              nil
                                              "--style"
                                              hindent-style)
                                        (hindent-extra-arguments)))))
                (cond
                 ((= ret 1)
                  (let ((error-string
			 (with-current-buffer temp
                           (let ((string (progn (goto-char (point-min))
                                                (buffer-substring (line-beginning-position)
                                                                  (line-end-position)))))
                             string))))
		    (if (string= error-string "hindent: Parse error: EOF")
			(message "language pragma")
		      (error error-string))))
                 ((= ret 0)
                  (let ((new-str (with-current-buffer temp
                                   (buffer-string))))
                    (if (not (string= new-str orig-str))
                        (let ((line (line-number-at-pos))
                              (col (current-column)))
                          (delete-region (car start-end)
                                         (cdr start-end))
                          (let ((new-start (point)))
                            (insert new-str)
                            (let ((new-end (point)))
                              (goto-char (point-min))
                              (forward-line (1- line))
                              (goto-char (+ (line-beginning-position) col))
                              (when (looking-back "^[ ]+")
                                (back-to-indentation))
                              (delete-trailing-whitespace new-start new-end)))
                          (message "Formatted."))
                      (message "Already formatted.")))))))))))))

(defun hindent-extra-arguments ()
  "Pass in extra arguments, such as extensions and optionally
other things later."
  (if (boundp 'haskell-language-extensions)
      haskell-language-extensions
    '()))

(defun hindent-reformat-decl-or-fill (justify)
  "Re-format current declaration, or fill paragraph.

Fill paragraph if in a comment, otherwise reformat the current
declaration."
  (interactive (progn
                 ;; Copied from `fill-paragraph'
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'full))))
  (if (hindent-in-comment)
      (fill-paragraph justify t)
    (hindent/reformat-decl)))

(defun hindent-decl-points (&optional use-line-comments)
  "Get the start and end position of the current
declaration. This assumes that declarations start at column zero
and that the rest is always indented by one space afterwards, so
Template Haskell uses with it all being at column zero are not
expected to work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (not use-line-comments)
         (hindent-in-comment)
         (save-excursion (goto-char (line-beginning-position))
                         (hindent-in-comment)))
    nil)
   ((save-excursion
      (goto-char (line-beginning-position))
      (or (looking-at "^-}$")
          (looking-at "^{-$")))
    nil)
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start (or (flet
                           ((jump ()
                                  (search-backward-regexp "^[^ \n]" nil t 1)
                                  (cond
                                   ((save-excursion (goto-char (line-beginning-position))
                                                    (looking-at "|]"))
                                    (jump))
                                   (t (unless (or (looking-at "^-}$")
                                                  (looking-at "^{-$"))
                                        (point))))))
                         (goto-char (line-end-position))
                         (jump))
                       0))
            (end (progn (goto-char (1+ (point)))
                        (or (flet
                                ((jump ()
                                       (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                                         (cond
                                          ((save-excursion (goto-char (line-beginning-position))
                                                           (looking-at "|]"))
                                           (jump))
                                          (t (forward-char -1)
                                             (search-backward-regexp "[^\n ]" nil t)
                                             (forward-char)
                                             (point))))))
                              (jump))
                            (point-max)))))
        (cons start end))))))

(defun hindent-in-comment ()
  "Are we currently in a comment?"
  (save-excursion
    (when (and (= (line-end-position)
                  (point))
               (/= (line-beginning-position) (point)))
      (forward-char -1))
    (and (or (eq 'font-lock-comment-delimiter-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-doc-face
                 (get-text-property (point) 'face))
             (eq 'font-lock-comment-face
                 (get-text-property (point) 'face))
             (save-excursion (goto-char (line-beginning-position))
                             (looking-at "^\-\- ")))
         ;; Pragmas {-# SPECIALIZE .. #-} etc are not to be treated as
         ;; comments, even though they are highlighted as such
         (not (save-excursion (goto-char (line-beginning-position))
                              (looking-at "{-# "))))))

(defun hindent-reformat-region ()
  (interactive)
  (save-excursion
    (save-restriction
      (if (> (point) (mark))
	  (exchange-point-and-mark))
      (while (< (point) (mark))
	(hindent/reformat-decl)
	(let ((dpoints (hindent-decl-points)))
	  (if dpoints ;; if we're in a comment hindent-decl-points returns nil
	      (goto-char (min (mark) (+ 1 (cdr dpoints))))
	    (forward-line 1)))
	;; might be on a blank line (which associates with the previous decl
	(if (search-forward-regexp "^[\\s-]*[^\\]" (mark) t)
	    nil
	  (goto-char (mark)))))))

(defun hindent-reformat-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (mark-whole-buffer)
      (hindent-reformat-region))))

(defvar hindent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap indent-region] #'hindent-reformat-region)
    (define-key map [remap fill-paragraph] #'hindent-reformat-decl-or-fill)
    map)
  "Keymap for `hindent-mode'.")

;;;###autoload
(define-minor-mode hindent-mode
  "Indent code with the hindent program.

Provide the following keybindings:

\\{hindent-mode-map}"
  :init-value nil
  :keymap hindent-mode-map
  :lighter " HI"
  :group 'haskell
  :require 'hindent)

(provide 'hindent)

;;; hindent.el ends here
