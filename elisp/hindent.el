;;; hindent.el --- Haskell indenter.

;; Copyright (c) 2014 Chris Done. All rights reserved.

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

(defcustom hindent-style
  "fundamental"
  "The style to use for formatting."
  :group 'haskell
  :type 'string)

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
              (let ((ret (call-process-region (car start-end)
                                              (cdr start-end)
                                              "hindent"
                                              nil  ; delete
                                              temp ; output
                                              nil
                                              "--style"
                                              hindent-style)))
                (cond
                 ((= ret 1)
                  (error (with-current-buffer temp
                           (let ((string (progn (goto-char (point-min))
                                                (buffer-substring (line-beginning-position)
                                                                  (line-end-position)))))
                             string))))
                 ((= ret 0)
                  (let ((new-str (with-current-buffer temp
                                   (buffer-string))))
                    (if (not (string= new-str orig-str))
                        (let ((line (line-number-at-pos))
                              (col (current-column)))
                          (delete-region (car start-end)
                                         (cdr start-end))
                          (insert new-str)
                          (goto-char (point-min))
                          (forward-line (1- line))
                          (goto-char (+ (line-beginning-position) col))
                          (when (looking-back "^[ ]+")
                            (back-to-indentation))
                          (message "Formatted."))
                      (message "Already formatted.")))))))))))))

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
   ((bound-and-true-p structured-haskell-repl-mode)
    (case major-mode
      (haskell-interactive-mode
       ;; If the prompt start is available.
       (when (boundp 'haskell-interactive-mode-prompt-start)
         ;; Unless we're running code.
         (unless (> (point)
                    (save-excursion (goto-char haskell-interactive-mode-prompt-start)
                                    (line-end-position)))
           ;; When we're within the prompt and not on some output lines or whatever.
           (when (and (>= (point) haskell-interactive-mode-prompt-start)
                      (not (= haskell-interactive-mode-prompt-start
                              (line-end-position))))
             (let ((whole-line (buffer-substring-no-properties
                                haskell-interactive-mode-prompt-start
                                (line-end-position))))
               ;; Don't activate if we're doing a GHCi command.
               (unless (string-match "^:" whole-line)
                 (cons haskell-interactive-mode-prompt-start
                       (line-end-position)))))))
       )))
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start (or (progn (goto-char (line-end-position))
                              (search-backward-regexp "^[^ \n]" nil t 1)
                              (unless (or (looking-at "^-}$")
                                          (looking-at "^{-$"))
                                (point)))
                       0))
            (end (progn (goto-char (1+ (point)))
                        (or (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                              (forward-char -1)
                              (search-backward-regexp "[^\n ]" nil t)
                              (forward-char)
                              (point))
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

(provide 'hindent)
