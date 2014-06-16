;; Copyright (C) 2014 Mattias Ugelvik
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun insert-lines (indentation lines)
  (insert "\n")
  (dolist (line lines)
    (indent-to indentation)
    (insert line)
    (insert "\n"))
  (delete-backward-char 1))

(defun find-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")))


(defun malang-insert-func (prefix)
  (interactive "P")
  (let ((indentation (find-indentation))
        (lines (if (null prefix)
                   ;; Insert docstring if there is a prefix arg
                   (list "  "    "].")
                   (list "  \""    "  "    "  \"."    "]."))))
    (end-of-line)
    (just-one-space)
    (save-excursion
      (insert ":= [")
      (insert-lines indentation lines)))
  (forward-line) (end-of-line)
  (unless (null prefix) (forward-line) (end-of-line)))

(defun malang-insert-case ()
  (interactive)
  (let ((indentation (find-indentation)))
    (end-of-line)
    (insert "case ")
    (save-excursion
      (insert-lines indentation (list "  -> ."    "end."))))
  (save-excursion (insert " of")))

(defun malang-insert-if ()
  (interactive)
  (let ((indentation (find-indentation)))
    (end-of-line)
    (insert "if ")
    (save-excursion
      (insert-lines indentation (list "  then ."    "  else ."    "end.")))))


(defvar malang-mode-hook nil)

(defvar malang-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c f") 'malang-insert-func)
    (define-key map (kbd "C-c c") 'malang-insert-case)
    (define-key map (kbd "C-c i") 'malang-insert-if)
    map)
  "Keymap for malang major mode")

(add-to-list 'auto-mode-alist '("\\.malang\\'" . malang-mode))

(defconst malang-font-lock-keywords
  (list
   (cons (format "\\<%s\\>" (regexp-opt '("case" "of" "if" "then" "else" "end" "catch"
                                          "throw" "classified" "exposing" "where" "endify")))
         'font-lock-keyword-face)
   (cons (regexp-opt '(":=" "*" "**" "/" "-" "+" "%" ":" "::" ">" "<" ">=" "<=" "=" "!=" "~" "$"))
         'font-lock-function-name-face)
   '("->\\|<-" . font-lock-keyword-face)
   '("\\<yeah\\|nope\\>" . font-lock-builtin-face)
   '("\\<[a-z][A-Za-z0-9_]*\\>" . font-lock-constant-face)
   '("\\<[A-Z_][A-Za-z0-9_?']*\\>" . font-lock-variable-name-face)
   '("@" . font-lock-variable-name-face))
  "highlighting expressions for malang mode.")

(defvar malang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\? "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?\- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for malang-mode")

(defun malang-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table malang-mode-syntax-table)
  (use-local-map malang-mode-map)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(malang-font-lock-keywords))
  (setq major-mode 'malang-mode)
  (setq mode-name "Malang")
  (run-hooks 'malang-mode-hook))

(provide 'malang-mode)
