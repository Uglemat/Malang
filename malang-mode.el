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



(defun insert-func ()
  (interactive)
  (beginning-of-line)
  (let ((indentation (skip-chars-forward " \t")))
    (end-of-line)
    (just-one-space)
    (insert ":= [\n")
    (indent-to (+ indentation 2))
    (save-excursion
      (insert "\n")
      (indent-to indentation)
      (insert "]\."))))

(defvar malang-mode-hook nil)

(defvar malang-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-f") 'insert-func)
    map)
  "Keymap for malang major mode")

(add-to-list 'auto-mode-alist '("\\.malang\\'" . malang-mode))

(defconst malang-font-lock-keywords
  (list
   '("\\(\\<\\(case\\|of\\|if\\|then\\|else\\|end\\|catch\\|throw\\)\\>\\|->\\|<-\\)" . font-lock-keyword-face)
   '("\\<\\([a-z][A-Za-z0-9_]*\\)\\>" . font-lock-constant-face)
   '("\\(\\<\\([A-Z_][A-Za-z0-9_]*\\)\\>\\|@\\)" . font-lock-variable-name-face))
  "highlighting expressions for malang mode.")

(defvar malang-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
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
